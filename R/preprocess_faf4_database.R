#' Transform FAF flow database distributed by FHWA into format for target year
#'
#' @param fhwa_db Data frame containing the FAF interregional flow database in
#'   format distributed by FHWA
#' @param target_year The four-digit simulation year as an integer value. The
#'   value must be one of the years in the FAF database, or fall within the
#'   interval x years before first year to x years after the time series in the
#'   FAF data, where x is the foreast_range_threshold.
#'
#' @details This function converts the commodity flows, measured in annual tons
#'   and value between FAF regions, into flows for the specified year or closest
#'   to it in the FAF database.
#'
#' @export
#' @examples
#' annual_flows <- preprocess_faf4_database(fhwa_database, 2018)


preprocess_faf4_database <- function(fhwa_db, target_year) {
  # Extract the years that are included in the database. We will grab the years
  # coded in the tons fields in the file header. There is probably some super
  # efficient way of doing all of this in one line of obscure code, but few
  # enough fields to chop through to make the least efficient way fast enough.
  years_found <- c()
  for (this_field in names(fhwa_db)) {
    if (substr(this_field, 1 , 5) == "tons_") {
      this_year <- as.integer(substr(this_field, 6, 9))
      years_found <- c(years_found, this_year)
    }
  }
  
  # Finding the closest year in the dataset should now be easy, as we can find
  # the year closest to our target year
  offsets <- abs(years_found - target_year)
  faf_year <- years_found[which.min(offsets)]
  print(paste("FAF data from", faf_year, "is closest to target year",
    target_year), quote = FALSE)
  
  # Append the tonnage, value, and ton-miles to each record from the FAF year
  # closest to the target year and scale them on the fly
  fhwa_db$year <- faf_year
  fhwa_db$exp_tons <- fhwa_db[[paste0("tons_", faf_year)]] * 1e3
  fhwa_db$exp_value <- fhwa_db[[paste0("value_", faf_year)]] * 1e6
  fhwa_db$exp_tmiles <- fhwa_db[[paste0("tmiles_", faf_year)]] * 1e6
  
  # How many records have zero transactions?
  n_zeros <- nrow(dplyr::filter(fhwa_db, exp_value <= 0.0, exp_tons <= 0.0))
  pct_zeros <- percent(n_zeros, nrow(fhwa_db))
  print(paste0(n_zeros, " of ", nrow(fhwa_db), " records (", pct_zeros,
    "%) have zero tons and value coded"), quote = FALSE)
  
  # Finally, create a database with only the fields we need and the format we can
  # can use them
  redux <- fhwa_db %>%
    dplyr::mutate(fr_orig = as.integer(fr_orig), dms_orig =as.integer(dms_orig),
      fr_dest = as.integer(fr_dest), dms_dest = as.integer(dms_dest),
      sctg2 = as.integer(sctg2), dms_mode = as.integer(dms_mode),
      trade_type = as.integer(trade_type)) %>%
    dplyr::select(year, fr_orig, dms_orig, fr_dest, dms_dest, fr_inmode,
      dms_mode, fr_outmode, sctg2, trade_type, wgt_dist, exp_tons, exp_value,
      exp_tmiles)
  
  # Return the results
  return(redux)
}