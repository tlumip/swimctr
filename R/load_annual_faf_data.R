#' Load preformatted and processed FAF data for the target year
#'
#' @param preprocessed_faf Data frame containing the FAF inter-regional flow
#'  database previous extracted for modeled area and in format required by CT
#'  modules
#' @param target_year The four-digit simulation year as an integer value
#' @param interpolate A boolean variable denoting whether values for the
#'   `target_year` should be interpolated between the two closest years if it is
#'   not one of the years included in the FAF database (defaults to FALSE,
#'   currently ignored)
#'
#' @details This function retrieves FAF regional flow data for a specific year
#'   from data already preprocessed from the original format distributed by
#'   FHWA. This conversion is usually done only once per FAF version and then
#'   stored as static inputs to the SWIM2 system. These preprocessed data are
#'   typically built using the `prebuild_faf_multiyear.rmd` program in the
#'   `data-raw` folder of the `swimctr` package. When used in a CT run within
#'   this module obviates the need to run the `swimctr::preprocess_faf_database`
#'   module, saving consider CT run time for each simulation year.
#'
#' @export
#' @examples
#' annual_flows <- load_annual_faf_data(prebuilt_database, 2018)


load_annual_faf_data <- function(preprocessed_faf, target_year,
  interpolate = FALSE) {
  # Start message
  noquote(swimctr:::self_identify(match.call()))
  crash_signal <<- FALSE

  # A preprocessed FAF database will have several fields not present in the
  # original data or in different formats. If these are not found then the user
  # has inadvertently passed a tibble that hasn't been reformatted for use in
  # CT yet. Complain and quit if that happens...
  preprocessed_faf <- swimctr:::load_tabular_data(preprocessed_faf)
  all_fields <- colnames(preprocessed_faf)
  if ("direction" %in% all_fields & "domestic_mode" %in% all_fields) {
    # We're working with a preprocessed file so safe to continue
    # Start by extracting a list of all the years in the preprocessed data
    target_year <- as.integer(target_year)  # In case it's read as string
    years_found <- sort(unique(preprocessed_faf$year))

    # Finding the closest year in the dataset should now be easy, as we can find
    # the year closest to our target year
    offsets <- abs(years_found - target_year)
    faf_year <- years_found[which.min(offsets)]
    if (faf_year == target_year) {
      noquote(paste("FAF data found for target year of", target_year))
    } else {
      noquote(paste("Using data from closest FAF year", faf_year, "to target year",
        target_year))
    }

    # Now just pull data for our closest FAF year and hand it to the calling
    # program after summarising the total flows by direction and mode
    this_year <- filter(preprocessed_faf, faf_year == year)
    noquote("Annual tonnage by direction and mode for modeled area:")
    noquote(addmargins(xtabs(exp_tons ~ domestic_mode + direction, data = this_year)))
    return(this_year)

  } else {
    # We didn't find our expected variables in the tibble passed to this
    # function, so fail
    crash_signal <<- TRUE
    error_msg <- paste("FAF database passed to load_annual_faf_data()",
      "not in required format")
    stop(error_msg)
  }
}
