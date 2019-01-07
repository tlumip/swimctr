#' Transform FAF flow database distributed by FHWA into format for target year
#'
#' @param fhwa_db Data frame containing the FAF interregional flow database in
#'   format distributed by FHWA, or string containing path and filename
#'   containing the same
#' @param target_year The four-digit simulation year as an integer value
#' @param interpolate A boolean variable denoting whether values for the
#'   `target_year` should be interpolated between the two closest years if it is
#'   not one of the years included in the FAF database (defaults to FALSE,
#'   currently ignored)
#' @param internal_regions A set of one or more FAF regions internal to the
#'   study area (optional, but must be specified if `outer_regions` is defined)
#' @param outer_regions A data frame containing FAF region pairs whose flows
#'   pass through the modeled area (optional, but if specified then
#'   `internal_regions` must also be specified)
#'
#' @details This function converts the FHWA Freight Analysis Framework (FAF)
#'   commodity flows, measured in annual tons, dollars, and ton-miles
#'   between domestic FAF regions and external regions into annual flows
#'   for the specified year or closest to it in the FAF database. The `fhwa_db`
#'   can either be a data frame previously created with the contents of the
#'   FAF flow database or a fully-qualified pathname to the data in comma-
#'   separated value (CSV) format. The latter can be in a compressed format that
#'   readr::read_csv() can handle.
#'
#'   It is important to note that the tons, value, and ton-miles are converted
#'   from implied thousands and millions of units in the source data. Zero
#'   values are retained, as they reportedly represent flows whose values are
#'   less than 1 unit (e.g., zero tons represent values between zero and one
#'   thousand tons since in the FAF tons are coded in implied thousands of
#'   tons).
#'
#'   The `target_year` must be one of the years in the FAF database, or fall
#'   between two of the years included within it. Only data for the target or
#'   closest year is processed by this function. An optional `interpolate`
#'   parameter will eventually allow for the tons, value, and ton-miles to be
#'   interpolated between the two closest years to the target year. The
#'   parameter is included so as to not break the interface, but the
#'   functionality is not currently implemented.
#'
#'   The `internal_regions` should be specified as a list of the FAF regions
#'   inside the modeled area. Flows to, from, and between these internal
#'   regions will be allocated to synthetic firms or traffic analysis zones
#'   within the model. Flows from the remainder of the USA will be retained if
#'   they have a trip end in one of the internal regions (e.g., internal,
#'   inbound, or outbound flows). The internal regions are optional, but if
#'   omitted data from the entire USA will be considered internal to the model.
#'
#'   Flows likely to pass through one or more internal regions can be included
#'   as `outer_regions`. Such flows must be defined as FAF region pairs (e.g.,
#'   truck flows between California and Washington on I-5 if Oregon comprises
#'   the internal regions). The flows are assumed to be bidirectional to reduce
#'   the amount of coding required. Note that any additional fields in
#'   `outer_regions` will be added to the FAF flows records. The user should
#'   remove any unwanted fields before or after passing the `outer_region`
#'   definitions to this function.
#'
#' @export
#' @examples
#' annual_flows <- preprocess_faf4_database(fhwa_database, 2018, FALSE,
#'   c(411, 419, 532))  # Several internal regions and no outer regions defined
#' annual_flows <- preprocess_faf4_database("./faf4.4.1.zip", 2016, FALSE,
#'   160, idaho_outer_regions)   # A single internal and multiple outer regions


preprocess_faf4_database <- function(fhwa_db, target_year, interpolate = FALSE,
  internal_regions = NULL, outer_regions = NULL) {
  # Determine whether fhwa_db is a data frame or filename, and complain if not
  contents <- as.character(class(fhwa_db)[1])
  if (contents %in% c("tbl_df", "data.frame", "data.table")) {
    # Assume that the fhwa_db points to a valid data frame
    print(paste("Processing FAF flow data from from contents in",
      deparse(substitute(fhwa_db))), quote = FALSE)
  } else if (contents == "character") {
    # The contents can be a valid filename, but check to make sure
    if (!file.exists(fhwa_db)) {
      stop(paste0("The fhwa_db parameter ", fhwa_db,
        " appears to be a string but does not specify a valid filename"))
    } else {
      print(paste("Build FAF flows data frame from", fhwa_db), quote = FALSE)
      fhwa_db <- readr::read_csv(fhwa_db, guess_max = Inf)
    }
  } else {
    # It isn't a data frame or string that resolves to valid filename
    stop(paste0("The fhwa_db parameter ", fhwa_db, " is invalid"))
  }

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
  fhwa_db$year <- target_year   # Reset to the user-specified year afterwards

  # Now that we have tonnage, value, and ton-miles for the target year drop the
  # original fields from the data
  for (this_year in years_found) {
    fhwa_db[[paste0("tons_", this_year)]] <- NULL
    fhwa_db[[paste0("value_", this_year)]] <- NULL
    fhwa_db[[paste0("tmiles_", this_year)]] <- NULL
  }

  # Before we do anything else we need to recast several variables as numeric if
  # they were incorrectly read as character variables
  recast <- dplyr::mutate(fhwa_db, fr_orig = as.integer(fr_orig),
    dms_orig = as.integer(dms_orig), fr_dest = as.integer(fr_dest),
    dms_dest = as.integer(dms_dest), sctg2 = as.integer(sctg2),
    dms_mode = as.integer(dms_mode), trade_type = as.integer(trade_type))

  # Next tag the regions that will be included within the modeled area. This
  # list is optional, as some users might want all domestic origins and desti-
  # nations, but otherwise set the direction code for those that are included.
  recast$direction <- "drop"   # Set the default value for this variable
  if (is.null(internal_regions)) {
    # Everything is internal if the user hasn't specified which regions are
    recast$direction <- "internal"

    # But they cannot specify outer regions in that case, as they don't make
    # sense within this context
    if (!is.null(outer_regions)) stop(paste("No internal regions were",
      "specified, but outer regions were"))
  } else {
    # Define the direction codes for those regions specified as internal
    recast$direction <- ifelse(recast$dms_orig %in% internal_regions &
        recast$dms_dest %in% internal_regions, "internal", recast$direction)
    recast$direction <- ifelse(recast$dms_orig %in% internal_regions &
        !recast$dms_dest %in% internal_regions, "outbound", recast$direction)
    recast$direction <- ifelse(!recast$dms_orig %in% internal_regions &
        recast$dms_dest %in% internal_regions, "inbound", recast$direction)

    # Then process the outer regions if they are defined. Note that the user can
    # define internal regions but leave outer regions undefined. We will do this
    # by merging the outer region data with the data frame. We will tag those
    # regions as through movements.
    if (!is.null(outer_regions)) {
      # The outer region flows are usually coded as one way, but of course the
      # flows move in opposite direction as well. Thus, we'll first need to add
      # the other direction to the flows.
      opposite_direction <- outer_regions %>%
        dplyr::mutate(temp = dms_orig, dms_orig = dms_dest, dms_dest = temp) %>%
        dplyr::mutate(temp = entry, entry = exit, exit = temp, temp = NULL)
      outer_regions <- dplyr::bind_rows(outer_regions, opposite_direction)

      # Now we can merge the flow data with these outer region definitions,
      # which will allow us to carry forward any data included in the latter
      recast <- dplyr::left_join(recast, outer_regions, by = c("dms_orig",
          "dms_dest"))

      # And finally, tag the outer regions as through flows. We will process the
      # list of all through flows as we cannot tag based upon unique or non-
      # missing contents outer_regions because we don't know for sure what
      # fields, if any, will be included other than domestic O and D.
      n_outer_regions <- nrow(outer_regions)
      for (i in (1:n_outer_regions)) {
        recast$direction <- ifelse(recast$dms_orig == outer_regions$dms_orig[i] &
            recast$dms_dest == outer_regions$dms_dest[i], "through",
          recast$direction)
      }
    }
  }

  # The trade type is coded as numeric variable, but it will make downstream use
  # less error-prone to recast as descriptive string. In this case we only need
  # to differentiate between domestic and cross-border flows.
  redo_trade_type <- dplyr::mutate(recast, trade_type = ifelse(trade_type == 1,
    "domestic", ifelse(trade_type %in% c(2, 3), "x-border", NA)))

  # We should also recode the mode of transport while we're at it
  redo_transport_mode <- dplyr::mutate(redo_trade_type,
    domestic_mode = ifelse(dms_mode == 1, "Truck", ifelse(dms_mode == 2, "Rail",
      ifelse(dms_mode == 3, "Water", ifelse(dms_mode == 4, "Air",
        ifelse(dms_mode == 5, "Multiple", ifelse(dms_mode == 6, "Pipeline",
          ifelse(dms_mode == 7, "Other", ifelse(dms_mode == 8, "None",
            NA)))))))))

  # Drop the records whose direction are not internal, inbound, outbound, or
  # through
  keep <- dplyr::filter(redo_transport_mode, direction != "drop")
  print(paste(nrow(keep), "records retained:"), quote = FALSE)
  print(table(keep$direction, useNA = "ifany"))

  # How many records have zero transactions?
  n_zeros <- nrow(dplyr::filter(keep, exp_value <= 0.0, exp_tons <= 0.0))
  pct_zeros <- swimctr::percent(n_zeros, nrow(keep))
  print(paste0(n_zeros, " of ", nrow(keep), " records (", pct_zeros,
    "%) have zero tons and value coded"), quote = FALSE)

  # Show us the total flows by mode and direction
  print("Annual tonnage by direction and mode for modeled area:", quote = FALSE)
  print(addmargins(xtabs(exp_tons ~ domestic_mode + direction, data = keep)))

  # Return the results
  return(keep)
}
