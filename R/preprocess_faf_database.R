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
#'   study area (optional, but must be specified if `external_regions` is
#'   defined)
#' @param external_regions A data frame containing FAF region pairs whose flows
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
#'   as `external_regions`. Such flows must be defined as FAF region pairs (e.g.,
#'   truck flows between California and Washington on I-5 if Oregon comprises
#'   the internal regions). The flows are assumed to be bidirectional to reduce
#'   the amount of coding required. Note that any additional fields in
#'   `external_regions` will be added to the FAF flows records. The user should
#'   remove any unwanted fields before or after passing the `outer_region`
#'   definitions to this function.
#'
#' @export
#' @examples
#' annual_flows <- preprocess_faf_database(fhwa_database, 2018, FALSE,
#'   c(411, 419, 532))  # Several internal regions and no outer regions defined
#' annual_flows <- preprocess_faf_database("./faf4.4.1.zip", 2016, FALSE,
#'   160, oregon_outer_regions)   # A single internal and multiple outer regions


preprocess_faf_database <- function(fhwa_db, target_year, interpolate = FALSE,
  internal_regions = NULL, external_regions = NULL) {
  # Introduce yourself
  print(swimctr:::self_identify(match.call()), quote = FALSE)
  crash_signal <<- FALSE  # Have I found unrecoverable errors yet?

  # Determine whether fhwa_db is a data frame or filename, and complain if not
  fhwa_db <- swimctr:::load_tabular_data(fhwa_db)

  # Make sure that the data contains the weighted distance (`wgt_dist`) between
  # FAF regions, which is missing from the FAF 5.x data. If it is remind the
  # user to append those values first.
  if (!"wgt_dist" %in% colnames(fhwa_db)) {
    crash_signal <- TRUE
    error_msg <- paste("Weighted distances missing from fhwa_db data; please",
      "append using `append_faf45_distances` or user-defined function")
    stop(error_msg)
  }

  # Extract the years that are included in the database. We will grab the years
  # coded in the tons fields in the file header. There is probably some super
  # efficient way of doing all of this in one line of obscure code, but few
  # enough fields to chop through to make the least efficient way fast enough.
  yf <- grep("tons_", colnames(fhwa_db), value = TRUE)
  years_found <- as.integer(sub("tons_", "", yf))
  print(paste("Years in FAF dataset:", paste0(years_found, collapse = " ")),
    quote = FALSE)

  # Finding the closest year in the dataset should now be easy, as we can find
  # the year closest to our target year
  offsets <- abs(years_found - target_year)
  faf_year <- years_found[which.min(offsets)]
  if (faf_year == target_year) {
    print(paste("FAF data found for target year of", target_year), quote = FALSE)
  } else {
    print(paste("Using data from closest FAF year", faf_year, "to target year",
      target_year), quote = FALSE)
  }

  # Append the tonnage, value, and ton-miles to each record from the FAF year
  # closest to the target year and scale them on the fly.
  fhwa_db$year <- faf_year
  fhwa_db$exp_tons <- fhwa_db[[paste0("tons_", faf_year)]] * 1e3
  fhwa_db$exp_value <- fhwa_db[[paste0("value_", faf_year)]] * 1e6
  fhwa_db$year <- target_year   # Reset to the user-specified year afterwards

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
    if (!is.null(external_regions)) stop(paste("No internal regions were",
      "specified, but external regions were"))
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
    if (!is.null(external_regions)) {
      # If the user has specified a string for this parameter assume that it is
      # a fully qualified filename. Read the data into a tibble.
      external_regions <- swimctr:::load_tabular_data(external_regions)

      # The outer region flows are usually coded as one way, but of course the
      # flows move in opposite direction as well. Thus, we'll first need to add
      # the other direction to the flows.
      opposite_direction <- external_regions %>%
        mutate(temp = dms_orig, dms_orig = dms_dest, dms_dest = temp) %>%
        mutate(temp = entry, entry = exit, exit = temp, temp = NULL)
      external_regions <- bind_rows(external_regions, opposite_direction)

      # Now we can merge the flow data with these outer region definitions,
      # which will allow us to carry forward any data included in the latter
      recast <- left_join(recast, external_regions, by = c("dms_orig",
        "dms_dest"))

      # And finally, tag the outer regions as through flows. We will process the
      # list of all through flows as we cannot tag based upon unique or non-
      # missing contents external_regions because we don't know for sure what
      # fields, if any, will be included other than domestic O and D.
      n_external_regions <- nrow(external_regions)
      for (i in (1:n_external_regions)) {
        recast$direction <- ifelse(recast$dms_orig == external_regions$dms_orig[i] &
            recast$dms_dest == external_regions$dms_dest[i], "through",
          recast$direction)
      }
    }
  }

  # The trade type is coded as a numeric variable, but it will make downstream
  # use less error-prone to recast as descriptive string. In this case we only
  # need to differentiate between domestic and cross-border flows.
  redo_trade_type <- mutate(recast,
    trade_type = case_when(trade_type == 1 ~ "domestic",
      trade_type %in% c(2, 3) ~ "x-border", TRUE ~ NA_character_))

  # We will also recode the mode of transport while we're at it
  redo_transport_mode <- mutate(redo_trade_type,
  	domestic_mode = case_when(dms_mode == 1 ~ "Truck", dms_mode == 2 ~ "Rail",
  	  dms_mode == 3 ~ "Water", dms_mode == 4 ~ "Air", dms_mode == 5 ~ "Multiple",
  	  dms_mode == 6 ~ "Pipeline", dms_mode == 7 ~ "Other",
  	  dms_mode == 8 ~ "NoDomestic", TRUE ~ NA_character_))

  # Drop the records whose direction are not internal, inbound, outbound, or
  # through
  keep <- filter(redo_transport_mode, direction != "drop")
  print(paste(nrow(keep), "records retained:"), quote = FALSE)
  print(table(keep$direction, useNA = "ifany"))

  # How many records have zero transactions?
  n_zeros <- nrow(filter(keep, exp_value <= 0.0, exp_tons <= 0.0))
  pct_zeros <- swimctr::percent(n_zeros, nrow(keep))
  print(paste0(n_zeros, " of ", nrow(keep), " records (", pct_zeros,
    "%) have zero tons and value coded"), quote = FALSE)

  # Show us the total flows by mode and direction
  print("Annual tonnage by direction and mode for modeled area:", quote = FALSE)
  zed <- round(xtabs(exp_tons ~ domestic_mode + direction, data = keep), 1)
  print(addmargins(zed))

  # FAF 5.x uses the midpoints of large distance bands to calculate ton miles.
  # Since we've added `wgt_dist`, introduced in FAF 4.x, to our data we can
  # calculate much more exact values.
  keep$exp_tmiles <- round(keep$exp_tons * keep$wgt_dist, 1)

  # Finally, drop the fields that we do not need in downstream CT modules
  export <- select(keep, year, fr_orig, dms_orig, dms_dest, fr_dest,
    fr_inmode, dms_mode, fr_outmode, sctg2, trade_type, exp_tons, exp_value,
    exp_tmiles, direction, entry, exit, domestic_mode, wgt_dist)

  # Return the results, which depends on whether we found problems with the
  # distances above
  if (crash_signal == TRUE) export = tibble()  # Send back an empty tibble
  return(export)
}
