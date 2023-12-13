#' Export local and inter-regional trip records in a combined trip list file
#'
#' @param hourly_faf_trips A tibble containing the final inter-regional flows,
#'   with origin, destination, and departure time appended for each trip
#' @param hourly_local_trips A tibble containing the final local truck tour
#'   flows, with origin, destination, and departure time appended for each trip
#'
#' @details This function will typically be the last part of the model to run,
#'   for it combines the local and inter-regional truck trip records into a
#'   single trip file, with common fields for both. Missing values are inserted
#'   for data present in one dataset, but not in the other. The result is a
#'   tibble with trips from both datasets in common value expected by later
#'   SWIM2 components. Code NULL if one of the input tibbles is to be skipped
#'   during model testing.
#'
#' @export
#' @examples
#' export_trip_list(hourly_faf_trips, hourly_local_trips)

export_trip_list <- function(hourly_faf_trips, hourly_local_trips) {
  # Announce yourself
  print(swimctr:::self_identify(match.call()), quote = FALSE)

  # Process the inter-regional (FAF) trip list first. In a few places using
  # formatC isn't enough to prevent R from stubbornly writing out 10-12
  # places past the decimals, so we'll cast it to character at the same time.
  if (is.null(hourly_faf_trips)) {
    regional <- tibble()
  } else {
    regional <- transmute(hourly_faf_trips,
      origin = as.integer(origin),
      destination = as.integer(destination),
      tripStartTime = sprintf("%04d", as.integer(departure_time)),
      tourMode = direction,
      tripMode = NA,  # Formerly loaded or empty
      truckID = NA,  # TO-DO: Tag individual trucks during truck synthesis
      truckType = truck_type,
      sctg2,
      value = paste(formatC(value, digits = 2, format = 'f')),
      tons = as.character(formatC(tons, digits = 2, format = 'f')),
      travelTime = NA,
      distance = formatC(wgt_dist, digits = 1, format = 'f'),
      dataset = contents)
  }

  # Next do the same with local trips. At the present time CT does not specify
  # the commodity of the truck, so it as well as value and tons are set to
  # missing values
  if (is.null(hourly_local_trips)) {
    local <- tibble()
  } else {
    local <- transmute(hourly_local_trips,
      origin,
      destination,
      tripStartTime = sprintf("%04d", as.integer(departure_time)),
      tourMode = "local",
      tripMode = NA,
      truckID = NA,
      truckType = truck_type,
      sctg2 = NA, value = NA, tons = NA,
      travelTime = paste(formatC(travel_time, digits = 1, format = 'f')),
      distance = paste(formatC(distance, digits = 1, format = 'f')),
      dataset = "CT")
  }

  # Now simply combine amd return the two datasets
  combined <- bind_rows(regional, local) %>%
    arrange(origin, destination, tripStartTime) %>%
    mutate(truckID = 1:n())
  print(paste0(nrow(combined), " trip records generated (", nrow(regional),
    " regional and ", nrow(local), " local trips)"), quote = FALSE)
  return(combined)
}
