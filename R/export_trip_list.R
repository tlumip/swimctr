#' Export local and inter-regional trip records in a combined trip list file
#'
#' @param hourly_faf_trips Data frame containing the final inter-regional flows,
#'   with origin, destination, and departure time appended for each trip
#' @param hourly_local_trips Data frame containing the final local (internal)'
#'   truck flows, with origin, destination, and departure time appended for each
#'   trip
#' @param save_to Filename for saving the final combined trip list in comma-
#'   separated value (CSV) format
#'
#' @details This function will typically be the last part of the model to run,
#'   for it combines the local and inter-regional truck trip records into a
#'   single trip file, with common fields for both. Missing values are inserted
#'   for data present in one dataset, but not in the other. The result is saved
#'   in a file for network analyses or post-processing. No values are returned
#'   by the function.
#'
#' @export
#' @examples
#' export_list_list(hourly_faf_trips, hourly_local_trips, "Trips_CTTruck.csv")

export_list_list <- function(hourly_faf_trips, hourly_local_trips, save_to) {

  ct_msg(header = "Export trip list")

  # Process the inter-regional (FAF) trip list first
  regional <- hourly_faf_trips %>%
    dplyr::select(origin, destination, departure_time, status, truck_type,
      distance, sctg2, value, tons, direction) %>%
    dplyr::mutate(travelTime = NA, dataset = "faf",
      departure_time = as.integer(departure_time)) %>%
    dplyr::rename(
      tripMode = status, tourMode = direction,
      tripStartTime = departure_time, truckType = truck_type
      )

  # Next do the same with local trips. At the present time CT does not specify
  # the commodity of the truck, so it as well as value and tons are set to
  # missing values
  local <- hourly_local_trips %>%
    dplyr::select(origin, destination, departure_time, truck_type, distance,
      travel_time) %>%
    dplyr::mutate(departure_time = as.integer(departure_time), dataset = "ct",
      tripMode = "loaded", tourMode = "internal", sctg = NA, value = NA,
      tons = NA) %>%
    dplyr::rename(tripStartTime = departure_time, travelTime = travel_time,
                  truckType = truck_type)

  # Now simply combine the two datasets, and write them out to the specified
  # output file in CSV format
  combined <- dplyr::bind_rows(regional, local)
  readr::write_csv(combined, save_to)
  print(paste(nrow(combined), "total trip records written to", save_to),
    quote = FALSE)

  # No value is returned
}
