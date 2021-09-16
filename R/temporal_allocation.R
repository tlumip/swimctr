#' Carry out temporal allocation for both intercity and local truck trips
#'
#' @param daily_trips A tibble containing daily trip records, typically with
#'   origin and destination traffic analysis zones already appended
#' @param temporal_factors A tibble containing proportions or number of trips
#'   per hour of the day, in 24-hour format (hours defined from 0 to 23), by
#'   truck type
#'
#' @details This function appends a departure time for each trip in data frame
#'   containing trip records. These records can either be for local or inter-
#'   regional (FAF) flows. Since we might have different temporal distibutions
#'   for each type of trip this function would typically be run separately for
#'   each. However, the same set of temporal factors can be applied to each if
#'   desired. The departure hour is sampled from the temporal factors provided
#'   by the user, which can either be expressed as percentages for each hour, or
#'   total observed trips within each hour (from travel survey), or asserted
#'   weights for each hour. The factors must be defined for each truck type in
#'   the simulation, although the same factors can obviously be recycled if
#'   available for only fewer classes (e.g., all combination trucks, rather than
#'   single, double, or triple-trailer combinations). The minute is sampled
#'   randomly and appended to the departure hour in order to create a departure
#'   time. The result is the input data frame with the departure time appended
#'   to it.
#'
#' @export
#' @examples
#' hourly_faf_trucks <- temporal_allocation(daily_faf_trips,
#'   long_distance_factors)
#' hourly_local_trucks <- temporal_allocation(daily_local_trips, hourly_factors)


temporal_allocation <- function(daily_trips, temporal_factors) {
  # Announce yourself
  print(swimctr:::self_identify(match.call()), quote = FALSE)

  # What kind of trips am I working with? If it has foreign origin included it
  # must be a FAF inter-regional trip. Otherwise we'll assume it is a trip in a
  # local truck tour.
  daily_trips$contents <- ifelse("fr_orig" %in% colnames(daily_trips), "FAF",
  	"CT")

  # The FAF truck analysis process is defined in terms of vehicle types, which
  # we leave alone because it includes other modes as well (e.g., carload or
  # container for rail). If this input file has truck type coded that way this
  # is where we'll change it to truck type.
  if ("vehicle_type" %in% colnames(daily_trips)) {
    daily_trips <- rename(daily_trips, truck_type = vehicle_type)
  }

  # We will process the trips by truck type, as the factors differ for each of
  # them (but not generally by local versus long-distance, although the user can
  # create separate truck types that will enable just that).
  all_truck_types <- sort(unique(daily_trips$truck_type))
  daily_trips$departure_time <- NA

  # Process each truck type in turn
  for (t in all_truck_types) {
    # Grab the departure time profiles for this particular truck type
    these_factors <- filter(temporal_factors, truck_type == t)
    if (nrow(these_factors) < 2) {
      error_message <- paste("Missing or insufficient temporal factors for",
        t, "trucks: n=", nrow(these_factors))
      stop(error_message)
    }

    # Sample their departure times
    N <- nrow(filter(daily_trips, truck_type == t))
    print(paste("Sampling departure times for", N, t, "trucks"), quote = FALSE)
    hour <- sample(these_factors$hour, N, replace = TRUE,
      prob = these_factors$share)
    minute <- sample(0:59, N, replace = TRUE)
    daily_trips$departure_time[daily_trips$truck_type == t] <- (hour * 100) + minute
  }

  # Report the results and exit
  print(paste(nrow(daily_trips), "total trips with departure times saved"),
    quote = FALSE)
  return(daily_trips)
}
