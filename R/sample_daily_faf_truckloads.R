#' Sample daily truck trips from annual FAF flow database
#'
#' @param annual_faf_trucks Data frame containing reformatted FAF commodity flow
#'   database with estimate of total annual truckload equivalents appended
#' @param external_scaling_factor Asserted or estimating factor to account for
#'   FAF commodity flows that grouped into single truck movements (default is
#'   1.0, which assumes no such factoring is carried out)
#' @param weeks_per_year The number of weeks per year the annual flows are
#'   assumed to be spread across (default = 50)
#' @param target_week The week of the year that the simulated flows will be
#'   extracted for (default = 17)
#' @param target_day The day of the week the flows are sampled for (default = 4,
#'   which corresponds to Wednesday)
#' @param debug_trace A boolean variable signifying whether extended messaging
#'   is enabled (defaults to FALSE)
#'
#' @details This function samples daily truck trips from FAF annual truckload
#'   equivalents. It samples for a Wednesday in the 17th week of the year by
#'   default, which typically falls in late April. The user can specify both the
#'   `target_day` (where Sunday is 1, Monday is 2, ...) and `target_week`. There
#'   are always 52 weeks/year, of course, but the user might want to specify a
#'   slightly smaller value for `weeks_per_year` to account for major holidays
#'   when all commercial activities are lower.
#'
#'   An `external_scaling_factor` can also be set that applies to inbound,
#'   outbound, and through truck flows. It defaults to 1.0 (i.e., no scaling),
#'   but can be set based upon truck intercept surveys or assertion to account
#'   for several shipments consolidated on a single truck. It might make sense
#'   to allow the user to specify a data frame with different scaling factors
#'   for each FAF region origin-destination pair of by distance band, but for
#'   now a single value is applied to all flows with one or both trip ends
#'   outside of the modeled area.
#'
#' @export
#' @examples
#' daily_trucks <- sample_daily_faf_truckloads(annual_faf_truckloads, 1.0)


sample_daily_faf_truckloads <- function(annual_faf_trucks,
  external_scaling_factor = 1.0, weeks_per_year = 50, target_week = 17,
  target_day = 4, debug_trace = FALSE) {

  # If we are scaling the external trips then apply the factor to affected trips
  if (external_scaling_factor != 1.0) {
    print(paste("Applying FAF external equivalencies scaling factor =",
      external_scaling_factor), quote = FALSE)
    annual_faf_trucks$annual_trucks <-
      ifelse(annual_faf_trucks$direction != "Internal",
        round(annual_faf_trucks$annual_trucks * external_scaling_factor, 0),
        annual_faf_trucks$annual_trucks)
  }

  # Tell us what our targets are (mostly for debugging and development use)
  annual_target <- round(sum(annual_faf_trucks$annual_trucks), 0)
  weekly_target <- round(sum(annual_faf_trucks$annual_trucks)/weeks_per_year, 0)
  daily_target <- round(weekly_target / 7.0, 0)
  print(paste0("Sampling truckload equivalents: annual = ", annual_target,
    ", weekly = ", weekly_target, ", daily = ", daily_target), quote = FALSE)

  # Build a function to sample the week and day for each annual truck trip.
  # Trips occurring on the sampled week and day are returned as fully-
  # attributed daily truck trip data table. This is pretty time-consuming if run
  # run monolithically, so using with parallel() or similar is a practical
  # necessity.
  sample_annual_truck_trips <- function(replicant, i, debug_msg = debug_trace) {
    # We will refer to number of trucks in this replicant several times
    trucks_in_replicant <- replicant$annual_trucks
    replicant$sequence <- i

    # Grab the probabilities associated with each trip
    draws <- dplyr::data_frame(sequence = i,
      week = sample(1:weeks_per_year, replicant$annual_trucks, replace = TRUE),
      day = sample(1:7, replicant$annual_trucks, replace = TRUE)
    )

    # Do any of the trips occur within our target week and day? If so append
    # the important details from the replicant record.
    keep <- dplyr::filter(draws, week == target_week, day == target_day)
    trucks <- nrow(keep)
    if (debug_msg == TRUE) {
      print(paste0("replicant ", i, ": draws=", nrow(draws), " trucks=", trucks),
        quote = FALSE)
    }
    if (trucks > 0) {
      keep <- keep %>%
        dplyr::mutate(value = round(replicant$exp_value / trucks_in_replicant, 0),
          tons = round(replicant$exp_tons / trucks_in_replicant, 1)) %>%
        dplyr::select(-week, -day) %>%
        dplyr::full_join(dplyr::select(replicant, -exp_tons, -exp_value,
          -annual_trucks, -zed), by = "sequence") %>%
        dplyr::select(-sequence)
    } else {
      keep <- dplyr::data_frame()  # Delete the contents if no trucks generated
    }

    # Return the results
    return(keep)
  }

  # Build the database of daily truck trips
  simulation_start <- proc.time()
  combined <- foreach(i = 1:nrow(annual_faf_trucks), .packages=c("dplyr")) %dopar%
    sample_annual_truck_trips(annual_faf_trucks[i,], i, debug_trace)
  recombined <- dplyr::bind_rows(combined)  # Convert back to data frame
  simulation_stop <- proc.time()
  elapsed_seconds <- round((simulation_stop-simulation_start)[["elapsed"]], 1)
  convergence <- round((nrow(recombined)/daily_target), 3)
  print(paste("Simulation time=", elapsed_seconds, "seconds, convergence=",
    convergence), quote = FALSE)
  print(paste(nrow(recombined),
    "daily truck records generated from weekly samples"), quote = FALSE)

  # Give us a brief summary and then head for the beach
  print("Daily truck trips generated by direction and type:", quote = FALSE)
  print(addmargins(xtabs(~direction + vehicle_type, data = recombined)))
  return(recombined)
}
