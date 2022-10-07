#' Simple destination choice model for local (internal) truck trips
#'
#' @param truck_origins A tibble containing discrete daily trips for each tour,
#'   usually produced by a trip or tour generation function
#' @param skim_matrices A tibble containing skim distance and travel time for
#'   each origin-destination pair, usually only containing records for zone
#'   pairs that fall within user-defined threshold (i.e., excludes what would be
#'   only a long-distance trip)
#' @param utility_parameters A tibble containing weights for distance and
#'   attractors for each truck type defined in the simulation
#'
#' @details This is a simple destination choice model that mimics a singly-
#'   constrained gravity model. It samples eligible destinations based upon the
#'   product of interzonal distance and attractions, however defined. The
#'   distances are typically limited to a certain distance (120-180 miles),
#'   beyond which are considered the realm of long-distance trips. Different
#'   weights for distance (alpha1) and attractors (alpha2) ar defined for each
#'   truck type (and can be all set to unity to eliminate such scaling),
#'   otherwise known as utility parameters. In a typical gravity model
#'   formulation we divide the product of attractions and interzonal impedance
#'   by the sum of them for all zones. Sampling rather than calculating
#'   deterministic proportions obviates the need for this step. The output from
#'   this function is the same as the input truck origins, with the destination
#'   appended. The user can optionally also save this data frame in a comma-
#'   separated value file.
#'
#' @export
#' @examples
#' sample_local_truck_destinations(truck_origins, skim_matrices,
#'   utility_parameters)

sample_local_truck_destinations <- function(truck_origins, skim_matrices,
  trip_length_targets, utility_parameters) {
  print(swimctr:::self_identify(match.call()), quote = FALSE)
  simulation_start <- proc.time()

  # The trip length targets are defined by truck type, and provided in wide
  # format. Convert to tall format to make subsetting easier.
  max_target_distance <- max(trip_length_targets$distance)
  targets <- trip_length_targets %>%
    gather(truck_type, probability, -distance) %>%
    filter(probability > 0.0) %>%
    rename(distanceI = distance)  # To match with integer distance

  # It is possible that a zone might be coded further from its nearest neighbor
  # such that its intrazonal travel time is longer than the maximum target
  # value. That is likely to be error in skim processing, for intrazonal times
  # are small even in rural areas. Thus, we will recode intrazonal distances to
  # fall within our specified range, so that at least intrazonal is an option.
  skim_matrices <- mutate(skim_matrices,
    distance = ifelse(origin == destination & distance > max_target_distance,
      max_target_distance-1, distance))

  # Now we can eliminate all of the skim zone pairs where the distance is beyond
  # threshold (maximum target distance), which will hopefully make processing
  # the remainders faster
  skims <- skim_matrices %>%
    filter(distance <= max_target_distance) %>%
    mutate(distanceI = round(distance, 0))

  # Create a list of eligible destination zones from the skim matrix (if it is
  # not defined then it is outside of our threshold, or is zone that has no
  # corresponding centroid connector in the network file)
  dzones <- sort(as.integer(unique(skims$destination)))
  print(paste(length(dzones), "destination zones found in skim matrices"),
    quote = FALSE)

  # Determine which truck types are defined in the trip origins
  truck_types <- unique(truck_origins$truck_type)

  # Check to make sure that we don't have trips originating from alpha zones
  # that are not in the skim matrix
  CTO <- sort(unique(truck_origins$origin))
  problem_children <- list()
  for (c in CTO) {
    if (!c %in% dzones) problem_children <- c(problem_children, c)
  }
  if (length(problem_children > 0)) {
    error_message <- "Alpha zones in trip list with no corresponding skim"
    print(paste0(error_message, ':'), quote = FALSE)
    print(unlist(problem_children))
    stop(error_message)
  }

  # Sum the attractors by alpha zone. We will need to have data defined for each
  # of the zones defined in the skim matrix, even if some have zero truck trips
  # associated with it. Thus, we'll substitute any missing values with zeros.
  attractors <- truck_origins %>%
    group_by(origin, truck_type) %>%
    summarise(attractors = n()) %>%
    rename(destination = origin)

  # Finally, run the model. The ideal distributions and alpha parameters differ
  # by truck type, so we will handle each one differently. At the end of
  # handling each truck type we will add those results to a final data table
  # that will have OD flows.
  origin_zones <- sort(unique(truck_origins$origin))
  truck_origins$destination <- NA   # We'll fill this in as we go along
  for (t in truck_types) {

    print(paste("Sampling destinations for",
      nrow(filter(truck_origins, truck_type==t)), t, "origins"), quote=FALSE)

    # Grab the target trip distances for this particular truck type only
    these_targets <- filter(targets, truck_type == t)

    # Grab the relevant attractors and scale them
    these_attractors <- attractors %>%
      filter(truck_type == t) %>%
      mutate(attractors = attractors *
        utility_parameters$alpha2[utility_parameters$truck_type == t])

    # Process each origin in turn
    for (this_origin in origin_zones) {
      # Determine how many trips we have
      these_origins <- filter(truck_origins, origin == this_origin, truck_type == t)
      N <- nrow(these_origins)
      if (N == 0) next

      # We first need to assign probability to each integer distance in the skim
      # matrix, which we can easily do by merging it with the targets. Drop
      # cases where the probability is undefined (i.e., outside our max target
      # distance)
      these_skims <- skims %>%
        filter(origin == this_origin) %>%
        left_join(these_targets, by = "distanceI") %>%
        filter(!is.na(probability)) %>%
        mutate(probability = probability *
          utility_parameters$alpha1[utility_parameters$truck_type == t])

      # But othewise calculate the total attractiveness (zed) of each possible
      # destination, and then sample from that the required number of times
      combined <- these_skims %>%
        left_join(these_attractors, by = "destination") %>%
        mutate(zed = probability * attractors) %>%
        filter(!is.na(zed))

      # Check that we have at least two destination choices. If not, show us the
      # 10 closest skims and attractions associated with them. Then create a new
      # data frame that allows us to choose intrazonal trip.
      if (nrow(combined) < 2) {
        # Tattle on the closest neighboring zone
        ex_skims <- filter(skim_matrices, origin == this_origin,
          destination != this_origin)
        nearest_neighbor <- round(min(ex_skims$distance, na.rm = TRUE), 1)
        print(paste("No neighbor within range: zone=", this_origin,
          ", nearest=", nearest_neighbor, ", ", N, " ", t,
          " trips set to intrazonal", sep = ''), quote = FALSE)

        # Then spin up the intrazonal alternative
        combined <- tibble(destination = this_origin, zed = 0.5, seq = 1:2)
      }

      # And finally, choose the destination(s) for trip originating in the
      # currently processed origin
      sampled_destinations <- sample(combined$destination, N, replace = TRUE,
        prob = combined$zed)
      truck_origins$destination[truck_origins$origin == this_origin &
          truck_origins$truck_type == t] <- sampled_destinations
    }
  }

  # There should be none, but check that we have no cases where the destination
  # is a missing value. If we find that pathological case then stop the
  # simulation.
  problems <- filter(truck_origins, is.na(destination))
  if (nrow(problems) > 0) {
    error_message <- paste("Destinations not processed for", nrow(problems),
      "cases")
    readr::write_csv(problems, "destinations_not_assigned.csv")
    stop(error_message)
  }

  # Append the travel time and distance for each distance to the trip records
  appended <- truck_origins %>%
    left_join(skim_matrices, by = c("origin", "destination"))

  # Report summary statistics for this truck type
  for (t in truck_types) {
    these_trucks <- filter(appended, truck_type == t)
    S <- paste0("Sampled ", t, " destinations: n=", nrow(these_trucks), " avg distance=",
      round(mean(these_trucks$distance, na.rm = TRUE),1), " avg travel time=",
      round(mean(these_trucks$travel_time, na.rm = TRUE),1))
    print(S, quote = FALSE)
  }

  # Tell us the outcome and exit stage right
  simulation_stop <- proc.time()
  elapsed_seconds <- round((simulation_stop-simulation_start)[["elapsed"]], 1)
  print(paste("Simulation time=", elapsed_seconds, "seconds"), quote = FALSE)
  return(appended)
}
