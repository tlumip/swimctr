#' Sample daily FAF flows from annual truckload equivalents
#'
#' @param annual_faf_flows Tibble containing annual FAF truckload equivalents
#' @param external_targets Tibble containing the target entry and exit truck
#'   volumes at the FAF external zones for each possible `t.year`
#' @param target_year Integer value of the current SWIM2 simulation year
#' @param internal_faf_region A list of the FAF regions included within the SWIM
#'   halo, defaults to 411 (Oregon portion of Portland MSA), 419 (remainder of
#'   Oregon), and 532 (Washington portion of Portland MSA).
#' @param daily_scaling_factor Real number the specifies the proportion of FAF
#'   annual truckload trips that will occur on the simulation day, defaults to
#'   calibrated value that best reproduces 2020 observed flows
#'
#' @details This function samples daily FAF truck trips from the previously
#'   created annual FAF truckload equivalents. The user will specify the truck
#'   volumes at each SWIM external station, which will constrain the sampled
#'   annual flows. A separate user-defined value must be supplied by the user to
#'   sample the annual internal flows.
#'
#' @export
#' @examples
#' daily_faf_flows <- sample_daily_FAF_flows(annual_faf_flows, external_targets,
#'   daily_scaling_factor = 0.00385)

sample_daily_faf_flows <- function(annual_faf_flows, target_year, external_targets,
  internal_faf_regions = c(411, 419, 532), daily_scaling_factor = 1.274154e-3) {
  # Announce yourself
  print(swimctr:::self_identify(match.call()), quote = FALSE)

  # Define local helper functions
  decrement_ <- function(x) { eval.parent(substitute(x <- x - 1)) }

  # Load the target external truck flows if not supplied as a tibble and build
  # a list of the included external zone numbers
  external_targets <- swimctr:::load_tabular_data(external_targets) %>%
    filter(t.year == target_year)
  if (nrow(external_targets) == 0) {
    print(paste("No external zone contraint volumes found for t.year", t.year),
      quote = FALSE)
    stop("Missing external zone constraint volumes")
  } else {
    print(paste(nrow(external_targets), "external volumes extracted for t.year",
      target_year), quote = FALSE)
  }
  all_external_stations <- sort(unique(external_targets$external_sta))

  # Load the annual FAF flows if they are not supplied as a tibble
  annual_faf_flows <- swimctr:::load_tabular_data(annual_faf_flows)

  # Summarise the external flows by direction to obtain the percentage accounted
  # for by through movements (i.e., passing through the SWIM halo)
  external_sums <- filter(annual_faf_flows, direction != "internal") %>%
    group_by(direction) %>%
    summarise(n = n()) %>%
    mutate(phi = round(n / sum(n), 3))
  avg_through_flows <- external_sums$phi[external_sums$direction == "through"]

  # Add through flows to our list of targets and shorten the tibble name
  targets <- mutate(external_targets,
    entry_rem = trunc(entry * avg_through_flows),
    exit_rem = trunc(exit * avg_through_flows))

  # Extract the through flows from our annual FAF flow data and sort them in
  # random order
  all_throughs <- annual_faf_flows %>%
    filter(dms_orig %in% all_external_stations, dms_dest %in% all_external_stations) %>%
    mutate(randy = rnorm(nrow(.)), chosen = 0) %>%
    arrange(randy)

  # Evaluate each through trip, tagging it for inclusion if we still need that
  # movement and decrementing the entry and exit zone remainders
  for (i in 1:nrow(all_throughs)) {
    # Grab the origin and destination from this trip record
    this_orig <- all_throughs$dms_orig[i]; this_dest <- all_throughs$dms_dest[i]

    # If can accommodate this flow do so and decrement the entry and exit targets
    if (targets$entry_rem[targets$external_sta == this_orig] > 0 &
        targets$exit_rem[targets$external_sta == this_dest] > 0) {
      all_throughs$chosen[i] <- all_throughs$chosen[i] + 1
      decrement_(targets$entry[targets$external_sta == this_orig])
      decrement_(targets$entry_rem[targets$external_sta == this_orig])
      decrement_(targets$exit[targets$external_sta == this_dest])
      decrement_(targets$exit_rem[targets$external_sta == this_dest])
    }
  }

  # We only want to keep the records that contains one or more through movements
  sampled_flows <- uncount(all_throughs, chosen)

  # Next sample internal-external flows, which have origins within the halo and
  # destinations outside of it.
  for (this_external_exit in all_external_stations) {
    # Extract the internal-external flows associated with this external station
    revised_subset <- filter(annual_faf_flows,
      dms_orig %in% internal_faf_regions, dms_dest == this_external_exit)
    N_subset <- nrow(revised_subset)
    if (N_subset < 1) next  # Not all possible interchanges are present in FAF

    # What is our target nunber of trips through this external station?
    N_target <- targets$exit[targets$external_sta == this_external_exit]
    if (N_target < 1) next  # Less likely case where no user-specified trips

    # If we've gotten this far sample the scaled number of truck trip records
    this_sample <- mutate(revised_subset, randy = rnorm(nrow(revised_subset))) %>%
      arrange(randy) %>%
      slice(1:N_target)
    sampled_flows <- bind_rows(sampled_flows, this_sample)
  }

  # Do the same with external-internal flows, which have origins in the external
  # zones and destinations within the SWIM halo
  for (this_external_entry in all_external_stations) {
    # Grab the external-internal flows entering through this external station
    revised_subset <- filter(annual_faf_flows, dms_orig == this_external_entry,
      dms_dest %in% internal_faf_regions)
    N_subset <- nrow(revised_subset)
    if (N_subset < 1) next  # This interchange not found in the FAF data

    # What is our target nunber of trips entering this external station?
    N_target <- targets$entry[targets$external_sta == this_external_entry]
    if (N_target < 1) next   # No trips requested for this external station

    # Sample the scaled number of truck trip records
    this_sample <- mutate(revised_subset, randy = rnorm(nrow(revised_subset))) %>%
      arrange(randy) %>%
      slice(1:N_target)
    sampled_flows <- bind_rows(sampled_flows, this_sample)
  }

  # Finally, sample the internal trips
  sampled_internals <- annual_faf_flows %>%
    filter(dms_orig %in% internal_faf_regions, dms_dest %in% internal_faf_regions) %>%
    mutate(randy = rnorm(nrow(.))) %>%
    arrange(randy) %>%
    slice(1:(trunc(nrow(.) * daily_scaling_factor)))
  sampled_flows <- bind_rows(sampled_flows, sampled_internals)

  # Summarise the results
  annual_sum <- group_by(annual_faf_flows, direction) %>%
    summarise(annual = n()) %>%
    mutate(annual_pct = swimctr::percent(annual))
  sampled_sum <- sampled_flows %>%
    group_by(direction) %>%
    summarise(sampled = n()) %>%
    full_join(annual_sum, ., by = "direction") %>%
    mutate(sampled_pct = swimctr::percent(sampled),
      tau = swimctr::percent(sampled, annual))
  print(paste(nrow(sampled_flows), "daily of", nrow(annual_faf_flows),
    "annual FAF truckload equivalents sampled:"), quote = FALSE)
  print(as.data.frame(sampled_sum))

  # Export the sampled daily truck flows
  return(select(sampled_flows, -randy))
}
