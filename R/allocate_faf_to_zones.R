#' Allocate FAF daily truck flows to traffic analysis zones
#'
#' @param daily_faf_trips Data frame containing discrete daily FAF truck trips
#' @param synthetic_firms Data frame containing the synthetic firms and their
#'   attributes, either exogenously specified or created within this platform
#' @param makeuse Data frame containing input-output make and use coefficient,
#'   previously translated from PECAS format to that usable by this platform
#' @param intermodal_connectors Data frame containing the location (traffic
#'   analysis zone) of air and sea ports within the modeled airport, and
#'   percentage of such flows originating and terminating within the FAF region
#'   they are situated in should be allocated to them
#' @param external_gateways Data frame containing the external stations (traffic
#'   analysis zone) that truck flows to or from FAF regions outside of the
#'   modeled area enter or leave through
#'
#' @details This function allocates discrete daily FAF (inter-regional) truck
#'   flows, coded to FAF regions, to traffic analysis zones within the modeled
#'   area. Input-output coefficients are used to map the commodity carried by
#'   each truck (or formerly or next carried, in case of empty trucks) to the
#'   industry type most likely to produce or consume it. Employment within the
#'   chosen industry within all zones situated within the FAF region are sampled
#'   to choose the internal trip end. Finally, the flows are assigned to
#'   external gateways entering or leaving the region. For trucks, external FAF
#'   regions are associated with a particular gateway. Note that through truck
#'   trips are coded to two external gateways. For import or export flows the
#'   appropriate intermodal connector (air or sea port) is chosen instead of
#'   internal zone within the modeled area. The result is a complete truck trip
#'   record at the traffic analysis zone level, for each daily truck trip in the
#'   dataset.
#'
#' @export
#' @examples
#' daily_faf_flows <- allocate_faf_to_zones(daily_faf_trips, synthetic_firms,
#'   makeuse, intermodal_connectors, external_gateways)

allocate_faf_to_zones <- function(daily_faf_trips, synthetic_firms, makeuse,
  intermodal_connectors, swim_external_zones = 5001:5012) {
  # Announce yourself
  print(swimctr:::self_identify(match.call()), quote = FALSE)
  
  # Define the list of modes included in the intermodal connectors tibble. We
  # assume that an alpha zone (`taz`) is defined for each of these included
  # modes within each FAF region within the SWIM modeled area
  all_intermodal_modes <- sort(unique(intermodal_connectors$mode))
  
  # A function that chooses a specific intermodal connector within a given FAF
  # region
  choose_intermodal_connector <- function(this_region, this_mode) {
    # Grab the eligible intermodal connectors for the specified region and
    # target mode of transport
    available <- filter(intermodal_connectors, faf_region == this_region,
      mode == this_mode)

    # If no eligible connectors can be found then sound the alarm
    if (nrow(available) == 0) stop(paste(this_mode,
      "connectors not found in region", this_region))

    # Otherwise choose from available modal connectors within the FAF region
    if (nrow(available) == 1) {
      # The choice is simple if only a single connector is found
      choice <- as.integer(available$taz)
    } else {
      # Choose from between multiple connectors based upon user-defined weights
      choice <- sample(available$taz, size = 1, prob = available$weight)
    }
    return(choice)
  }

  # We need to determine which FAF regions are within the modeled area. We can
  # quickly deduce this from the synthetic firm data.
  internal_regions <- sort(unique(synthetic_firms$faf_region))

  # Create a list of the unique sectors found in the synthetic firms
  all_categories <- sort(unique(synthetic_firms$category))

  # It will be considerably easier to simply return the list of origin and
  # destination zones, which we can merge with the original trip origins. We
  # will create a sequence number to faciltate that.
  daily_faf_trips$sequence <- 1:nrow(daily_faf_trips)

  # We also need to recode the foreign inbound and outbound modes, for we are
  # concerned with air and water intermodal connections only. We don't know what
  # multi is, but assume it is truck-rail connection. And obviously if truck is
  # both foreign and domestic modes we don't need to worry about connections.
  all_faf_modes <- c("Truck", "Rail", "Water", "Air", "Multiple", "Pipeline",
    "Other", "None")
  add_foreign_modes <- mutate(daily_faf_trips,
    foreign_inmode = all_faf_modes[as.integer(fr_inmode)],
    foreign_outmode = all_faf_modes[as.integer(fr_outmode)])

  # Define a function to recode the originating and terminating FAF regions to
  # traffic analysis zones. This is where most of the processing will take
  # place.
  choose_zones <- function(replicant, i) {
    # How we code the origin and destination zone will depend upon whether it is
    # internal to the modeled area, outside of it, or connecting to different
    # mode of transport. Start by processing the originating FAF region
    if (replicant$dms_orig %in% swim_external_zones) {
      # If the origin is an external station we don't care whether it went 
      # through intermodal connector before getting there
      ozone <- replicant$dms_orig
    } else {
      # If the foreign inbound mode is defined then bring it into the modeled
      # area through a connector appropriate for it.
      if (replicant$foreign_inmode %in% all_intermodal_modes) {
        ozone <- choose_intermodal_connector(replicant$dms_orig,
          replicant$foreign_inmode)
      } else {
        # Choose an internal zone within this FAF region. However, not all firms
        # are equally likely. We'll use make coefficients to scale employment in
        # each firm, which is an expensive but helpful calculation.
        make_coefficients <- filter(makeuse, MorU == "M", sctg2 == replicant$sctg2)
        if (nrow(make_coefficients) == 0) {
          # If there are no make coefficients defined by AA for the current
          # commodity then we just have to assume that all firms are equally
          # likely to produce it.
          make_coefficients <- tibble(category = all_categories, coefficient = 1.0)
        }
        # Sample from the eligible firms within the origin FAF region
        eligible_firms <- synthetic_firms %>%
          filter(faf_region == replicant$dms_orig) %>%
          left_join(make_coefficients, by = "category") %>%
          filter(!is.na(coefficient)) %>%   # Remove irrelevant firms
          mutate(zed = employees * coefficient)
        ozone <- sample(eligible_firms$Azone, size = 1, prob = eligible_firms$zed)
      }
    }

    # Now we carry out the same tortured process for choosing the destination
    # zone, except that use coefficients are used instead
    if (replicant$dms_dest %in% swim_external_zones) {
      # We don't care where the trip goes once it leaves the SWIM modeled area
      dzone <- replicant$dms_dest
    } else {
      # If foreign export mode is chosen then associate it with an appropriate
      # intermodal connector from within the internal region
      if (replicant$foreign_outmode %in% all_intermodal_modes) {
        dzone <- choose_intermodal_connector(replicant$dms_dest,
          replicant$foreign_outmode)
      } else {
        # Choose from firms within the internal destination FAF region
        use_coefficients <- filter(makeuse, MorU == "U", sctg2 == replicant$sctg2)
        if (nrow(use_coefficients) == 0) {
          # If use coefficients are not defined for this SCTG then create
          # bogus coefficients that are same for all industries (in effect
          # eliminating influence of use coefficient)
          use_coefficients <- tibble(category = all_categories, coefficient = 1.0)
        }
        eligible_firms <- synthetic_firms %>%
          filter(faf_region == replicant$dms_dest) %>%
          left_join(use_coefficients, by = "category") %>%
          filter(!is.na(coefficient)) %>%
          mutate(zed = employees * coefficient)
        dzone <- sample(eligible_firms$Azone, size = 1, prob = eligible_firms$zed)
      }
    }

    # At this point we should have valid origin and destination zones, which
    # we'll return in a data frame
    tibble(sequence = replicant$sequence, origin = ozone, destination = dzone)
  }

  # Finally, we get to endure a lot of debugging by trying to run the daily FAF
  # trips through this
  myCluster <- parallel::makeCluster(parallel::detectCores(),
    outfile = RTP[["ct.cluster.logfile"]])
  doParallel::registerDoParallel(myCluster)
  print(paste("doParallel cluster instance started with", 
    foreach::getDoParWorkers(), "cores"), quote = FALSE)  
  simulation_start <- proc.time()
  results <- foreach(i = 1:nrow(add_foreign_modes), .packages = c("dplyr")) %dopar%
    choose_zones(add_foreign_modes[i,], i)
  assigned_zones <- bind_rows(results)

  # We need to merge the origin and destination zones with the corresponding
  # daily FAF trip records
  final <- left_join(add_foreign_modes, assigned_zones, by = "sequence")
  final$sequence <- NULL  # No longer required
  stopCluster(myCluster)
  simulation_stop <- proc.time()
  elapsed_seconds <- round((simulation_stop-simulation_start)[["elapsed"]], 1)
  print(paste("Simulation time=", elapsed_seconds, "seconds"), quote = FALSE)

  # Summarise trips by direction
  s1 <- final %>%
    group_by(direction) %>%
    summarise(trips = n(), value = sum(value), tons = sum(tons)) %>%
    mutate(pct_trips = swimctr:::percent(trips))
  print(paste(nrow(final), "daily trips allocated to alpha zones:"),
    quote = FALSE)
  print(as.data.frame(s1))

  # Report the percent of trips that are intrazonal
  n_intrazonals <- nrow(filter(final, origin == destination))
  if (n_intrazonals > 0) {
    pct_intrazonals <- swimctr::percent(n_intrazonals, nrow(final))
    print(paste(pct_intrazonals, "percent of trips are intrazonal"),
      quote = FALSE)
  }

  return(final)
}
