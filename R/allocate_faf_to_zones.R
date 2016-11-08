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
#' @param cluster_log The filename of a file used to store output from the
#'   doParallel cluster (optional)
#' @param save_to File name for saving the FAF trip records with traffic
#'   analysis zones appended in comma-separated value format (optional)
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
#'   makeuse, intermodal_connectors, external_gateways, "daily_faf_flows.csv")

allocate_faf_to_zones <- function(daily_faf_trips, synthetic_firms, makeuse,
  intermodal_connectors, external_gateways, cluster_log = "cluster.log",
  save_to = NULL) {
  
  ct_msg(header = "Allocate daily FAF inter-regional flows to alpha zones")
  
  # A function that chooses a specific intermodal connector within a given FAF
  # region
  choose_intermodal_connector <- function(this_region, this_mode) {
    # Grab the eligible intermodal connectors for the specified region and
    # target mode of transport
    available <- dplyr::filter(intermodal_connectors, faf_region == this_region,
      mode == this_mode)
    
    # If no eligible connectors can be found then sound the alarm
    if (nrow(available)==0) stop(paste(this_mode,
      "connectors not found in region", this_region))
    
    # Otherwise choose from available modal connectors within the FAF region
    if (nrow(available)==1) {
      # The choice is simple if only a single connector is found
      choice <- as.integer(available$taz)
    } else {
      # Choose from between multiple connectors based upon user-defined weights
      choice <- sample(available$taz, 1, prob = available$weight)
    }
    choice
  }
  
  # We will also need a function that returns the external gateway associated
  # with a FAF region external to the model
  get_external_gateway <- function(external_region) {
    # Get the list of external stations associated with this external region
    this_gateway <- external_gateways %>%
      dplyr::filter(faf_region == external_region)
    if (nrow(this_gateway)==0) {
      stop(paste("External gateway not defined for FAF region", external_region))
    }
    this_gateway$external_station
  }
  
  # We need to determine which FAF regions are within the modeled area. We can
  # quickly deduce this from the synthetic firm data.
  internal_regions <- sort(unique(synthetic_firms$faf_region))
  
  # Create a list of the unique sectors found in the synthetic firms
  all_sectors <- sort(unique(synthetic_firms$sector))
  
  # It will be considerably easier to simply return the list of origin and 
  # destination zones, which we can merge with the original trip origins. We
  # will create a sequence number to faciltate that.
  daily_faf_trips$sequence <- 1:nrow(daily_faf_trips)
  
  # We also need to recode the foreign inbound and outbound modes, for we are
  # concerned with air and water intermodal connections only. We don't know what
  # multi is, but assume it is truck-rail connection. And obviously if truck is
  # both foreign and domestic modes we don't need to worry about connections.
  daily_faf_trips <- daily_faf_trips %>%
    dplyr::mutate(fr_inmode = ifelse(!fr_inmode %in% c("Air", "Water"), NA,
      fr_inmode), fr_outmode = ifelse(!fr_outmode %in% c("Air", "Water"), NA,
        fr_outmode))
  
  # Define a function to recode the originating and terminating FAF regions to
  # traffic analysis zones. This is where most of the processing will take 
  # place, but we need to encapsulate it so that we can parallelize it.
  choose_zones <- function(replicant, i) {
    # How we code the origin and destination zone will depend upon whether it is
    # internal to the modeled area, outside of it, or connecting to different
    # mode of transport. Start by processing the originating FAF region
    if (!replicant$dms_orig %in% internal_regions) {
      # It doesn't matter whether it is bound for intermodal or not if it is
      # outside of the modeled area
      ozone <- get_external_gateway(replicant$dms_orig)
    } else {
      # If the foreign inbound mode is defined then bring it into the modeled
      # area through a connector appropriate for it.
      if (!is.na(replicant$fr_inmode)) {
        ozone <- choose_intermodal_connector(replicant$dms_orig, 
          replicant$fr_inmode)
      } else {
        # Choose an internal zone within this FAF region. However, not all firms
        # are equally likely. We'll use make coefficients to scale employment in
        # each firm, which is an expensive but helpful calculation.
        make_coefficients <- makeuse %>%
          dplyr::filter(MorU == "M", sctg2 == replicant$sctg2)
        if (nrow(make_coefficients)==0) {
          # If there are no make coefficients defined by AA for the current
          # commodity then we just have to assume that all firms are equally
          # likely to produce it.
          make_coefficients <- dplyr::data_frame(sector = all_sectors,
            coefficient = 1.0)
        }
        eligible_firms <- synthetic_firms %>%
          dplyr::filter(faf_region == replicant$dms_orig) %>%
          dplyr::left_join(make_coefficients, by = "sector") %>%
          dplyr::filter(!is.na(coefficient)) %>%   # Remove irrelevant firms
          dplyr::mutate(zed = employees*coefficient)
        ozone <- sample(eligible_firms$taz, size = 1, prob = eligible_firms$zed)
      }
    }
    
    # Now we carry out the same tortured process for choosing the destination
    # zone, except that use coefficients are used instead
    if (!replicant$dms_dest %in% internal_regions) {
      # Choose the external gateway that corresponds to the external FAF region
      dzone <- get_external_gateway(replicant$dms_dest)
    } else {
      # If foreign export mode is chosen then associate it with an appropriate
      # intermodal connector from within the internal region
      if (!is.na(replicant$fr_outmode)) {
        dzone <- choose_intermodal_connector(replicant$dms_dest,
          replicant$fr_outmode)
      } else {
        # Choose from firms within the internal FAF region
        use_coefficients <- makeuse %>%
          dplyr::filter(MorU == "U", sctg2 == replicant$sctg2)
        if (nrow(use_coefficients)==0) {
          # If use coefficients are not defined for this SCTG then create 
          # bogus coefficients that are same for all industries (in effect
          # eliminating influence of use coefficient)
          use_coefficients <- dplyr::data_frame(sector = all_sectors,
            coefficient = 1.0)
        }
        eligible_firms <- synthetic_firms %>%
          dplyr::filter(faf_region == replicant$dms_dest) %>%
          dplyr::left_join(use_coefficients, by = "sector") %>%
          dplyr::filter(!is.na(coefficient)) %>%
          dplyr::mutate(zed = employees*coefficient)
        dzone <- sample(eligible_firms$taz, size = 1, prob = eligible_firms$zed)
      }
    }
    
    # At this point we should have valid origin and destination zones, which
    # we'll return in a data frame
    dplyr::data_frame(sequence = replicant$sequence, origin = ozone,
      destination = dzone)
  }
  
  # Finally, we get to endure a lot of debugging by trying to run the daily FAF
  # trips through this
  simulation_start <- proc.time()
  numberOfCores <- detectCores()
  ct_msg(paste("Number of cores detected=", numberOfCores))
  cluster <- makeCluster(numberOfCores, outfile = cluster_log)
  registerDoParallel(cluster)
  results <- foreach(i=1:nrow(daily_faf_trips), .packages = c("dplyr")) %dopar%
    choose_zones(daily_faf_trips[i,], i)
  assigned_zones <- dplyr::bind_rows(results)
  
  # We need to merge the origin and destination zones with the corresponding
  # daily FAF trip records
  final <- dplyr::left_join(daily_faf_trips, assigned_zones, by = "sequence")
  final$sequence <- NULL  # No longer required
  stopCluster(cluster)
  simulation_stop <- proc.time()
  elapsed_seconds <- round((simulation_stop-simulation_start)[["elapsed"]], 1)
  ct_msg(paste("Simulation time=", elapsed_seconds, "seconds"))
  
  # If the user wants intermediate output then by all means, give it to them
  if (!is.null(save_to)) readr::write_csv(final, save_to)
  
  # Finish up and out
  ct_msg(paste(nrow(final), "daily trips allocated to alpha zones"))
  final
}
