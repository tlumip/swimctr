#' Generate annual truckload equivalents using CVS and FAF4 truck factors
#'
#' @param faf_flows A tibble containing FAF truck commodity flow records for
#'   the current model year
#' @param truck_allocation_factors A tibble containing factors that map
#'   commodity flows to different truck types
#' @param payload_distributions A tibble containing the frequency or percentage
#'   of trucks by payload weight in the population for each distance band in the
#'   data
#' @param stop_frequencies A tibble containing the incidence of number of stops
#'   per truck tour (OD movement in the FAF data) by distance band (optional,
#'   defaults to NULL indicating that no stop frequencies will be applied)
#' @param cache_size An integer specifying the number of sampled trucks to be
#'   generated within each distance band (defaults to 1E6)
#' @param trace_flows Turns detailed tracing statements on or off (defaults to
#'   FALSE)
#'
#' @details This function creates a database of discrete truckload equivalents
#'   corresponding to each FAF commodity flow record. This conversion is meant
#'   to be carried out for trucks only, but could be expanded to include mixed
#'   modes of shipment that include trucking. This process represents a
#'   departure from earlier methods based on the FAF4 Freight Traffic
#'   Assignment report. Instead, trucks by type are generated using the FAF
#'   truck allocation factors with cargo weights sampled from observed distri-
#'   butions from the 2012 Canadian Commercial Vehicle Survey (CVS). A data-
#'   base of discrete trucks are created for each FAF commodity flow, with
#'   the sum of tonnage and value for corresponding synthetic trucks equal to
#'   the same values reported in the FAF.
#'
#' @export
#' @examples
#' faf_annual_trucks <- create_annual_truckload_equivalents(faf_data,
#'   truck_allocation_factors, cvs_payload_weight_distributions,
#'   cvs_stop_pattern_frequencies)


create_annual_truckload_equivalents <- function(faf_flows,
  truck_allocation_factors, payload_distributions, stop_frequencies = NULL,
  cache_size = 1e6, trace_flows = FALSE) {
  #---[0]--- Announce yourself
  print(swimctr:::self_identify(match.call()), quote = FALSE)

  #---[1]--- Start doParallel cluster -----------------------------------------
  # Clearly we will have to parallelize this in order to obtain decent runtimes
  myCluster <- parallel::makeCluster(parallel::detectCores(),
    outfile = RTP[["ct.cluster.logfile"]])
  doParallel::registerDoParallel(myCluster)
  print(paste("doParallel cluster instance started with", getDoParWorkers(),
    "cores"), quote = FALSE)
  simulation_start <- proc.time()

  #---[2]--- Check and process commodity flow data ----------------------------
  # Drop records with zero expanded tons, as they'll cause problems later.
  # Report the number of such problem trucks if found.
  nonzero_flows <- filter(faf_flows, exp_tons > 0.0)
  records_dropped <- nrow(faf_flows) - nrow(nonzero_flows)
  if (records_dropped > 0) {
    print(paste(records_dropped, "records with zero tonnage coded dropped"),
      quote = FALSE)
  }

  # We have challenging case where some observations are huge (14M) while most
  # are small (<0.5M). The former can take hours to run a sampling process that
  # is ideal for the latter. So we'll scale the former and then replicate them
  # later to keep the problem tractable.
  scaling_threshold <- 0.5e6
  scaled_flows <- nonzero_flows %>%
    arrange(desc(exp_tons)) %>%
    mutate(flowID = row_number(), distance = as.integer(round(wgt_dist, 0)),
      offset = sample(1:cache_size, nrow(.), replace = FALSE),
      replicants = ifelse(exp_tons <= scaling_threshold, 1,
        trunc(exp_tons / scaling_threshold)),
      scaled_tons = exp_tons / replicants,
      scaled_value = exp_value / replicants)
  print(paste0(nrow(filter(scaled_flows, replicants > 1)), " flows >",
    scaling_threshold, " tons scaled for optimal sampling"), quote = FALSE)

  #---[3]--- Reshape payload distributions ------------------------------------
  # Reshape the payload distributions, as we'll be using the reshaped tibble
  # several times
  payload_distributions <- payload_distributions %>%
    gather(truck_type, frequency, 2:6) %>%
    filter(frequency > 0)

  #---[4]--- Define sampling function -----------------------------------------
  generate_discrete_trucks <- function(this_flow, sampled_trucks, distance_band,
    single_stop_threshold, flow_trace = FALSE) {
    # We define starting place in circular list we start sampling from
    index <- this_flow$offset

    # Generate discrete trucks by truck type and weight until we match or exceed
    # the target expanded tons
    add_trucks <- tibble()
    repeat {
      add_trucks <- bind_rows(add_trucks, sampled_trucks[index,])
      zed <- sum(add_trucks$tons)
      if (zed >= this_flow$scaled_tons) break
      index <- index + 1
      if (index == cache_size) index <- 1
    }

    # We next optionally reduce the number of trucks to account for multiple
    # shipments per truck.
    if (!is.null(single_stop_threshold)) {
      # Sort the truck list in descending order by weight, and then generate
      # list of random numbers in descending order. Delete all records with
      # random number above, which will eliminate the smallest shipments
      add_trucks <- arrange(add_trucks, desc(tons)) %>%
        mutate(key = sort(runif(nrow(.)))) %>%
        filter(key <= single_stop_threshold) %>%
        select(-key)
    }

    # We need to scale the simulated tonnage to the expanded tonnage coded on
    # the flow record. They won't be different for large commodity flows but can
    # differ considerably for very small flows that are smaller than typical
    # payload weight of single truck.
    rho <- this_flow$scaled_tons / sum(add_trucks$tons)
    add_trucks$tons <- rho * add_trucks$tons

    # The value of the shipment is its share of the scaled value, which we don't
    # know but can pivot off of the scaled value
    add_trucks$value <- this_flow$scaled_value *
      (add_trucks$tons / this_flow$scaled_tons)

    # Combine the discrete trucks with their commodity flow attributes
    add_trucks$distance_band <- distance_band
    add_trucks$flowID <- this_flow$flowID
    combined <- left_join(add_trucks, this_flow, by = "flowID")

    # Report the results if requested and exit stage left. By including this
    # within the function the output will go to the doParallel logfile defined
    # at the start of this program rather than cluttering the screen.
    if (trace_flows == TRUE) {
      print(paste0("band=", distance_band, " flowID=", this_flow$flowID,
        ": wgt_dist=", this_flow$distance, " scaled_tons=",
        round(this_flow$scaled_tons, 1), " trucks=", nrow(combined),
        " rho=", round(rho, 4)), quote = FALSE)
    }
    return(combined)
  }  # End function definition

  #---[5] --- Process each distance band in turn ------------------------------
  # At the top level we will work through each distance band in the truck
  # allocation factors
  discrete_trucks <- tibble()
  for (this_band in 1:nrow(truck_allocation_factors)) {
    # Reshape the truck allocation factors for this distance band
    taf <- slice(truck_allocation_factors, this_band) %>%
      gather(truck_type, share, 3:7) %>%
      mutate(share = round(share, 4)) %>%
      filter(share > 0.0)

    # If the user supplies stop pattern frequency data extract the percentage of
    # single-stop trips in this distance band
    if (!is.null(stop_frequencies)) {
      # Determine the lower and upper bounds of the ranges
      taf_lower <- taf$minimum_range[1]; taf_upper <- taf$maximum_range[1]

      # Grab the associated record from the stop patterns that corresponds to
      # single number of shipments (`n_shipments`)
      stop_frequencies_subset <- filter(stop_frequencies,
        lower == taf_lower, upper == taf_upper, n_shipments == 1)
      single_stop_threshold <- stop_frequencies_subset$percent[1] / 100.0
    } else {
      single_stop_threshold <- NULL
    }

    # Generate a list of sampled truck types for this distance band
    sampled_trucks <- tibble(truck_type = sample(taf$truck_type, cache_size,
      replace = TRUE, prob = taf$share), tons = NA_real_)
    sampled_truck_types <- sort(unique(sampled_trucks$truck_type))
    for (this_truck_type in sampled_truck_types) {
      n_trucks <- nrow(filter(sampled_trucks, truck_type == this_truck_type))
      pd <- filter(payload_distributions, truck_type == this_truck_type)
      sampled_trucks$tons[sampled_trucks$truck_type == this_truck_type] <-
        sample(pd$payload_tons, n_trucks, replace = TRUE, prob = pd$frequency)
    }

    # Now that we have our cached list of trucks we will create a list of trucks
    # from them for each flow record
    these_flows <- filter(scaled_flows, distance >= taf$minimum_range[1],
      distance <= taf$maximum_range[1])
    msg <- paste0("Processing ", nrow(these_flows),
      " flow records in distance band ", this_band, " (", taf$minimum_range[1],
      '-', taf$maximum_range[1], " miles)")
    if (!is.null(stop_frequencies)) msg <- paste0(msg, ": SST=",
      single_stop_threshold)
    print(msg, quote = FALSE)
    band_label <- paste0(this_band, " (", taf$minimum_range[1], '-',
      taf$maximum_range[1], ")")

    #---[6] --- Process each commodity flow record ----------------------------
    results <- foreach(i = 1:nrow(these_flows), .packages = c("dplyr"),
        .errorhandling = "remove") %dopar% {
      generate_discrete_trucks(these_flows[i,], sampled_trucks, band_label,
        single_stop_threshold, flow_trace = TRUE)
    }
    discrete_trucks <- bind_rows(discrete_trucks, bind_rows(results))
  }

  #---[7]--- Combine results into single tibble -------------------------------
  final_results <- discrete_trucks %>%
    tidyr::uncount(replicants) %>%
    select(-distance, -offset, -scaled_tons, -scaled_value)
  years_included <- sort(unique(final_results$year))
  print(paste("Total", paste0(years_included, collapse = "+"),
    "annual trucks synthesized:"), quote = FALSE)
  print(addmargins(xtabs(~ distance_band + truck_type,
    data = final_results, na.action = na.pass, exclude = NULL)))

  #---[8]--- Finish up --------------------------------------------------------
  stopCluster(myCluster)
  simulation_stop <- proc.time()
  elapsed_seconds <- round((simulation_stop - simulation_start)[["elapsed"]], 1)
  print(paste("Simulation time=", elapsed_seconds), quote = FALSE)
  return(final_results)
}
