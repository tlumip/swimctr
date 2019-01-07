#' Generate annual truckload equivalents using FAF3 Traffic Analysis process
#'
#' @param faf_data Data frame containing reformatted FAF commodity flow database
#' @param truck_allocation_factors Data frame containing factors that map
#'   commodity flows to different types of trucks
#' @param truck_equivalency_factors Data frame containing factors that
#'   translate tons into truckload equivalents
#' @param empty_truck_factors Data frame containing factors for calculating
#'   incidence of empty trucks, given the trade (domestic versus foreign) and
#'   truck type
#' @param debug_trace Turns detailed tracing statements on or off (defaults to
#'   FALSE)
#'
#' @details This function appends annual truckload equivalents to the FHWA
#'   Freight Analysis Framework (FAF) commodity flow forecast database. The
#'   process is based upon the FAF3 Traffic Analysis report developed by
#'   Battelle, which maps tonnage to truckload equivalents, including empty
#'   trucks. The function requires reformatted FAF data that replaces several
#'   numeric values with strings that are more intuitive and retains data only
#'   for the target year. The function also requires three tables from Chapter 3
#'   of the FAF Traffic Analysis report, and then follows their methodology
#'   exactly (but without the errors in the report). The function returns an
#'   updated FAF database with truckload equivalents appended to each record.
#'
#' @export
#' @examples
#' faf_annual_trucks <- create_annual_faf_truckloads(faf_data,
#'   truck_allocation_factors, truck_equivalency_factors, empty_truck_factors)


create_annual_faf_truckloads <- function(faf_data, truck_allocation_factors,
  truck_equivalency_factors, empty_truck_factors, debug_trace = FALSE) {
  # Start by reporting the problem size
  truck_flows <- dplyr::filter(faf_data, domestic_mode == "Truck")
  print(paste("Generating annual truckload equivalents for", nrow(truck_flows),
    "FAF truck flow records"), quote = FALSE)

  # Define a helper function to extract truck allocation factors (Table 3-3)
  # for the appropriate distance range. We will normalize the factors within the
  # chosen distance on the fly
  getTruckAllocationFactors <- function(distance) {
    truck_allocation_factors %>%
      dplyr::filter(distance >= minimum_range & distance <= maximum_range) %>%
      dplyr::mutate(alloc_factor = allocation_factor/sum(allocation_factor)) %>%
      dplyr::select(vehicle_type, alloc_factor)
  }

  # The truck equivalency factors found in Appendix A of the FAF3 Freight
  # Traffic Analysis report are used for converting kilotons into truckload
  # equivalents. Convert them from wide to tall format. Since we are dealing
  # with tons we need to scale the factors to account for the differences.
  truck_equivalency_factors <- tidyr::gather(truck_equivalency_factors,
    body_type, equiv_factor, auto:other)

  # We will also define a helper function to pull the appropriate values by
  # commodity and vehicle type.
  getTruckEquivalencyFactors <- function(commodity) {
    dplyr::filter(truck_equivalency_factors, sctg2==commodity, equiv_factor>0.0)
  }

  # Finally, convert the empty truck factors from wide to tall format, and
  # define a helper function to grab appropriate values by body type and flow
  # direction.
  empty_truck_factors <- tidyr::gather(empty_truck_factors, vehicle_type,
    empty_factor, SU:TPT)
  getEmptyTruckFactors <- function(flow_direction) {
    dplyr::filter(empty_truck_factors, crossing_type == flow_direction)
  }

  # Calculate truck equivalencies for each FAF flow record. The resulting table
  # will have flows by each vehicle (truck) type and status (empty, loaded)
  calcTruckloadEquivalencies <- function(replicant, i,
    debug_msg = debug_trace) {
    # Get the factors appropriate for the current distance between domestic FAF
    # regions, trade type, and commodity
    taf <- getTruckAllocationFactors(replicant$wgt_dist)
    tef <- getTruckEquivalencyFactors(replicant$sctg2)
    etf <- getEmptyTruckFactors(replicant$trade_type)

    # Allocate the tonnage to each vehicle (as in Table 3-7)
    taf$tons <- replicant$exp_tons * taf$alloc_factor

    # Merge the tonnage by vehicle type with the truck equivalency factors to
    # get tonnage by vehicle type and body type.
    loaded <- tef %>%
      dplyr::left_join(taf, by="vehicle_type") %>%
      dplyr::mutate(annual_trucks = tons * equiv_factor) %>%
      dplyr::select(-equiv_factor, -alloc_factor, -tons)

    # We need to catch the case where rounding annual truck trips winds up zero
    # for all truck types under consideration. We will always have a minimum of
    # one truck, so pick vehicle and body type that has higher number of them
    check_loaded <- loaded %>%
      dplyr::group_by(vehicle_type) %>%
      dplyr::summarize(annual_trucks = sum(annual_trucks, na.rm = TRUE))
    zed <- max(check_loaded$annual_trucks)
    if (zed < 1.0) {
      loaded <- loaded %>%
        dplyr::arrange(desc(annual_trucks)) %>%
        dplyr::mutate(annual_trucks = 1.0) %>%
        dplyr::slice(1)
    }

    # Calculate the number of empty trucks. If we obtain zero trucks we will
    # accept that.
    empty <- loaded %>%
      dplyr::left_join(etf, by=c("body_type", "vehicle_type")) %>%
      dplyr::mutate(empty_trucks = annual_trucks * empty_factor) %>%
      dplyr::select(-crossing_type, -empty_factor)

    # We needed to retain the body types to calculate empties, but now we can
    # collapse each group to vehicle types
    loaded <- loaded %>%
      dplyr::group_by(vehicle_type) %>%
      dplyr::summarise(annual_trucks = round(sum(annual_trucks), 0)) %>%
      dplyr::mutate(status = "loaded")
    empty <- empty %>%
      dplyr::group_by(vehicle_type) %>%
      dplyr::summarise(annual_trucks = round(sum(empty_trucks), 0)) %>%
      dplyr::mutate(status = "empty")

    # Append the original value and tons for it to operate upon
    loaded$exp_tons <- replicant$exp_tons
    loaded$exp_value <- replicant$exp_value

    # Create a separate record for the combined groups and merge with the data
    # from the flow record. Note that we drop the original value and tons, as
    # they are now split among the various loaded truck types.
    loaded <- loaded %>%
      dplyr::mutate(percent = annual_trucks / sum(annual_trucks),
        exp_tons = percent * exp_tons,
        exp_value = as.numeric(percent * exp_value)) %>%
      dplyr::select(-percent)
    empty <- dplyr::mutate(empty, exp_tons = 0.0, exp_value = 0.0)
    together <- dplyr::bind_rows(loaded, empty)

    # Add the SCTG code back in, as we will use that to merge the two tables
    together$sctg2 <- replicant$sctg2
    final_result <- together %>%
      dplyr::left_join(dplyr::select(replicant, -exp_tons, -exp_value,
        -exp_tmiles), by = "sctg2") %>%
      dplyr::mutate(zed = zed) %>%
      dplyr::filter(annual_trucks > 0)   # Drop truck types with no annual trips

    # Write debugging info is requested, and then return the results
    if (debug_msg == TRUE) {
      print(paste0("replicant ", i, ": zed=", round(zed, 4), " annual trucks=",
        sum(final_result$annual_trucks)), quote = FALSE)
    }
    final_result
  }

  # Run the simulation
  simulation_start <- proc.time()
  results <- foreach(i = 1:nrow(truck_flows), .packages=c("dplyr")) %dopar%
    calcTruckloadEquivalencies(truck_flows[i,], i)
  combined <- dplyr::bind_rows(results)
  simulation_stop <- proc.time()
  elapsed_seconds <- round((simulation_stop-simulation_start)[["elapsed"]], 1)
  print(paste("Simulation time=", elapsed_seconds, "seconds"), quote=FALSE)

  # Tabulate system-level summary
  summary1 <- combined %>%
    dplyr::group_by(vehicle_type) %>%
    dplyr::summarise(records = n(), exp_tons = sum(exp_tons, na.rm = TRUE),
      exp_value = sum(exp_value, na.rm = TRUE),
      annual_trucks = sum(annual_trucks)) %>%
    dplyr::mutate(avg_tons = round(exp_tons / annual_trucks, 2))
  print("Annual trucks generated:", quote = FALSE)
  print(summary1)

  # Exit stage left
  combined
}
