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
#' @param this_year The four-digit calendar year currently being simulated
#' @param cluster_log The filename of a file used to store output from the
#'   doParallel cluster (optional)
#' @param save_to File name for saving the trip records with destinations 
#'   appended in comma-separated value format (optional)
#'
#' @details This function appends annual truckload equivalents to the FHWA
#'   Freight Analysis Framework (FAF) commodity flow forecast database. The
#'   process is based upon the FAF3 Traffic Analysis report developed by 
#'   Battelle, which maps tonnage to truckload equivalents, including empty
#'   trucks. The function requires reformatted FAF data that replaces several
#'   numeric values with strings that are more intuitive, adds the distance
#'   traveled between FAF region centroids to each record, and converts the data
#'   from the wide format FHWA distributes it in to tall format, with separate
#'   record for each foreast year for each origin-destination-mode-commodity
#'   combination. The function also requires three tables from Chapter 3 of the
#'   FAF Traffic Analysis report, and then follows their methodology exactly 
#'   (but without the errors in the report). Because the SWIM simulation runs
#'   through time the data are mapped to the closest FAF forecast year for each
#'   year in the simulation. The function returns an updated FAF database with
#'   truckload equivalents appended to each record.  
#'   
#' @export
#' @examples
#' faf_annual_trucks <- create_annual_faf_truckloads(faf_data,
#'   truck_allocation_factors, truck_equivalency_factors, empty_truck_factors,
#'   2020)  # Accepts the default for cluster log and does not save output

create_annual_faf_truckloads <- function(faf_data, truck_allocation_factors,
  truck_equivalency_factors, empty_truck_factors, this_year,
  cluster_log = "cluster.log", save_to = NULL) {
  
  # Find the closest year in the data associated with the SWIM2 target year, but
  # but only up to a certain point. If further away than allowed from one of the
  # the FAF forecast years then stop and complain.
  getClosestYear <- function(this_year, years_in_data, max_difference = 6) {
    # Find out how far away from the years in the FAF database this year is
    differences <- abs(years_in_data-this_year)
    closest_year <- which.min(differences)
    
    # If further apart than we can stomach then stop the simulation
    if (differences[closest_year]>max_difference) {
      error_message <- paste("Error: the closest year in the FAF database to",
        "simulation year of", this_year, "is", years_in_data[closest_year])
      stop(error_message)
    } else {
      print(paste("Simulation year of", this_year, "mapped to FAF year",
        years_in_data[closest_year]), quote=FALSE)
    }
    
    # Otherwise return the FAF year closest to the current year
    years_in_data[closest_year]
  }
  
  # Associate the current year in the simulation to the closest FAF year
  faf_years <- sort(as.integer(unique(faf_data$year)))
  use_faf_year <- getClosestYear(this_year, faf_years)
  # Keep only non-zero truck flows
  flows <- faf_data %>%
    dplyr::filter(dms_mode=="Truck", tons>0.0, year==use_faf_year)
  
  # Define a helper function to extract truck allocation factors (Table 3-3)
  # for the appropriate distance range. We will normalize the factors within the
  # chosen distance on the fly
  getTruckAllocationFactors <- function(distance) {
    truck_allocation_factors %>%
      dplyr::filter(distance>=minimum_range & distance<=maximum_range) %>%
      dplyr::mutate(alloc_factor=allocation_factor/sum(allocation_factor)) %>%
      dplyr::select(vehicle_type, alloc_factor)
  }
  
  # The truck equivalency factors found in Appendix A of the FAF3 Freight
  # Traffic Analysis report are used for converting kilotons into truckload
  # equivalents. Convert them from wide to tall format. Since we are dealing
  # with tons we need to scale the factors to account for the differences.
  truck_equivalency_factors <- truck_equivalency_factors %>%
    tidyr::gather(body_type, equiv_factor, auto:other)
  # We will also define a helper function to pull the appropriate values by
  # commodity and vehicle type.
  getTruckEquivalencyFactors <- function(commodity) {
    dplyr::filter(truck_equivalency_factors, sctg2==commodity, equiv_factor>0.0)
  }
  
  # Finally, convert the empty truck factors from wide to tall format, and
  # define a helper function to grab appropriate values by body type and flow
  # direction.
  empty_truck_factors <- empty_truck_factors %>%
    tidyr::gather(vehicle_type, empty_factor, SU:TPT)
  getEmptyTruckFactors <- function(flow_direction) {
    dplyr::filter(empty_truck_factors, crossing_type==flow_direction)
  }
  
  # Calculate truck equivalencies for each FAF flow record. The resulting table
  # will have flows by each vehicle (truck) type and status (empty, loaded)
  calcTruckloadEquivalencies <- function(replicant, i) {
    ## Grab record x from the data frame we are working on
    ##replicant <- flows[x,]
    
    # Get the factors appropriate for the current distance between domestic FAF
    # regions, trade type, and commodity
    taf <- getTruckAllocationFactors(replicant$distance)
    tef <- getTruckEquivalencyFactors(replicant$sctg2)
    trip_type <- ifelse(replicant$trade_type=="Domestic", "domestic", "border")
    etf <- getEmptyTruckFactors(trip_type)
    
    # Allocate the tonnage to each vehicle (as in Table 3-7)
    taf$tons <- replicant$tons*taf$alloc_factor
    
    # Merge the tonnage by vehicle type with the truck equivalency factors to 
    # get tonnage by vehicle type and body type. 
    loaded <- tef %>%
      dplyr::left_join(taf, by="vehicle_type") %>%
      dplyr::mutate(annual_trucks = tons*equiv_factor) %>%
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
      dplyr::mutate(empty_trucks = annual_trucks*empty_factor) %>%
      dplyr::select(-crossing_type, -empty_factor)
    
    # We needed to retain the body types to calculate empties, but now we can
    # collapse each group to vehicle types
    loaded <- loaded %>%
      dplyr::group_by(vehicle_type) %>%
      dplyr::summarise(annual_trucks = round(sum(annual_trucks),0)) %>%
      dplyr::mutate(status = "loaded")
    empty <- empty %>%
      dplyr::group_by(vehicle_type) %>%
      dplyr::summarise(annual_trucks = round(sum(empty_trucks),0)) %>%
      dplyr::mutate(status = "empty")
    
    # Append the original value and tons for it to operate upon
    loaded$tons <- replicant$tons
    loaded$value <- replicant$value
    
    # Create a separate record for the combined groups and merge with the data
    # from the flow record. Note that we drop the original value and tons, as
    # they are now split among the various loaded truck types.
    loaded <- loaded %>%
      dplyr::mutate(percent = annual_trucks/sum(annual_trucks),
        tons = percent*tons, value = as.numeric(percent*value)) %>%
      dplyr::select(-percent)
    empty <- dplyr::mutate(empty, tons=0.0, value=0.0)
    together <- dplyr::bind_rows(loaded, empty)
    
    # Add the SCTG code back in, as we will use that to merge the two tables
    together$sctg2 <- replicant$sctg2
    result <- together %>%
      dplyr::left_join(dplyr::select(replicant, -tons, -value), by = "sctg2") %>%
      dplyr::mutate(zed = zed) %>%
      dplyr::filter(annual_trucks>0)  # Drop empty trucks
    
    # We can turn on a print statement if required to debug this function, but
    # ordinarily we will skip it. If running in parallel the results will be 
    # written to cluster log, if one is defined, or to stdout otherwise
    #print(paste("replicant", i, "zed=", zed, "annual trucks=",
    #  sum(result$annual_trucks)), quote = FALSE)
    
    result
  }
  
  # Run the simulation
  simulation_start <- proc.time()
  numberOfCores <- detectCores()
  print(paste("Number of cores detected=", numberOfCores), quote=FALSE)
  this_cluster <- makeCluster(numberOfCores, outfile = cluster_log)
  registerDoParallel(this_cluster)
  results <- foreach(i=1:nrow(flows), .packages=c("dplyr")) %dopar%
    calcTruckloadEquivalencies(flows[i,], i)
  combined <- dplyr::bind_rows(results)
  stopCluster(this_cluster)
  simulation_stop <- proc.time()
  elapsed_seconds <- round((simulation_stop-simulation_start)[["elapsed"]], 1)
  print(paste("Simulation time=", elapsed_seconds, "seconds"), quote=FALSE)
  
  # Tabulate system-level summary
  summary1 <- combined %>%
    dplyr::group_by(vehicle_type) %>%
    dplyr::summarise(records = n(), tons = sum(tons, na.rm = TRUE),
      value = sum(value, na.rm = TRUE), annual_trucks = sum(annual_trucks)) %>%
    dplyr::mutate(avg_tons = round(tons/annual_trucks, 2))
  print("Annual trucks generated:", quote = FALSE)
  print(summary1)
  
  # Save the intermediate results if requested by the user, but otherwise give
  # brief report and exit stage left
  if (!is.null(save_to)) readr::write_csv(combined, save_to)
  combined
}
