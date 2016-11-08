#' Sample daily FAF truck trips from annual FAF flow database
#' 
#' @param annual_faf_trucks Data frame containing reformatted FAF commodity flow
#'   database with estimate of total annual truckload equivalents appended
#' @param external_scaling_factor Asserted or estimating factor to account for
#'   FAF commodity flows that grouped into single truck movements (default is
#'   1.0, which assumes no such factoring is carried out
#' @param weeks_per_year The number of weeks per year the annual flows are 
#'   assumed to be spread across (default = 50, which is less than full year,
#'   to account for reduced flows on national holidays)
#' @param target_week The week of the year that the simulated flows will be
#'   extracted for (default = 24, which is near end of April)
#' @param target_day The day of the week the flows are sampled for (default = 4,
#'   which corresponds to Wednesday)
#' @param cluster_log The filename of a file used to store output from the
#'   doParallel cluster (optional)
#' @param save_to File name for saving the daily FAF truck trip records in
#'   comma-separated value format (optional)
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

sample_daily_faf_truckloads <- function(annual_faf_trucks,
  external_scaling_factor = 1.0, weeks_per_year = 50, target_week = 24,
  target_day = 4, cluster_log = "cluster.log", save_to = NULL) {

  ct_msg(header = "Sample daily FAF truckloads from annual flows")
  
  # If we are scaling the external trips then apply the factor to affected trips
  if (external_scaling_factor!=1.0) {
    ct_msg(paste("Scaling factor to correct FAF external equivalencies=",
      external_scaling_factor))
    annual_faf_trucks$annual_trucks <-
      ifelse(annual_faf_trucks$direction!="Internal",
        round(annual_faf_trucks$annual_trucks*external_scaling_factor, 0),
        annual_faf_trucks$annual_trucks)
  }
  
  # Tell us what our targets are (mostly for debugging and development use)
  annual_target <- round(sum(annual_faf_trucks$annual_trucks), 0)
  weekly_target <- round(sum(annual_faf_trucks$annual_trucks)/weeks_per_year, 0)
  daily_target <- round(weekly_target/7.0, 0)
  ct_msg(paste("Targets: annual=", annual_target, "weekly=", weekly_target,
    "daily=", daily_target))
  
  # Build a function to sample the week and day for each annual truck trip.
  # Trips occurring on the sampled week and day are returned as fully-
  # attributed daily truck trip data table. This is pretty time-consuming if run
  # run monolithically, so using with parallel() or similar is a practical
  # necessity.
  sample_annual_truck_trips <- function(replicant, i) {
    # We will refer to number of trucks in this replicant several times
    trucks_in_replicant <- replicant$annual_trucks
    replicant$sequence <- i
    
    # Grab the probabilities associated with each trip
    draws <- dplyr::data_frame(sequence = i,
      week = sample(1:weeks_per_year, replicant$annual_trucks, replace=TRUE),
      day = sample(1:7, replicant$annual_trucks, replace=TRUE)
    )
    
    # Do any of the trips occur within our target week and day? If so append
    # the important details from the replicant record.
    keep <- dplyr::filter(draws, week==target_week, day==target_day)
    trucks <- nrow(keep)
    # Following print statement only required if doing extended debugging
    #print(paste("replicant", i, "draws=", nrow(draws), "trucks=", trucks))
    if (trucks>0) {
      keep <- keep %>%
        dplyr::mutate(value = round(replicant$value/trucks_in_replicant, 0),
          tons = round(replicant$tons/trucks_in_replicant, 1)) %>%
        dplyr::select(-week, -day) %>%
        dplyr::full_join(dplyr::select(replicant, -tons, -value, -annual_trucks,
          -zed), by="sequence") %>%
        dplyr::select(-sequence)
    } else {
      keep <- dplyr::data_frame() # Delete the contents if no trucks generated
    }
    
    # Return the results
    keep
  }
  
  # Build a database of daily truck trips
  simulation_start <- proc.time()
  numberOfCores <- detectCores()
  ct_msg(paste("Number of cores detected=", numberOfCores))
  my_cluster <- makeCluster(numberOfCores, outfile = cluster_log)
  registerDoParallel(my_cluster)
  combined <- foreach(i=1:nrow(annual_faf_trucks), .packages=c("dplyr")) %dopar%
    sample_annual_truck_trips(annual_faf_trucks[i,], i)
  recombined <- dplyr::bind_rows(combined)  # Convert back to data frame
  stopCluster(my_cluster)
  simulation_stop <- proc.time()
  elapsed_seconds <- round((simulation_stop-simulation_start)[["elapsed"]], 1)
  convergence <- round((nrow(recombined)/daily_target), 3)
  ct_msg(paste("Simulation time=", elapsed_seconds, "seconds, convergence=",
    convergence))
  ct_msg(paste(nrow(recombined), "daily truck records generated from weekly",
    "samples"))
    
  # If the user has asked to save intermediate output then do so now
  if (!is.null(save_to)) readr::write_csv(recombined, save_to)
  
  # Give us a brief summary and then head for the beach
  ct_msg("Daily truck trips generated by direction and type:")
  ct_msg(addmargins(xtabs(~direction+vehicle_type, data=recombined)))
  recombined
}
