#' Convert time and distance skim matrices in OMX format to data frame format
#' 
#' @param distance_skimsFN Fully-qualified filename of distance skim matrix in
#'   ZMX format
#' @param time_skimsFN Fully-qualified filename of travel time skim matrix in
#'   ZMX format
#' @param save_to Filename for saving the combined skim matrix file in comma-
#'   separated value format (optional)
#'
#' @details This function reads distance and travel time skim matrices stored in
#'   compressed matrix format (zmx) used by the SWIM system, combines them into
#'   a single data frame, and returns the result. The function does not assume
#'   that the dimensionality is the same, so as to not use column joins that 
#'   might not properly align. Instead, the more inefficient but safer method of
#'   matching the two by origin and destination zone numbers are used, with
#'   missing values inserted for when a zone pair appears in one file but not
#'   the other. The results can optionally be stored in a comma-separated value
#'   file if desired.  
#'   
#' @export
#' @examples
#' skim_matrices <- read_skim_matrices("pmautodist.zmx", "pmautotime.zmx",
#'   "skim_matrices.csv")

read_skim_matrices <- function(distance_skimsFN = NULL, time_skimsFN = NULL,
  save_to = NULL) {
  # Start by making sure that the user has specified both skim matrices
  if (is.null(distance_skimsFN) | is.null(time_skimsFN)) {
    error_message <- "read_skim_matrices called, but one or both inputs are null"
    stop(error_message)
  }
  
  # Read the distance matrix
  distance_skims <- omxr::read_zmx(distance_skimsFN)
  this_matrix_size <- object.size(distance_skims)
  print(paste("Reading ", distance_skimsFN, " (",
    round(this_matrix_size/(1024^2), 1), " MB)", sep = ''), quote = FALSE)
    
  # Then convert it to tall data frame format, combining the origin and 
  # destination fields to zone pair key on the fly
  distances <- omxr::long_matrix(distance_skims, value = "distance") %>%
    tidyr::unite(zone_pair, origin, destination, sep = '-')
  
  # Likewise, read the travel time matrix
  time_skims <- omxr::read_zmx(time_skimsFN) 
  this_matrix_size <- object.size(time_skims)
  print(paste("Reading ", time_skimsFN, " (", 
    round(this_matrix_size/(1024^2), 1), " MB)", sep = ''))
    
  # Then convert it to tall data frame format
  times <- omxr::long_matrix(time_skims, value = "travel_time") %>%
    tidyr::unite(zone_pair, origin, destination, sep = '-')
  
  # Every zone pair defined in one matrix should also be defined in the other,
  # but we should not blindly assume that.
  combined <- 
    dplyr::full_join(distances, times, by = "zone_pair") %>%
    tidyr::separate(zone_pair, c("origin", "destination"), sep = '-') %>%
    dplyr::mutate(origin = as.integer(origin),
      destination = as.integer(destination))
  
  # If the user has asked for intermediate output write it before returning
  # the combined skim matrix
  if (!is.null(save_to)) readr::write_csv(combined, save_to)
  combined
}