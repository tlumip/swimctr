#' Summarize number of households per traffic analysis zone
#'  
#' @param zonal_data Data frame containing zonal population and employment
#'   estimates at the traffic analysis zone level
#' @param save_to File name for saving the total number of households by traffic
#'   analysis zone in comma-separated value format (optional)
#'
#' @details This simple function tallies the number of households within each
#'   traffic analysis zone within the modeled area. These totals are included in
#'   the calculation of local truck attraction proportions. The result is a data
#'   frame containing this information, which can optionally be also stored in a
#'   comma-separated value file if desired.  
#'   
#' @export
#' @examples
#' total_households <- summarize_zonal_households(zonal_data, "houeholds.csv")

summarize_zonal_households <- function(zonal_data, save_to = NULL) {

  ct_msg(header = "Summarize total households from zonal data")
  
  # We might eventually save households by income range, or area type, but for
  # now just sum them into single total value
  households <- zonal_data %>%
    dplyr::select(TAZ, as.numeric(TotalHHs)) %>%
    dplyr::rename(taz = TAZ, total_households = TotalHHs)
  
  # If the user has specified a filename to save intermediate results to do so
  if (!is.null(save_to)) readr::write_csv(households, save_to)
  
  # Return the results
  ct_msg(paste(sum(households$total_households), "total households found in",
    length(unique(households$taz)), "zones"))
  households
}
