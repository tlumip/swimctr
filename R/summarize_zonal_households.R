#' Summarize number of households per traffic analysis zone
#'  
#' @param zonal_data A tibble containing zonal population and employment
#'   estimates at the traffic analysis zone level
#'
#' @details This simple function tallies the number of households within each
#'   traffic analysis zone within the modeled area. These totals are included in
#'   the calculation of local truck attraction proportions. The result is a data
#'   frame containing this information, which can optionally be also stored in a
#'   comma-separated value file if desired.  
#'   
#' @export
#' @examples
#' total_households <- summarize_zonal_households(zonal_data)

summarize_zonal_households <- function(zonal_data) {
  # Announce yourself
  print(swimctr:::self_identify(match.call()), quote = FALSE)

  # We might eventually save households by income range, or area type, but for
  # now just sum them into single total value
  households <- zonal_data %>%
    select(TAZ, as.numeric(TotalHHs)) %>%
    rename(taz = TAZ, total_households = TotalHHs)
  
  # Return the results
  print(paste(sum(households$total_households), "total households found in",
    length(unique(households$taz)), "zones"), quote = FALSE)
  return(households)
}
