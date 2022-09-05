#' Sample daily truck trips from annual FAF flow database
#'
#' @param annual_faf_trucks A tibble containing the annual truckload equivalents
#'   (discrete truck records) generated from the FAF commodity flow database
#' @param target_day The day of the week the flows are sampled for (default = 4,
#'   which corresponds to Wednesday)
#'
#' @details This function samples daily truck trips from FAF annual truckload
#'   equivalents. It samples for a designated day of the week in the first week
#'   of the year, as the current version does not include monthly or weekly flow
#'   percentages by SCTG or other variables. A tibble is returned with the
#'   sampled daily truck trips.
#'
#' @export
#' @examples
#' daily_trucks <- sample_daily_faf_truckloads(annual_faf_truckloads, 5)


sample_daily_faf_truckloads <- function(annual_faf_trucks, target_day = 4) {
  # Announce yourself
  message(swimctr:::self_identify(match.call()))

  # First randomly assign a week of the year to each of the annual trucks, and
  # then drop for all weeks except for one. Since we are not using different
  # factors for each week we'll just pick data from the first week.
  weekly_trucks <- annual_faf_trucks %>%
    mutate(week = sample(1:52, nrow(.), replace = TRUE)) %>%
    filter(week == 1)
  percent_sampled <- swimctr::percent(nrow(weekly_trucks), nrow(annual_faf_trucks))
  print(paste0(nrow(weekly_trucks), " weekly trucks sampled (", percent_sampled,
    "% of ", nrow(annual_faf_trucks), " annual truckload equivalents)"),
    quote = FALSE)

  # Then assign a day of the week randomly, and keep only those trucks that
  # are assigned to that day
  daily_trucks <- weekly_trucks %>%
    mutate(day = sample(1:7, nrow(.), replace = TRUE)) %>%
    filter(day == target_day) %>%
    select(-week, -day)
  percent_sampled <- swimctr::percent(nrow(daily_trucks), nrow(weekly_trucks))
  print(paste0(nrow(daily_trucks), " daily trucks sampled (", percent_sampled,
    "% of ", nrow(weekly_trucks), " weekly truckload equivalents)"),
    quote = FALSE)

  # Tell us about the results
  years_included <- paste0(sort(unique(daily_trucks$year)), collapse = "+")
  print(paste("Total", years_included, "daily truck trips sampled:"),
    quote = FALSE)
  print(addmargins(xtabs(~distance_band + truck_type, data = daily_trucks,
  	na.action = na.pass, exclude = NULL)))

  # Head for the beach
  return(daily_trucks)
}
