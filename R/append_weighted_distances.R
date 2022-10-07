#' Added weighted distances to FAF 5.x+ data without it
#'
#' @param fhwa_db A FAF regional database
#' @param distances A tibble containing intra-regional weighted distances
#'   (`wgt_dist`) by `dms_orig` and `dms_dest` pairs
#' @param internal_regions A list of FAF regions within the SWIM2 modeled area
#'   (defaults to FAF regions 411 [Portland] and 419 [rest of Oregon])
#' @param ignore_regions A list of FAF regions that will be ignored if weighted
#'   distances are not found in the data (defaults to currently defined FAF 
#'   regions in Hawaii, which are not reachable by surface transport modes)
#'
#' @details This appends weighted distances from an exogenously produced file of
#'   FAF region interchanges. The `distances` tibble or file is assumed to 
#'   contain only those fields (i.e., `dms_orig`, `dms_dest`, `wgt_dist`). This
#'   tibble is joined to the FAF regional database records, and the resulting
#'   tibble (the original FAF regional database plus the new `wgt_dist` field)
#'   saved for later use. If an interchange is present in `fhwa_db` that is not
#'   found in `distances` the program crashes. In that case the user will need to
#'   add the missing interchanges to `distances` before continuing.
#'
#' @export
#' @examples
#' add_distances <- append_weighted_distances("./faf5.3.csv.xz",
#'   "./faf451_distances.csv")

append_weighted_distances <- function(fhwa_db, distances,
  internal_regions = c(411, 419), ignore_regions = c(151, 159)) {
  # Identify the current function
  message(swimctr:::self_identify(match.call()))

  # Read the data from files if they are not already in tibbles. We don't care
  # whether the origin and destination are character or integers, but since we
  # cannot be sure how `read_csv` will guess at their classes we'll be explicit
  fhwa_db <- swimctr:::load_tabular_data(fhwa_db) %>%
    mutate(dms_orig = as.integer(dms_orig), dms_dest = as.integer(dms_dest))
  distances <- swimctr:::load_tabular_data(distances) %>%
    mutate(dms_orig = as.integer(dms_orig), dms_dest = as.integer(dms_dest))
  combined <- left_join(fhwa_db, distances, by = c("dms_orig", "dms_dest"))

  # Check that we don't have missing values for distance for interchanges to
  # or from our modeled area
  relevant <- filter(combined, is.na(wgt_dist),
    dms_orig %in% internal_regions | dms_dest %in% internal_regions)
  problems <- filter(relevant,
    !(dms_orig %in% ignore_regions | dms_dest %in% ignore_regions))

  # Now find the remaining disconnects between FAF regions within the modeled
  # area and other FAF regions within the continental USA
  if (nrow(problems) > 0) {
    # Finger the missing interregional interchanges but exclude Hawaii
    problems <- mutate(problems, pair = paste0(dms_orig, '-', dms_dest))
    eh <- sort(unique(problems$pair))
    print("Missing weighted distances for FAF interchanges:", quote = FALSE)
    print(eh)
    stop(paste("Missing weighted distances for", length(eh), "FAF interchanges"))
  }

  # Otherwise export the combined data set and exit stage left
  return(combined)
}