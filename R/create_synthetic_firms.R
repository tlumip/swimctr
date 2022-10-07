#' Create synthetic pseudo-firms 
#' 
#' @param zonal_data A tibble containing traffic analysis zone attributes, to
#'   include employment by sector
#' @param sector_equivalencies A tibble containing PECAS sectors and their
#'   corresponding firm categories (groups of sectors) used in trip generation
#' @param alpha2beta A tibble containing, among other things, the county and
#'   Freight Analysis Framework (FAF) region that each zone lies within
#'
#' @details This function maps employment by PECAS sector within each alpha zone
#'   into separate pseudo-firms, which are unit of analyses for remainder of CT
#'   simulation. 
#'   
#' @export
#' @examples
#' zones <- create_synthetic_firms(zonal_data, sector_equivalencies, alpha2beta)

create_synthetic_firms <- function(zonal_data, sector_equivalencies,
  alpha2beta) {
  # Introduce yourself
  print(swimctr:::self_identify(match.call()), quote = FALSE)
  
  # Start by converting from wide to tall format, so that we have a record for
  # every alpha zone+sector pair
  nonzero <- zonal_data %>%
    gather("sector", "employees", -Azone, -Total) %>%
    rename(taz = Azone) %>%
    filter(employees > 0)

  # The industry equivalency table maps the AA industry categories to trip
  # generation categories. We'll use the latter for our modeling, so that is 
  # the field we need to carry forward.
  nonzero <- left_join(nonzero, sector_equivalencies, by = "sector")
  
  # Next we'll append the county the alpha zone falls within, from which we can
  # can add FAF region
  counties <- alpha2beta %>%
    filter(STATEFIPS == 41) %>%
    mutate(taz = Azone,
      fipscode = as.numeric(STATEFIPS) * 1000 + as.numeric(COUNTYFIPS)) %>%
    select(taz, fipscode)
  
  # Assign each to FAF3 region. Clackamas, Columbia, Multnomah, Washington, and
  # Yamhill counties are in the Portland MSA, while the rest are in the Oregon
  # remainder FAF region.
  Portland_MSA <- c(41005, 41009, 41051, 41067, 41071)
  counties$faf_region <- ifelse(counties$fipscode %in% Portland_MSA, 411, 419)
  
  # Finally, merge county and FAF region to alphas. We'll quietly drop alpha
  # zones outside of Oregon since the key field must exist in both tables.
  pseudo_firms <- left_join(nonzero, counties, by="taz")
  pseudo_firms$firmID <- seq(1, nrow(pseudo_firms))
  print(paste(max(pseudo_firms$firmID), "synthetic firms created"),
    quote = FALSE)
  
  # Drop the fields we no longer need to drag along, and change the class of 
  # fields that readr choose poorly about
  final_firms <- pseudo_firms %>%
    mutate(taz = as.integer(taz), fipscode = as.integer(fipscode),
      faf_region = as.integer(faf_region), employees = as.integer(employees)) %>%
    select(firmID, taz, fipscode, faf_region, sector, category, employees)

  # Return the data table with synthetic firms
  print(paste(nrow(final_firms), "firms with", sum(final_firms$employees),
    "employees in", length(unique(final_firms$sector)), "sectors created"),
    quote = FALSE)
  return(final_firms)
}
