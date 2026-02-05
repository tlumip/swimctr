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
  # every combination of alpha zone and PECAS sector
  nonzero <- zonal_data %>%
    gather("Activity", "employees", -Azone, -Total) %>%
    filter(employees > 0)

  # The industry equivalency table maps the PECAS industry categories to trip
  # generation categories. We'll use the latter for our modeling, so that is
  # the field we need to carry forward.
  nonzero <- left_join(nonzero, sector_equivalencies, by = "Activity") %>%
    group_by(Azone, category) %>%
    summarise(employees = sum(employees))

  # Next we'll append the county the alpha zone falls within, from which we can
  # can add FAF region
  add_counties <- alpha2beta %>%
    mutate(fipscode = as.numeric(STATEFIPS) * 1000 + as.numeric(COUNTYFIPS)) %>%
    select(Azone, STATEFIPS, fipscode)

  # Assign each to its enclosing FAF5 region
  add_faf_regions <- mutate(add_counties, faf_region = case_when(
    # Counties in the Portland MSA (FAF region 411)
    fipscode %in% c(41003, 41005, 41009, 41043, 41047, 41051, 41053, 41067,
      41071) ~ 411,
    # Counties in the Washington portion of the Portland MSA (FAF region 532)
    fipscode %in% c(53011, 53015, 53059) ~ 532,
    # The remaining counties in Oregon
    STATEFIPS == 41 ~ 419,
    # And everything else in the halo collar of zones
    TRUE ~ NA_real_))

  # Merge the FAF region and FIPS code for each alpha zone to the list of
  # employees by alpha zone and CT employment category
  pseudo_firms <- left_join(nonzero, add_faf_regions, by = "Azone") %>%
    ungroup() %>%
    mutate(firm_ID = row_number())

  # Return the data table with synthetic firms
  print(paste(nrow(pseudo_firms), "firms with", sum(pseudo_firms$employees),
    "employees in", length(unique(pseudo_firms$category)), "categories created"),
    quote = FALSE)
  return(pseudo_firms)
}
