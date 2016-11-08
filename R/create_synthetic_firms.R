#' Create synthetic pseudo-firms 
#' 
#' @param zonal_data Data frame containing traffic analysis zone attributes, to
#'   include employment by sector
#' @param sector_equivalencies Data frame containing PECAS sectors and their
#'   corresponding firm categories (groups of sectors) used in trip generation
#' @param alpha2beta Data frame containing, among other things, the county and
#'   Freight Analysis Framework (FAF) region that each zone lies within
#' @param save_to File name for saving the pseudo-firms produced by this 
#'   function (optional)
#'
#' @details This function maps employment by PECAS sector within each alpha zone
#'   into separate pseudo-firms, which are unit of analyses for remainder of CT
#'   simulation. 
#'   
#' @export
#' @examples
#' zones <- create_synthetic_firms(zonal_data, sector_equivalencies, alpha2beta,
#'   "synthetic-firms.csv")

create_synthetic_firms <- function(zonal_data, sector_equivalencies, alpha2beta,
  save_to = NULL) {

  ct_msg(header = "Create synthetic firms from zonal data")
  
  # Start by converting from wide to tall format, so that we have a record for
  # every alpha zone+sector pair
  nonzero <- zonal_data %>%
    tidyr::gather("sector", "employees", -Azone, -Total) %>%
    dplyr::rename(taz = Azone) %>%
    dplyr::filter(employees>0)

  # The industry equivalency table maps the AA industry categories to trip
  # generation categories. We'll use the latter for our modeling, so that is 
  # the field we need to carry forward.
  nonzero <- dplyr::left_join(nonzero, sector_equivalencies, by = "sector")
  
  # Next we'll append the county the alpha zone falls within, from which we can
  # can add FAF region
  counties <- alpha2beta %>%
    dplyr::filter(STATEFIPS==41) %>%
    dplyr::mutate(taz = Azone,
      fipscode = as.numeric(STATEFIPS)*1000+as.numeric(COUNTYFIPS)) %>%
    dplyr::select(taz, fipscode)
  
  # Assign each to FAF3 region. Clackamas, Columbia, Multnomah, Washington, and
  # Yamhill counties are in the Portland MSA, while the rest are in the Oregon
  # remainder FAF region.
  Portland_MSA <- c(41005, 41009, 41051, 41067, 41071)
  counties$faf_region <- ifelse(counties$fipscode %in% Portland_MSA, 411, 419)
  
  # Finally, merge county and FAF region to alphas. We'll quietly drop alpha
  # zones outside of Oregon since the key field must exist in both tables.
  pseudo_firms <- dplyr::left_join(nonzero, counties, by="taz")
  pseudo_firms$firmID <- seq(1, nrow(pseudo_firms))
  ct_msg(paste(max(pseudo_firms$firmID), "synthetic firms created"))
  
  # Drop the fields we no longer need to drag along, and change the class of 
  # fields that readr choose poorly about
  final_firms <- pseudo_firms %>%
    dplyr::mutate(taz = as.integer(taz), fipscode = as.integer(fipscode),
      faf_region = as.integer(faf_region), employees = as.integer(employees)) %>%
    dplyr::select(firmID, taz, fipscode, faf_region, sector, category,
      employees)

  # If the user has specified a filename to save intermediate results to do so
  if (!is.null(save_to)) readr::write_csv(final_firms, save_to)
  
  # Return the data table with synthetic firms
  ct_msg(paste(nrow(final_firms), "firms with", sum(final_firms$employees),
    "employees in", length(unique(final_firms$sector)), "sectors created"))
  final_firms
}
