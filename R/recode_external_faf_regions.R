#' Recode FAF regions beyond the SWIM halo to SWIM external zones
#'
#' @param annual_faf_flows A tibble or name of file containing pre-
#'   processed annual FAF commodity flows by trucks for the desired year
#' @param external_equivalencies A tibble or name of file containing
#'   equivalencies between distal FAF regions and SWIM external stations
#' @param through_equivalencies A tibble or name of file containing
#'   equivalencies between distal FAF regions for both trip ends and the likely
#'   SWIM external zones they pass through
#' @param swim_faf_regions A list of FAF regions that are wholly contained
#'   within the SWIM modeled area
#'
#' @details This function replaces the domestic origin and destination
#'   (`dms_orig` and `dms_dest`) of FAF regions outside the SWIM modeled area
#'   with their corresponding SWIM external stations. In most cases two or more
#'   candidate routes will be available, each with an exogenously defined
#'   probability of being chosen based on user-supplied travel times between the
#'   centroids of the FAF regions. This function will proportionally assign
#'   commodity flows, in annual tons, between the FAF regions. Note that FAF
#'   reqions within the modeled area, defined in `swim_faf_regions` are not
#'   changed. They will be allocated to alpha zones later in the CT process
#'   after these flows have been transformed into annual truckload equivalents.
#'
#'   The `swim_faf_regions` correspond to FAF regions that are completely
#'   contained within the SWIM modeled area. This is currently defined as the
#'   Oregon counties within the Portland MSA (FAF region 411), the remaining
#'   counties in Oregon (FAF region 419), and the Washington counties within the
#'   Portland MSA (FAF region 532).
#'
#' @export
#' @examples
#' recode_to_externals <- recode_external_faf_regions(annual_faf_flows, 2025,
#'   external_equivalencies, through_equivalencies)

recode_external_faf_regions <- function(annual_faf_flows, external_equivalencies,
  through_equivalencies, swim_faf_regions = c(411, 419, 532)) {
  # Announce yourself
  print(swimctr:::self_identify(match.call()), quote = FALSE)

  # Filter the pre-processed FAF annual commodity flows for the target year and
  # drop the initial guesses at entry to and exit from Oregon
  faf <- select(annual_faf_flows, -entry, -exit)

  # Summarise the starting total tonnage by direction
  starting_totals <- group_by(faf, direction) %>%
    summarise(input_tons = sum(exp_tons))

  # Start by copying all records where both trip ends are within the SWIM
  # modeled area to `recombined` without any further processing.
  print(paste("Processing", nrow(filter(faf, direction == "internal")),
    "internal FAF flow records"), quote = FALSE)
  recombined <- filter(faf,
    dms_orig %in% swim_faf_regions, dms_dest %in% swim_faf_regions)

  # Recode the inbound (external-internal) flows, replacing the external FAF
  # region with its corresponding SWIM external station(s). Start by reading
  # the equivalencies file for external (IE, EI) flows.
  equiv <- swimctr:::load_tabular_data(external_equivalencies)

  # Generate a list of all origins outside of halo in the `dms_orig` field
  all_origins <- sort(unique(faf$dms_orig))
  all_origins <- all_origins[!all_origins %in% swim_faf_regions]
  print(paste("Processing", nrow(filter(faf, direction == "inbound")),
    "inbound FAF flow records"), quote = FALSE)

  # Make the substitutions
  for (this_origin in all_origins) {
    for (this_destination in swim_faf_regions) {
      # Grab the FAF records that match this trip end combination
      faf_subset <- filter(faf, dms_orig == this_origin, dms_dest == this_destination)
      if (nrow(faf_subset) == 0) next   # No matches found for this combo

      # Read the equivalencies the correspond to this trip end combo
      equiv_subset <- equiv %>%
        filter(dms_orig == this_origin, dms_dest == this_destination) %>%
        select(dms_orig, dms_dest, external_sta, gamma)
      n_equiv <- nrow(equiv_subset)
      if (n_equiv == 0) {
        stop(paste0("No equivalencies found for FAF pair ", this_origin,
          "-", this_destination))
      }

      # We now need to process each `equiv_subset` record in turn
      for (i in 1:nrow(faf_subset)) {
        zed <- faf_subset[i,]
        if (n_equiv == 1) {
          zed$dms_orig <- equiv_subset$external_sta[1]
        } else {   # Two or more possible external station traversals
          zed <- left_join(equiv_subset, zed, by = c("dms_orig", "dms_dest")) %>%
            mutate(exp_tons = exp_tons * gamma, exp_value = exp_value * gamma,
              exp_tmiles = exp_tmiles * gamma) %>%
            mutate(dms_orig = external_sta) %>%
            select(-external_sta, -gamma)
        }
        recombined <- bind_rows(recombined, zed)
      }
    }
  }

  # Next we'll tackle the outbound (internal-external) flows, replacing the
  # external FAF region with its corresponding SWIM external station(s). The
  # equivalencies file is only coded in one direction, so we'll need to swap the
  # `dms_orig` and `dms_dest` fields so that they're correct for this direction
  # of flow.
  opposite <- mutate(equiv, tmp = dms_orig, dms_orig = dms_dest, dms_dest = tmp)
  print(paste("Processing", nrow(filter(faf, direction == "outbound")),
    "outbound FAF flow records"), quote = FALSE)

  # Generate a list of all destinations outside of halo in the `dms_dest` field
  all_destinations <- sort(unique(faf$dms_dest))
  all_destinations <- all_destinations[!all_destinations %in% swim_faf_regions]

  # Make the external zone substitutions for the FAF destinations outside the halo
  for (this_origin in swim_faf_regions) {
    for (this_destination in all_destinations) {
      # Grab the FAF records that match this trip end combination
      faf_subset <- filter(faf, dms_orig == this_origin, dms_dest == this_destination)
      if (nrow(faf_subset) == 0) next   # No matches found for this combo

      # Read the equivalencies the correspond to this trip end combo from the
      # `opposite` direction equivalencies we just created above
      equiv_subset <- opposite %>%
        filter(dms_orig == this_origin, dms_dest == this_destination) %>%
        select(dms_orig, dms_dest, external_sta, gamma)
      n_equiv <- nrow(equiv_subset)
      if (n_equiv == 0) {
        stop(paste0("No equivalencies found for FAF pair ", this_origin,
          "-", this_destination))
      }

      # We now need to process each `equiv_subset` record in turn (this code chunk
      # unchanged from inbound case above)
      for (i in 1:nrow(faf_subset)) {
        zed <- faf_subset[i,]
        if (n_equiv == 1) {
          zed$dms_dest <- equiv_subset$external_sta[1]
        } else {   # Two or more possible external station traversals
          zed <- left_join(equiv_subset, zed, by = c("dms_orig", "dms_dest")) %>%
            mutate(exp_tons = exp_tons * gamma, exp_value = exp_value * gamma,
              exp_tmiles = exp_tmiles * gamma) %>%
            mutate(dms_dest = external_sta) %>%
            select(-external_sta, -gamma)
        }
        recombined <- bind_rows(recombined, zed)
      }
    }
  }

  # Finally, handle the through trips. In this case we will read a different set
  # of zonal equivalencies. And because they are also only coded one way we'll
  # this time add the opposite direction to the same tibble so that we only need
  # to go through this process once. So, read the external station equivalencies
  # for through trips.
  equiv <- swimctr:::load_tabular_data(through_equivalencies)
  opposite <- mutate(equiv, tmp = dms_orig, dms_orig = dms_dest, dms_dest = tmp,
    tmp = entry, entry = exit, exit = tmp)
  throughs <- bind_rows(equiv, opposite)
  print(paste("Processing", nrow(filter(faf, direction == "through")),
    "through FAF flow records"), quote = FALSE)

  # A through trip passes through Oregon without stopping. They will have origin
  # within `all_origins` and destinations within `all_destinations`. That is,
  # neither trip end will be within `swim_faf_region`. So, repeat the same
  # process used above for inbound and outbound trips, with one important twist
  # noted below.
  for (this_origin in all_origins) {
    for (this_destination in all_destinations) {
      # Grab the FAF records that match this trip end combination. Note that in
      # most cases there will not be a match, as most of the FAF flows in FHWA
      # database never touch Oregon.
      faf_subset <- filter(faf, dms_orig == this_origin, dms_dest == this_destination)
      if (nrow(faf_subset) == 0) next   # No matches found for this combo

      # Read the equivalencies that correspond to this trip end combo from the
      # `opposite` direction equivalencies we just created above
      equiv_subset <- throughs %>%
        filter(dms_orig == this_origin, dms_dest == this_destination) %>%
        select(dms_orig, dms_dest, entry, exit, gamma)
      n_equiv <- nrow(equiv_subset)
      if (n_equiv == 0) {
        print(paste0("No through equivalencies found for FAF pair ", this_origin,
          "-", this_destination), quote = FALSE)
        next
      }

      # We now need to process each `equiv_subset` record in turn (this code
      # chunk unchanged from inbound case above)
      for (i in 1:nrow(faf_subset)) {
        zed <- faf_subset[i,]
        if (n_equiv == 1) {
          zed$dms_orig <- equiv_subset$entry[1]
          zed$dms_dest <- equiv_subset$exit[1]
        } else {   # Two or more possible external station traversals
          zed <- left_join(equiv_subset, zed, by = c("dms_orig", "dms_dest")) %>%
            mutate(exp_tons = exp_tons * gamma, exp_value = exp_value * gamma,
              exp_tmiles = exp_tmiles * gamma) %>%
            mutate(dms_orig = entry, dms_dest = exit) %>%
            select(-gamma, -entry, -exit)
        }

        # Some of the routes that compete with those through Oregon go through
        # adjacent states. These are coded with zeroes for `dms_orig` and
        # `dms_dest` in the `faf_through_equivalencies` file. We will jettison
        # those outside routes before writing the values to `recombined`.
        drop_external_routes <- filter(zed, dms_orig > 0, dms_dest > 0)
        recombined <- bind_rows(recombined, drop_external_routes)
      }
    }
  }

  # Show us the total expanded tons in the `recombined` tibble, which should
  # match in the inputs
  ending_totals <- group_by(recombined, direction) %>%
    summarise(output_tons = round(sum(exp_tons))) %>%
    full_join(starting_totals, ., by = "direction") %>%
    mutate(difference = output_tons - input_tons,
      pct_diff = swimctr::percent((output_tons - input_tons), input_tons))
  print(paste(nrow(recombined), "records written:"), quote = FALSE)
  print(ending_totals)

  # Return the `recombined` tibble
  return(recombined)
}
