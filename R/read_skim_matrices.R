#' Convert time and distance skim matrices in OMX format to data frame format
#'
#' @param distance_skimsFN Fully-qualified filename of distance skim matrix in
#'   ZMX format
#' @param time_skimsFN Fully-qualified filename of travel time skim matrix in
#'   ZMX format
#'
#' @details This function reads distance and travel time skim matrices stored
#'   in compressed matrix format (zmx) used by the SWIM system. The function
#'   does not assume that the dimensionality is the same, so as to not use
#'   column joins that might not properly align. Instead, the more inefficient
#'   but safer method of matching the two by origin and destination zone
#'   numbers are used, with missing values inserted for when a zone pair
#'   appears in one file but not the other.
#'
#' @returns A tibble containing both time and distance skims on each record.
#'
#' @export
#' @examples
#' skim_matrices <- read_skim_matrices("pmautodist.zmx", "pmautotime.zmx")

read_skim_matrices <- function(distance_skimsFN = NULL, time_skimsFN = NULL) {
  # Make sure that the user has specified both skim matrices
  print(swimctr:::self_identify(match.call()), quote = FALSE)
  if (is.null(distance_skimsFN) | is.null(time_skimsFN)) {
    stop("Both time and distance skims must be provided as inputs")
  }

  # Use Ben Stabler's method for reading ZMX files. I would normally just install
  # the package and use it from there, but I get errors when I try to clone it.
  # Fortunately the expedient method of copy and paste still works.
  # https://github.com/tlumip/ZMX/blob/master/ZMX.R
  readZipMat = function(fileName) {

    #define matrix
    rowCon = unz(fileName,"_rows")
    colCon = unz(fileName,"_columns")
    xRowNumCon = unz(fileName,"_external row numbers")
    xColNumCon = unz(fileName,"_external column numbers")
    nrows = as.integer(scan(rowCon, what="", quiet=T))
    ncols = as.integer(scan(colCon, what="", quiet=T))
    rowNames = strsplit(scan(xRowNumCon, what="", quiet=T),",")[[1]]
    colNames = strsplit(scan(xColNumCon, what="", quiet=T),",")[[1]]
    close(rowCon)
    close(colCon)
    close(xRowNumCon)
    close(xColNumCon)

    #create matrix
    outMat = matrix(0, nrows, ncols)
    rownames(outMat) = rowNames
    colnames(outMat) = colNames

    #read records
    zipEntries = paste("row_", 1:nrows, sep="")
    for(i in 1:nrows) {
      con = unz(fileName,zipEntries[i],"rb")
      outMat[i,] = readBin(con,what=double(),n=ncols, size=4, endian="big")
      close(con)
    }
    #return matrix
    return(outMat)
  }

  # And I'm likewise lifting ghosted code from Greg Macfarlane's oxmr package,
  # although you must roll back to a much earlier commit to find this gem. It
  # takes the matrix that the ZMX file gets dumped into and scarily fast turns it
  # into the tibble that I want.
  # https://github.com/gregmacfarlane/omxr/blob/8a1d88f21cf82d392c2bd9d90cbeaba8363b29b1/R/long_matrix.R
  gather_matrix <- function(matrix, value_name = NULL){
  	# if no value name given, default to "value"
  	if(is.null(value_name)) value_name <- "value"

    list(
      # set matrix row and column names if they exist
      origin = rownames(matrix)[row(matrix)] %||% row(matrix),
      destination = colnames(matrix)[col(matrix)] %||% col(matrix),
      value = matrix
    ) %>%
      purrr::map_dfc(as.vector) %>%
      dplyr::rename(!!value_name := value)
  }

  # Read the distance matrix
  distance_skims <- readZipMat(distance_skimsFN)
  this_matrix_size <- object.size(distance_skims)
  print(paste0("Reading ", distance_skimsFN, " (",
    round(this_matrix_size / (1024^2), 1), " MB)"), quote = FALSE)

  # Then convert it to tall data frame format, combining the origin and
  # destination fields to zone pair key on the fly
  distances <- gather_matrix(distance_skims, "distance") %>%
    tidyr::unite(zone_pair, origin, destination, sep = '-')

  # Likewise, read the travel time matrix
  time_skims <- readZipMat(time_skimsFN)
  this_matrix_size <- object.size(time_skims)
  print(paste0("Reading ", time_skimsFN, " (",
    round(this_matrix_size / (1024^2), 1), " MB)"), quote = FALSE)

  # Then convert it to tall data frame format
  times <- gather_matrix(time_skims, "travel_time") %>%
    tidyr::unite(zone_pair, origin, destination, sep = '-')

  # Every zone pair defined in one matrix should also be defined in the other,
  # but we should not blindly assume that.
  combined <-
    full_join(distances, times, by = "zone_pair") %>%
    tidyr::separate(zone_pair, c("origin", "destination"), sep = '-') %>%
    mutate(origin = as.integer(origin), destination = as.integer(destination))

  # Check to ensure that all intrazonal movements are defined
  all_origins <- tibble(origin = sort(unique(combined$origin)))
  missing_intrazonals <- combined %>%
    filter(origin == destination) %>%
    left_join(all_origins, by = "origin") %>%
    filter(is.na(distance) | is.na(travel_time))
  n_missing <- nrow(missing_intrazonals)
  if (n_missing > 0) {
    error_message <- paste("Missing intrazonal data for", n_missing, "zones")
    print(sort(unique(missing_intrazonals$origin)))
    stop(error_message)
  }

  # Exit stage right
  return(combined)
}
