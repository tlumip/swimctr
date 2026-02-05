#' Read tabular data from an ambiguous data source
#'
#' @param data_object A fully-qualified path for a file or a tibble previously
#'   created and visible within the current environment
#' @param sep A character that delimits the values in an external file. Only
#'   applicable if a filename is passed to the function, and defaults to a
#'   comma.
#' @param comment A leading character that identifies the information on a
#'   record as a comment. Only applicable if a filename is passed to the
#'  function, and defaults to '#'.
#' @param verbose Prints a listing of the variables, class, number of unique
#'   values, and range of values for the tibble if TRUE. Defaults to FALSE.
#'
#' @details This function reads, and optionally lists a summary of, tabular data
#'   from an ambiguous source. That is, the data object can either be passed in
#'   a tibble or string name. If the latter the file's contents are read into
#'   the tibble and returned to the calling program. Thus, the user can pass
#'   either without having to change the code the calls this function. Moreover,
#'   different delimiters can be used without having to convert the source data
#'   into CSV (e.g., semi-colon or tab).
#'
#' @examples
#' this_tibble <- load_tabular_data(another_tibble)  # Does nothing
#' this_tibble <- load_tabular_data("./myfile.csv.xz", verbose = TRUE)

load_tabular_data <- function(data_object, sep = ',', comment = '#',
  verbose = FALSE) {

  # Determine what class the data object belongs to
  data_class <- class(data_object)[1]
  data_identifier <- ifelse(data_class == "character", data_object,
    deparse(substitute(data_object)))

  # If the data object is already a tibble simply return it
  if (data_class %in% c("tbl_df", "data.frame", "data.table", "spec_tbl_df")) {
    if (verbose == TRUE) print("data_object is a tibble or data frame")
  } else if (data_class == "character") {
    # Assume a string is a fully-qualified path name to a CSV file
    if (file.exists(data_object)) {
      data_object <- readr::read_delim(data_object, guess_max = 1e6, delim = sep,
        comment = comment, show_col_types = FALSE, trim_ws = TRUE)
      print(paste(nrow(data_object), "records with",
        length(colnames(data_object)), "fields loaded from", data_identifier),
        quote = FALSE)
    } else {
      error_msg <- paste0("File ", data_object, " does not exist")
      crash_signal <<- TRUE
    }
  } else {
    # Otherwise it is some type of data object other than a tibble or file with
    # those contents
    print(paste0("DEBUG: data_object=", data_object, " data_class=", data_class,
      " data_identifier=", data_identifier), quote = FALSE)
    error_msg <- paste0("Unable to process ", data_object, ": unknown class ",
      data_class)
    crash_signal <<- TRUE
    stop(error_msg)
  }

  if (verbose == TRUE) print(swimctr:::list_tibble_contents(data_object))

  return(data_object)
}
