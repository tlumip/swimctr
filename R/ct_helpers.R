#' Print a ct message
#'
#' @param s Character string to print to console
#' @param header [Optional] string to print as a header
#'
#' Instead of R's normal print function we can use one that prints without quotes,
#' as well as extending it to print function headers in the output.
#'
#' @export
#'
ct_msg <- function(s = NULL, header = NULL) {
  # If the user asks us to print a section header in the output stream then put
  # an empty string (blank line) if the logfile is defined, and then put a
  # Markdown section header and title to default output stream
  if (!is.null(header)) {
    if (exists("logfile", inherits = TRUE)) {
      cat(' ', file = logfile, sep = '\n', append = TRUE)
    }
    print(paste("# ", header, " (", date(), ")", sep = ''), quote = FALSE)
  }

  # If the string is defined then print it now (even if header is also defined)
  if (!is.null(s)) {
    print(s, quote = FALSE)
  }

  # The results have already been written to output stream, so no need to return
  # anything to calling program
}

# Amazing that R doesn't have this function already, isn't it?
percent <- function(x, y = sum(x), places = 1) round((x/y)*100, places)
