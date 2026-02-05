#' Return the identity of the calling function and date string
#'
#' @param mc The results of a match.call() function call from the calling
#'   program
#'
#' @details This function returns a string that identifies the calling 
#'   function and the time it was called. This is used to announce the start of
#'   each CT function in the logging output. You wouldn't think you'd have to
#'   go to this much work, but `match.call()` will return three identifying 
#'   strings if the function name is preceded by its package name--but of 
#'   course not in order: the double-colon comes first, then the package name,
#'   and finally the function. Unless there is no prefix, in which case the
#'   first element of the return value is what we're after. We try to handle
#'   both cases here.
#'
#' @return The function returns a string with the calling function's name and
#'   the current date and time.
#'
#' @examples
#' print(self_identify(match.call())

self_identify <- function(mc) {
  if (length(mc[[1]]) == 3) {
    result <- paste0(mc[[1]][[2]], "::", mc[[1]][[3]], "()")
  } else {
    result <- mc
  }
  paste0(result, " started on ", date())
}
