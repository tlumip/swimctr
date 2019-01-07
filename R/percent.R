#' Calculate the percent value of a vector (stunning this isn't built-in to R)
#'
#' @param x Value or vector of number to calculate as percentage of y
#' @param y The total to calculate percent x from, by default the sum of the
#'   value(s) of x
#' @param places Number of places after the decimal to round the result to
#'   (default value is 1)
#'
#' @details This function calculates percentages, generally on values in data
#'   frame field. If you'd like the values to be normalized simply divide the
#'   result by 100. To use this function to calculate percent change simply
#'   code the current or future value as x and prior value to y as x-prior.
#'
#' @export
#' @examples
#' df$pct_varname <- percent(df$varname)   # Simple percentage
#' df$pct_varname <- percent(df$varname)/100.0    # Normalized value
#' df$pct_change <- percent(df$current, (df$current - df$prior), 3)  # % change

percent <- function(x, y = sum(x), places = 1) round((x/y)*100, places)
