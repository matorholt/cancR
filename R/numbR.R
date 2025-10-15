#' Format numeric vectors
#'
#'
#' @param numbers numeric value or vector for formatting
#' @param digits number of digits
#' @param nsmall number of zero-digits
#'
#' @return returns a vector of same lentgh with formatted digits
#' @export
#'
#'

numbR <- function(numbers, digits = 1, nsmall) {
  if(missing(nsmall)) {
    nsmall <- digits
  }
  format(round(numbers, digits), nsmall = nsmall)
}
