#' Format numeric vectors
#'
#'
#' @param numbers numeric value or vector for formatting
#' @param digits number of digits
#' @param nsmall number of zero-digits
#' @param ama whether the numbers should be printed according to AMA guidelines (no digits on values >= 10). Default = F.
#'
#' @return returns a vector of same lentgh with formatted digits
#' @export
#'
#' @examples
#' numbR(c(5,2,4,10,100, 41.2), ama=F)
#' numbR(c(5,2,4,10,100, 41.2), ama=T)
#'

numbR <- function(numbers, digits = 1, nsmall, ama = F) {
  if(missing(nsmall)) {
    nsmall <- digits
  }

  vals <- format(round(numbers, digits), nsmall = nsmall)



  if(ama) {

    vals <- ifelse(as.numeric(str_extract(vals, "\\d+\\.\\d*")) >= 10, str_replace(vals, "\\d{2,}\\.\\d*", as.character(round(as.numeric(str_extract(vals, "\\d{2,}\\.\\d*")),0))), vals)

  }

  vals

}
