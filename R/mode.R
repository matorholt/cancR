#' Mode
#'
#' @description
#' Get the mode (most common value) of a vector.
#'
#'
#' @param x Vector of values
#' @param na.rm Whether NAs should be removed, defaults to TRUE
#'
#' @return The most common value of the vector excluding NAs
#' @export
#'
#' @examples
#' mode(c(1,1,2,3))
mode <- function(x, na.rm=T) {
  if(na.rm) {
    ux <- unique(x[!is.na(x)])
  } else {
    ux <- unique(x)
  }
  ux[which.max(tabulate(match(x, ux)))]
}
