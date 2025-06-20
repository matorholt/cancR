#' ClosR
#'
#' @description
#' Pick the closest value from a range in vector.
#'
#'
#' @param x Input number
#' @param vec vector to search for closest value. If NULL, provide str which vec should be created from
#' @param str Multiplier for the generic vector
#' @param split whether the tablet can be split
#'
#'
#' @return Returns the dose closest to the estimated
#' @export
#'
#'
#'
#'

closR <- function(x, vec=NULL, str, split=T) {
  if(is.na(x)) {
    return(x)
  } else {
    if(is.null(vec)){
    if(split) {
      vec <- str * c(0.5, seq(1,6))
    } else {
      vec <- str * seq(1, 6)
    }
    }
  }
  vec[which.min(abs(x-vec))]
}

