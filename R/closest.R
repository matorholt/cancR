#' Closest
#'
#' @description
#' Pick the closest value from a range in vector.
#'
#'
#' @param mdose estimated dose in the patient
#' @param str strength of the tablet
#' @param split whether the tablet can be split
#'
#' @return Returns the dose closest to the estimated
#' @export
#'
#'
closest <- function(mdose, str, split=T) {
  if(is.na(mdose)) {
    return(mdose)
  } else {
    if(split) {
      vec <- str * c(0.5, seq(1,6))
    } else {
      vec <- str * seq(1, 6)
    }
  }
  vec[which.min(abs(mdose-vec))]
}
