#' Closest
#'
#' @description
#' Pick the closest value from a range in vector.
#'
#'
#' @param mdose estimated dose in the patient
#' @param vec vector to search for closest value. If NULL, provide str which vec should be created from
#' @param str strength of the tablet
#' @param split whether the tablet can be split
#'
#'
#' @return Returns the dose closest to the estimated
#' @export
#'
#'  @examples
#'  closest(25, str=50, split=F)
#'  closest(50, c(100,200,300))
#'
#'

closest <- function(mdose, vec=NULL, str, split=T) {
  if(is.na(mdose)) {
    return(mdose)
  } else {
    if(is.null(vec)){
    if(split) {
      vec <- str * c(0.5, seq(1,6))
    } else {
      vec <- str * seq(1, 6)
    }
    }
  }
  vec[which.min(abs(mdose-vec))]
}

