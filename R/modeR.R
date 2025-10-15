#' Get the mode (most common value) of a vector.
#'
#'
#' @param x Vector of values
#' @param ties If two or more values are equally common, which should be chosen. Default is first.
#' @param na.rm Whether NAs should be removed, defaults to TRUE
#'
#' @return The most common value of the vector excluding NAs
#' @export
#'
# nums1 <- c(1,1,2,3)
# nums2 <- c(1,2,3,3)
# nums3 <- c(1,2,3)
# char1 <- c("first", "first", "middle", "last")
# char2 <- c("first", "first", "middle", "last", "last")
# char3 <- c("first", "middle", "last", "hepto")
# char4 <- c("first", "middle", "last")
#
# mode(nums1)
# mode(nums2)
# mode(nums3, "last")
# mode(char1, "last")
# mode(char2, "first")
# mode(char3, "last")


modeR <- function(x, ties = "first", na.rm=T) {
  ties <- match.arg(ties, c("first", "last"))

  if(na.rm) {
    ux <- unique(x[!is.na(x)])
  } else {
    ux <- unique(x)
  }

  ux <- ux[which(tabulate(match(x, ux)) == max(tabulate(match(x, ux))))]

  switch(ties,
         "first" = {mode <- ux[1]},
         "last" = {mode <- tail(ux, n=1)}
  )
  return(mode)
}


