#' colR
#'
#' @description
#' Set default color palette
#'
#' @param cols Vector to set the default color palette. If "default", the default palette is reverted
#'
#' @return Adds cancR_palette to the global environment
#' @export
#'
#'
colR <- function(cols) {

  if(cols == "default") {

    cols <- c("#AA6489", "#39507C", "#FB8E4B", "#6F9580", "#6F8A95", "#BD6176", "#2B3068", "#99A5A5")

  }

  cancR_palette <<- cols

}


cancR_palette <- c("#AA6489", "#39507C", "#FB8E4B", "#6F9580", "#6F8A95", "#BD6176", "#2B3068", "#99A5A5")
