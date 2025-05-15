#' geom_stepribbon
#' Copy of the geom_stepribbon function by adibender/pammtools
#' @export
#'
#'
geom_stepribbon <- function(
    mapping     = NULL,
    data        = NULL,
    stat        = "identity",
    position    = "identity",
    direction   = "hv",
    na.rm       = FALSE,
    show.legend = NA,
    inherit.aes = TRUE, ...) {

  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomStepribbon,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(na.rm = na.rm, direction = direction, ... )
  )

}
