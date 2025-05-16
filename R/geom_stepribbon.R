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

GeomStepribbon <- ggproto(
  "GeomStepribbon", GeomRibbon,

  extra_params = c("na.rm"),

  draw_group = function(data, panel_scales, coord, na.rm = FALSE, direction = "hv") {

    if (na.rm) data <- data[complete.cases(data[c("x", "ymin", "ymax")]), ]
    data   <- rbind(data, data)
    data   <- data[order(data$x), ]
    data   <- ggplot2_stairstep(data[complete.cases(data["x"]), ],
                                direction = direction)
    GeomRibbon$draw_group(data, panel_scales, coord, na.rm = na.rm)
  }

)
