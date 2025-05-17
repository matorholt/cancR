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

ggplot2_stairstep <- function(data, direction =  c("hv", "vh", "mid")) {
  direction <- match.arg(direction)
  data <- as.data.frame(data)[order(data$x), ]
  n <- nrow(data)
  if (n <= 1) {
    return(data[0, , drop = FALSE])
  }
  if (direction == "vh") {
    xs <- rep(1:n, each = 2)[-2 * n]
    ys <- c(1, rep(2:n, each = 2))
  }
  if (direction == "hv") {
    xs <- c(1, rep(2:n, each = 2))
    ys <- rep(1:n, each = 2)[-2 * n]
  }
  if (direction == "mid") {
    xs <- rep(1:(n - 1), each = 2)
    ys <- rep(1:n, each = 2)
  }

  ymin <- c(data$ymin[ys])
  ymax <- c(data$ymax[ys])
  if (direction == "mid") {
    gaps <- data$x[-1] - data$x[-n]
    mid_x <- data$x[-n] + gaps/2
    x <- c(data$x[1], mid_x[xs], data$x[n])
    data_attr <- data[c(1, xs, n),
                      setdiff(names(data), c("x", "ymin", "ymax"))]
  } else {
    x <- data$x[xs]
    ymin <- data$ymin[ys]
    ymax <- data$ymax[ys]
    data_attr <- data[xs, setdiff(names(data), c("x", "ymin", "ymax"))]
  }
  cbind(data.frame(x = x, ymin = ymin, ymax = ymax), data_attr)
}
