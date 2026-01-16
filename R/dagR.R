#' Draw directed acyclic graphs (DAGS)
#'
#' @description
#' Function for drawing DAGs. Compatible with dagitty.net via the "dagitty" argument.
#'
#'
#' @param treatment name of treatment variable
#' @param outcome name of outcome variable
#' @param path.list named list of paths to draw in the format list("treatment" = "outcome", "confounder" = c("treatment", "outcome"))
#' @param label.fill fill color for labels
#' @param label.color border.color for labels
#' @param label.positions manual adjustment of label positions (x,y) in the format list("treatment" = c(0,0))
#' @param segment.shift list of vectors of c(from, to, x, y) where x and y are shifts in coordinate, e.g. list("conf", "outcome", 1,0)
#' @param treatment.fill fill color for the treatment label
#' @param outcome.fill fill color for the outcome label
#' @param arrow.color color of the arrows
#' @param arrow.color.main color of the arrow between treatment and outcome
#' @param arrow.linewidth linewidth of the arrows
#' @param arrow.size size of the arrow head in mm
#' @param arrow.distance distance from label center to arrowhead (unit same as coordinates)
#' @param distance.method how the points should be spread around lables ("rectangle" (default), "midways", "corners", "circle" and "oval")
#' @param distance.resolution the number of subdivisions on the x and y axis of the given shape. For circles it indicates the subdivisions of 360 degrees.
#' @param distance.ratio the ratio between the x.distance and y.distance. Larger values increase the distance width.
#' @param distance.string arbitrary value specifying the amount of spacing caused by the number of characters in the label
#' @param curvature named list of segments that should be curved in the format list(c("treatment", "outcome", 0.2))
#' @param label.size text size of the labels
#' @param dagitty output from dagitty.net inserted in single quotation marks.
#' @param position.digits rounding of the position cooridinates for dagitty plots for aligment. Default = 0.
#' @param seed for reproducibility
#' @param margin vector of length 4 adding space to the limits of the x and y axis in the format c(xmin, xmax, ymin,ymax)
#' @param draw whether the distance.points should be drawn around label for diagnostical purposes (default = F)
#'
#'
#' @returns plot of the specified dag
#' @export
#'
#' @examples
#' dagR(treatment = "treatment",
#'      outcome = "outcome",
#'      list("treatment" = "outcome",
#'           "conf" = c("treatment", "outcome"),
#'           "x1" = "outcome",
#'           "x2" = c("treatment", "outcome")),
#'      arrow.distance = 0.1,
#'      arrow.linewidth = 0.7,
#'      arrow.size = 5,
#'      distance.ratio = 40,
#'      distance.method = "rectangle",
#'      distance.resolution = 10,
#'      label.positions = list("treatment" = c(5,5)),
#'      segment.shift = list(c("conf", "outcome", .2,-0.05)),
#'      seed = 3,
#'      curvature = list(c("conf", "outcome", 0.2)))


# dagR(treatment = "treatment",
#      outcome = "outcome",
#      list("treatment" = "outcome",
#           "conf" = c("treatment", "outcome"),
#           "x1" = "outcome",
#           "x2" = c("treatment", "outcome")),
#      arrow.distance = 0.25,
#      arrow.linewidth = 0.7,
#      arrow.size = 5,
#      distance.ratio = 1.2,
#      distance.method = "oval",
#      distance.resolution = 180,
#      label.positions = list("treatment" = c(5,5)),
#      segment.shift = list(c("conf", "outcome", .2,-0.05)),
#      seed = 3,
#      curvature = list(c("conf", "outcome", 0.2)),
#      margin = c(-1,1,-1,1))
#
# dagR(treatment = "treatment",
#      outcome = "outcome",
#      list("treatment" = "outcome",
#           "conf" = c("treatment", "outcome"),
#           "x1" = "outcome",
#           "x2" = c("treatment", "outcome")),
#      draw = T,
#      distance.method = "rectangle",
#      distance.resolution = 6)
#
# #Simple example
# mydag1 <- 'dag {
# bb="0,0,1,1"
# conf [pos="0.429,0.558"]
# ex [exposure,pos="0.251,0.386"]
# mediator [pos="0.429,0.236"]
# out [outcome,pos="0.621,0.391"]
# x1 [pos="0.587,0.206"]
# conf -> ex
# conf -> out
# ex -> mediator
# ex -> out
# mediator -> out
# x1 -> out
# }
# '
# dagR(dagitty= mydag1,
#      margin = c(-1,1,-1,1),
#      arrow.distance = 0.3,
#      distance.ratio = 1.4,
#      arrow.size = 2,
#      distance.resolution = 4)
#
# mydag2 <- 'dag {
# bb="0,0,1,1"
# "Excision Depth" [pos="0.308,0.163"]
# "Local Recurrence" [outcome,pos="0.612,0.319"]
# "Margin (5 vs. 10)" [exposure,pos="0.176,0.322"]
# Age [pos="0.361,0.570"]
# Immunosuppression [pos="0.537,0.533"]
# Invasion [pos="0.502,0.161"]
# Sex [pos="0.187,0.525"]
# "Excision Depth" -> "Local Recurrence"
# "Excision Depth" -> "Margin (5 vs. 10)"
# "Margin (5 vs. 10)" -> "Local Recurrence"
# Age -> "Local Recurrence"
# Age -> "Margin (5 vs. 10)"
# Immunosuppression -> "Local Recurrence"
# Immunosuppression -> "Margin (5 vs. 10)"
# Invasion -> "Local Recurrence"
# Sex -> "Local Recurrence"
# Sex -> "Margin (5 vs. 10)"
# }
# '
#
# dagR(dagitty= mydag2,
#      arrow.distance = 0.3,
#      distance.ratio = 1,
#      arrow.size = 3,
#      distance.resolution = 4)



dagR <- function(treatment,
                 outcome,
                 path.list,
                 label.fill = cancR_palette[8],
                 label.color = "Black",
                 label.positions = NULL,
                 segment.shift = list(),
                 treatment.fill = cancR_palette[1],
                 outcome.fill = cancR_palette[4],
                 arrow.color = "Black",
                 arrow.color.main = "Black",
                 arrow.linewidth = 0.7,
                 arrow.size = 5,
                 arrow.distance = 0.1,
                 distance.method = "rectangle",
                 distance.resolution = 6,
                 distance.ratio = 1,
                 distance.string = 0.12,
                 curvature = NULL,
                 label.size = 6,
                 dagitty = NULL,
                 position.digits = 0,
                 seed=1,
                 margin = c(-.1,.1,-.1,.1),
                 draw = F) {



  if(distance.method %nin% (method.choices <- c("rectangle", "midways", "corners", "circle", "oval"))) {

    return(cat(paste0(c("Error: Invalid choice of distance.method. Options:\n", paste0(method.choices, collapse="\n "), collapse = ""))))

  }

  if(!is.null(dagitty)) {

    #Cleaning
    dagitty <- str_remove(dagitty, 'bb=\\"0,0,1,1\\"') %>%
      str_remove_all(., '\\"')

    treatment <- str_extract(dagitty, ".+(?=(\\s\\[exposure))")
    outcome <- str_extract(dagitty, ".+(?=(\\s\\[outcome))")

    vars <- unlist(str_extract_all(dagitty, "(?<!(\\>\\s)).+(?=(\\s\\[))"))

    #Scaling, x = 2.25 and y=1.7, reverse y-axis
    positions <- data.frame(vars = vars,
                            x = (as.numeric(unlist(str_extract_all(dagitty, "(?<=(pos.))\\-?\\d+\\.?\\d*"))[1:length(vars)]))*5,
                            y = -1*(as.numeric(unlist(str_extract_all(dagitty, "(?<=(\\,))\\-?\\d+\\.?\\d*"))[1:length(vars)]))*5) %>%
      mutate(across(c(x,y), ~ round(., position.digits)))

    from <- unlist(str_extract_all(dagitty, ".+(?=(\\s\\-\\>))"))
    to <- unlist(str_extract_all(dagitty, "(?<=(\\-\\>\\s)).+"))

        path.list <- lapply(unique(from), function(i) {
      c(to[which(from == i)])
    }) %>% set_names(unique(from))

    #scaling
    arrow.distance <- arrow.distance *0.6
    if(distance.string != 0.12) distance.string = 0.08
    margin <- margin * 10


  } else {

  vars <- unique(c(treatment, outcome, c(names(path.list), unlist(path.list))))

  slots <- data.frame(x <- c(rep(seq(4,8),2),
                             rep(seq(3,9),2)),
                      y <- c(rep(c(4,6), each=5),
                             rep(c(3,7),each=7)))
  set.seed(seed)
  positions <- data.frame(vars = vars,
             x = c(5,7,6,sample(slots$x, size = (length(vars)-3))),
             y = c(5,5,4,sample(slots$y, size = (length(vars)-3))))


  }

  #Update positions manually
  if(!is.null(label.positions)) {

    for(i in names(label.positions)) {
      positions[positions$vars == i, "x"] <- label.positions[[i]][1]
      positions[positions$vars == i, "y"] <- label.positions[[i]][2]

    }

  }

  segs <-
    bind_rows(lapply(seq_along(path.list), function(i) {


      bind_cols(positions %>%
                  filter(vars %in%  names(path.list)[i]) %>%
                  select(vars, x,y) %>%
                  rename(vars_from = vars),

                positions %>%
                  filter(vars %in% path.list[[i]]) %>%
                  select(vars, x,y) %>%
                  rename(vars_to = vars,
                         xend = x,
                         yend= y)) %>%
        mutate(curvature = 0)

    }))

  #Apply distance between x/y-ends and label positions

  coords <- c()
  draw.list <- list()

    for(n in 1:nrow(segs)) {

    xend <- segs[n,"xend"]
    yend <- segs[n,"yend"]

    x.distance <- (arrow.distance*distance.ratio)*((str_count(segs[n, "vars_to"])*distance.string)+0.5)

    if(distance.method == "midways") {

      xs <- c(xend+(c(-1,1)*x.distance), rep(xend,2))
      ys <- c(rep(yend,2), yend+(c(-1,1)*arrow.distance))

    }

    if(distance.method == "corners") {
      print(distance.method)
      xs <- c(rep(xend+(c(-1,1)*x.distance),2))
      ys <- c(rep(yend+(c(-1,1)*arrow.distance), each=2))
    }

    if(distance.method == "rectangle") {

      if(distance.resolution %% 2 != 0 | distance.resolution < 2 ) {
        return(cat("Error: distance.resolution needs to be an even number > 0"))
      }

      xvec <- seq(xend-x.distance, xend+x.distance, length.out = distance.resolution + 1)
      yvec <- seq(yend-arrow.distance, yend+arrow.distance, length.out = distance.resolution + 1)

      xs <- c(rep(xvec, 2),
              rep(range(xvec), each = length(yvec)))

      ys <- c(rep(range(yvec), each = length(xvec)),
              rep(yvec, 2))

    }
    degrees <- (pi*seq(0,360,360/distance.resolution))/180

    if(distance.method == "circle") {

      xs <-
        xend+cos(degrees)*arrow.distance
      ys <-
        yend+sin(degrees)*arrow.distance

    }

    if(distance.method == "oval") {

      xs <-
        xend+cos(degrees)*x.distance
      ys <-
        yend+sin(degrees)*arrow.distance

    }

    draw.list[[segs[n, "vars_to"]]]$x <- xs
    draw.list[[segs[n, "vars_to"]]]$y <- ys

    xsys <- paste(xs,ys,sep=",")
    indices <- which(xsys %in% coords)

    if(length(indices > 0)) {
      xs <- xs[-indices]
      ys <- ys[-indices]
    }

    min <- which.min(sqrt((xs-segs[n, "x"])^2 + (ys - segs[n, "y"])^2))

    coords <- c(coords, paste(xs[min],ys[min],sep=","))

    segs[n, "xend"] <- xs[min]
    segs[n, "yend"] <- ys[min]

    }

   if(!is.null(curvature)) {

    for(i in seq_along(curvature)) {

      segs$curvature[segs$vars_from == curvature[[i]][1] & segs$vars_to == curvature[[i]][2]] <- curvature[[i]][3]

    }

  }

  segs <- segs %>%
    mutate(lw = ifelse(vars_from == treatment & vars_to == outcome, arrow.linewidth*2, arrow.linewidth),
           cols = ifelse(vars_from == treatment & vars_to == outcome, arrow.color.main, arrow.color))

  for(i in seq_along(segment.shift)) {

    vf <- segment.shift[[i]][1]
    vt <- segment.shift[[i]][2]
    x <- as.numeric(segment.shift[[i]][3])
    y <- as.numeric(segment.shift[[i]][4])

    segs$xend[segs$vars_from == vf & segs$vars_to == vt] <- segs$xend[segs$vars_from == vf & segs$vars_to == vt] + x
    segs$yend[segs$vars_from == vf & segs$vars_to == vt] <- segs$yend[segs$vars_from == vf & segs$vars_to == vt] + y

  }

  positions <- positions %>%
    mutate(
      cols = case_when(vars == treatment ~ treatment.fill,
                       vars == outcome ~ outcome.fill,
                       T ~ label.fill),
      vars = str_to_title(vars))

  p <- ggplot(positions, aes(x=x, y=y, label = vars))

  for(n in 1:nrow(segs)) {

    p <- p +
      annotate("curve",
               x=segs[n, "x"],
               xend = segs[n, "xend"],
               y=segs[n, "y"],
               yend=segs[n, "yend"],
               arrow = arrow(type = "closed",
                             length = unit(arrow.size, "mm")),
               linewidth = segs[n, "lw"],
               curvature = segs[n, "curvature"],
               color = segs[n, "cols"])


  }

  p <- p +
    geom_label(size = label.size, border.color = label.color, fill = positions$cols) +
    theme_void() +
    coord_cartesian(xlim=range(segs$x)+margin[1:2],
                    ylim=range(segs$y)+margin[3:4])

  if(draw) {

    for(i in seq_along(draw.list)) {

    p <- p +
      annotate("point", x = draw.list[[i]]$x, y = draw.list[[i]]$y, size = 3)

    }

  }

return(p)

}
