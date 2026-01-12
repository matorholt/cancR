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
#' @param treatment.fill fill color for the treatment label
#' @param outcome.fill fill color for the outcome label
#' @param arrow.fill fill color of the arrow heads
#' @param arrow.color color of the arrows
#' @param arrow.linewidth linewidth of the arrows
#' @param arrow.size size of the arrow head in mm
#' @param arrow.distance distance from label center to arrowhead as a fraction of the euclidian distance
#' @param curvature named list of segments that should be curved in the format list(c("treatment", "outcome", 0.2))
#' @param label.size text size of the labels
#' @param dagitty output from dagitty.net inserted in single quotation marks.
#' @param seed for reproducibility
#'
#' @returns plot of the specified dag
#' @export
#'
#' @examples
#' dagR(treatment = "treatment",
#' outcome = "outcome",
#' list("treatment" = "outcome",
#'      "conf" = c("treatment", "outcome"),
#'      "x1" = "outcome",
#'      "x2" = c("treatment", "outcome")),
#' arrow.distance = 0.25,
#' arrow.linewidth = 0.7,
#' label.positions = list("treatment" = c(5,5)),
#' seed = 3,
#' curvature = list(c("conf", "outcome", 0.2)))
#'

# dagR(dagitty = 'dag {
# A [selected,pos="-2.155,-1.572"]
# B [pos="1.311,-1.479"]
# D [outcome,pos="1.312,1.327"]
# E [exposure,pos="-2.084,1.383"]
# Z [adjusted,pos="-0.466,-0.243"]
# A -> Z [pos="-0.791,-1.045"]
# A <-> E
# B -> D
# B -> Z [pos="0.680,-0.496"]
# E -> D
# }
#
# ')

# dagR(treatment = "treatment",
#      outcome = "outcome",
#      list("treatment" = "outcome",
#           "conf" = c("treatment", "outcome"),
#           "x1" = "outcome",
#           "x2" = c("treatment", "outcome")),
#      arrow.distance = 0.25,
#      arrow.linewidth = 0.7,
#      label.positions = list("treatment" = c(5,5)),
#      seed = 3,
#      curvature = list(c("conf", "outcome", 0.2)))

dagR <- function(treatment,
                 outcome,
                 path.list,
                 label.fill = "#C4C4C4",
                 label.color = "Black",
                 label.positions = NULL,
                 treatment.fill = "#AB5CB8",
                 outcome.fill = "#97AFCC",
                 arrow.fill = "Black",
                 arrow.color = "Black",
                 arrow.linewidth = 0.7,
                 arrow.size = 5,
                 arrow.distance = 0.2,
                 curvature = NULL,
                 label.size = 6,
                 dagitty = NULL,
                 seed=1) {

  if(!is.null(dagitty)) {

    treatment <- str_extract(daggity, "\\w+(?=(\\s\\[exposure))")
    outcome <- str_extract(daggity, "\\w+(?=(\\s\\[outcome))")

    vars <- unlist(str_extract_all(dagitty, "(?<!(\\>\\s))\\w+(?=(\\s\\[))"))

    #Scaling, x = 2.25 and y=1.7, reverse y-axis
    positions <- data.frame(vars = vars,
                            x = (as.numeric(unlist(str_extract_all(dagitty, "(?<=(pos..))\\-?\\d+\\.?\\d*"))[1:length(vars)])+2.7),
                            y = (-1*as.numeric(unlist(str_extract_all(dagitty, "(?<=(\\,))\\-?\\d+\\.?\\d*"))[1:length(vars)])+1.5))

    from <- unlist(str_extract_all(dagitty, "\\w+(?=(\\s\\-\\>))"))
    to <- unlist(str_extract_all(dagitty, "(?<=(\\-\\>\\s))\\w+"))

    path.list <- lapply(unique(from), function(i) {
      list("to" = to[which(from == i)])
    }) %>% set_names(unique(from))

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

  degrees <- (pi*seq(0,360,5))/180

  #Add euclidian distance to all points + update evenutal curvatures
  for(n in 1:nrow(segs)) {

    x <- segs[n, "x"]
    y <- segs

    xend <- segs[n,"xend"]
    yend <- segs[n,"yend"]

    xs <- segs[n,"xend"]+cos(degrees)*arrow.distance*1.5
    ys <- segs[n,"yend"]+sin(degrees)*arrow.distance

    min <- which.min(sqrt((xs-segs[n, "x"])^2 + (ys - segs[n, "y"])^2))

    segs[n, "xend"] <- xs[min]
    segs[n, "yend"] <- ys[min]

  }

  if(!is.null(curvature)) {

    for(i in seq_along(curvature)) {

      segs$curvature[segs$vars_from == curvature[[i]][1] & segs$vars_to == curvature[[i]][2]] <- curvature[[i]][3]

    }

  }

  segs <- segs %>%
    mutate(lw = ifelse(vars_from == treatment & vars_to == outcome, arrow.linewidth*2, arrow.linewidth))

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
               curvature = segs[n, "curvature"])


  }

  p <- p +
    geom_label(size = label.size, border.color = label.color, fill = positions$cols) +
    theme_void()

return(p)

}
