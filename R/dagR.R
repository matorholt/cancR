#' Draw directed acyclic graphs (DAGS)
#'
#' @description
#' Function for drawing DAGs. Compatible with dagitty.net via the "dagitty" argument.
#'
#'
#' @param treatment name of treatment variable
#' @param outcome name of outcome variable
#' @param path.list named list of paths to draw in the format list("treatment" = list("to" = "outcome"), "confounder" = list("to" = c("treatment", "outcome")))
#' @param arrow.fill color of the arrow heads
#' @param arrow.color color of the arrows
#' @param arrow.width linewidth of the arrows
#' @param arrow.distance percentage reduction in arrow length
#' @param curvature curvature of the arrows
#' @param label.size text size of the variable nodes
#' @param dagitty output from dagitty.net inserted in single quotation marks.
#'
#' @returns plot of the specified dag
#' @export
#'

# dagR(treatment = "treatment",
#      outcome = "outcome",
#      list("treatment" = list("to" = "outcome"),
#           "conf" = list("to" = c("treatment", "outcome")),
#           "x1" = list("to" = c("outcome"))),
#      arrow.distance = 0.05)
#
# dagR(dagitty = 'dag {
#  C [pos="0.370,0.367"]
#  Ex [pos="0.147,0.282"]
#  Out [pos="0.558,0.266"]
#  C -> Ex
#  C -> Out
#  Ex -> Out
#  }')

dagR <- function(treatment,
                 outcome,
                 path.list,
                 label.fill = "White",
                 label.color = "White",
                 arrow.fill = "Black",
                 arrow.color = "Black",
                 arrow.width = 0.7,
                 arrow.distance = 0.2,
                 curvature = 0,
                 label.size = 6,
                 dagitty = NULL) {

  if(!is.null(dagitty)) {

    vars <- unlist(str_extract_all(dagitty, "(?<!(\\>\\s))\\w+(?=(\\s\\[))"))

    positions <- data.frame(vars = vars,
                            x = as.numeric(unlist(str_extract_all(dagitty, "(?<=(pos..))\\-?\\d+\\.?\\d*"))[1:length(vars)]),
                            y = -1*as.numeric(unlist(str_extract_all(dagitty, "(?<=(\\,))\\-?\\d+\\.?\\d*"))[1:length(vars)]))

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

  positions <- data.frame(vars = vars,
             x = c(5,7,6,sample(slots$x, size = (length(vars)-3))),
             y = c(5,5,4,sample(slots$y, size = (length(vars)-3))))

  }

  segs <-
    bind_rows(lapply(seq_along(path.list), function(i) {


      bind_cols(positions %>%
                  filter(vars %in%  names(path.list)[i]) %>%
                  select(x,y),

                positions %>%
                  filter(vars %in% path.list[[i]]$to) %>%
                  select(x,y) %>%
                  rename(xend = x,
                         yend= y))

    })) %>%
    mutate(xend = xend-((xend-x)*arrow.distance),
           yend = yend-((yend-y)*arrow.distance))

  positions %>%
    mutate(vars = str_to_title(vars)) %>%
  ggplot(aes(x=x, y=y, label = vars)) +
    geom_curve(data = segs, aes(x=x,
                                xend=xend,
                                y=y,
                                yend=yend),
               inherit.aes=F,
               arrow = arrow(type = "closed"),
               arrow.fill = arrow.fill,
               color = arrow.color,
               linewidth = arrow.width,
               curvature = curvature) +
    geom_label(size = label.size, fill = label.fill, border.color = label.color) +
    theme_void()

}
