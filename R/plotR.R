#' plotR
#'
#' @description
#' Automatic plot function for the functions estimatR, incidencR and clustR
#'
#'
#'
#' @param list an object of class estimatR, incidencR or clustR
#' @param y Upper limit for y-axis
#' @param col Vector of colors
#' @param table.col Grid color
#' @param time_unit Specification of the time-unit and optional conversion. Conversions include Months to years ("m2y"), days to years ("d2y") and days to months ("d2m")
#' @param labs Character vector of similar length to the number of levels in the group with labels. Reference is first.
#' @param print.est Whether absolute risks at the time horizon should be printet. Defaults to TRUE
#' @param contrast The type of contrast that should be provided. Includes risk difference ("rd", default), risk ratio ("rr"), hazard ratio ("hr") or "none".
#' @param se whether the confidence interval should be shown
#' @param border whether there should be borders around the results
#' @param style the formatting style of the contrast. Currently JAMA and italic
#' @param linewidth thickness of the risk curve lines
#' @param title Plot title
#' @param title.size Plot title size
#' @param x.title X-axis title
#' @param x.title.size X-axis title size
#' @param x.title.shift X-axis vertical shift
#' @param x.text.size X-axis text size
#' @param y.title Y-axis title
#' @param y.title.size Y-axis title size
#' @param y.title.shift Y-axis title horizontal shift
#' @param y.text.size Y-axis text.size
#' @param res.size Size of the results
#' @param res.shift Vector of XY shifting of the results
#' @param res.spacing Vertical spacing between results
#' @param res.digits Number of digits on the risk estimates
#' @param contrast.digits Number of digits on the contrasts
#' @param table Which parts of the risk table should be provided ("event", "risk", "none"). Default is c("event", "risk")
#' @param table.space Spacing between counts in risk table
#' @param table.padding Spacing between lines and first/last rows in the risk table
#' @param table.title.size Risk table titles size
#' @param table.text.size Risk table text size
#' @param table.linewidth Risk table linewidth
#' @param legend.pos XY vector of legend position in percentage
#' @param tscale Global size scaler
#' @param censur Whether values <= 3 should be censored. Default = FALSE
#'
#' @return Plot of the adjusted cumulative incidence or Kaplan-Meier curve
#' @export
#'
#'
# n <- 500
# set.seed(1)
# df <- riskRegression::sampleData(n, outcome="survival")
# df$time <- round(df$time,1)*12
# df$time2 <- df$time + rnorm(n)
# df$X1 <- factor(rbinom(n, prob = c(0.3,0.4) , size = 2), labels = paste0("T",0:2))
# df$X3 <- factor(rbinom(n, prob = c(0.3,0.4,0.3) , size = 3), labels = paste0("T",0:3))
# df$event2 <- rbinom(n, 2, prob=.3)
# df <- as.data.frame(df)
#
# df2 <- df %>% mutate(X2 = ifelse(row_number()==1, NA, X2),
#                      event = as.factor(event)) %>%
#   rename(ttt = time)
#
# t1 <- estimatR(df2, ttt, event2, X2, time = 120, type = "select", vars = c(X6,X7), pl=T)
# t2 <- estimatR(df2, ttt, event2, X1, time = 60, type = "select", vars = c(X6,X7), pl=T)
# t3 <- estimatR(df2, ttt, event2, X3, time = 60, type = "select", vars = c(X6,X7), pl=T)
#
# plotR(t1, style = "jama")

plotR <- function(list,
                  y=100,
                  col=cancR_palette,
                  table.col = "#616161",
                  time_unit = "m2y",
                  labs = levels,
                  print.est = TRUE,
                  contrast = "rd",
                  se = T,
                  border = T,
                  style = NULL,
                  linewidth = 1.5,
                  title = "",
                  title.size = 7,
                  x.title = unit,
                  x.title.size = 6,
                  x.title.shift = 0,
                  x.text.size = 6,
                  y.title = "Risk of Event (%)",
                  y.title.size = 6,
                  y.title.shift = 0,
                  y.text.size = 6,
                  res.size = 5,
                  res.shift = c(0,0),
                  res.spacing = 1,
                  res.digits = 1,
                  contrast.digits = 1,
                  table = c("event", "risk"),
                  table.space = 1,
                  table.padding = 1,
                  table.title.size = 6,
                  table.text.size = 5,
                  table.linewidth = 1,
                  legend.pos = c(0.5,0.9),
                  tscale = 1,
                  censur=F) {

  if(class(list) %nin% c("estimatR", "clustR", "incidencR")) {
    return(cat("Data not generated with the functions estimatR, clustR or incidencR from the cancR package"))
  }

  time_unit <- match.arg(time_unit, c("m2y", "d2m", "d2y", "days", "months", "years"))
  diff <- match.arg(contrast, c("rd", "rr", "hr", "none"))
  table <- match.arg(table, c("event", "risk", "none"), several.ok=T)

  plot <- list$plot_data %>% drop_na(est, lower, upper)
  est <- list$risks
  group <- list$info$group
  levels <- list$info$group_levels
  horizon <- list$info$time
  breaks <- list$info$breaks
  surv <- list$info$surv
  survscale <- list$info$survscale
  tab <- list$table %>% filter(time %in% seq(0, horizon, breaks))
  res <- est %>% filter(time %in% horizon)

  #Contrast labels
  c_var <- names(list)[str_detect(names(list), paste0(contrast, collapse="|"))]

  switch(contrast,
         "rd" = c_var <- "difference",
         "rr" = c_var <- "ratio")

  #Prints
  c_labels <- str_c(str_to_upper(contrast),
                    " = ",
                    numbR(list[[c_var]][,which(names(list[[c_var]]) %in% c("hr", "ratio", "diff"))], contrast.digits),
                    " (95%CI ",
                    numbR(list[[c_var]][["lower"]], contrast.digits),
                    "-",
                    numbR(list[[c_var]][["upper"]], contrast.digits),
                    "), ",
                    list[[c_var]][["p.value"]])

  if(!is.null(style)) {

  if(style == "jama") {
  c_labels <- sapply(c_labels, function(x) {
    x <- str_replace(x, "RD","ARD")
    x <- str_split(str_remove(x, "(?<=(p.{3}))0"), "p")

    bquote(.(x[[1]][[1]])~italic("P")~.(x[[1]][[2]]))
  })
  }

  if(style == "italic") {

    c_labels <- sapply(c_labels, function(x) {

      bquote(italic(.(x)))
    })

  }
  }


  if(length(levels) > 2) {
    c_labels <- c("reference", c_labels[1:(length(levels)-1)])
  }




  if(censur) {
    tab <- tab %>% mutate(across(c(cumsum, n.risk), ~ ifelse(between(., 1, 3), "hej", .)))
  }

  if(missing(y)) {
    y <- closR(pmin(max(plot$upper[plot$time == horizon]*1.3), 1)*100, c(seq(1,5), seq(10, 100, 10)))
  }

  #Grid
  y=y/100

  if(any(table %nin% "none")) {
    #Table
    #Space
    s <- y*0.07*table.space
    #n_groups
    g <- length(levels)
    #padding
    b <- 1.2*table.padding
    #Rows
    zmax <- -(y*0.25)
    z1 <- rev(seq(zmax - b*s, (zmax - b*s) - (g-1)*s, -s))
    z2 <- rev(seq(min(z1) - (2*b*s), (min(z1) - (2*b*s)) - (g-1)*s, -s))

    if(length(table) == 1) {
      zmin <- min(z1) - b*s
      lines <- c(zmax, zmin)

    } else {
      zmin <- min(z2) - b*s
      lines <- c(zmax, zmin - (zmin-zmax)/2, zmin)

    }
    rows <- list(z1, z2)
  } else {
    zmin <- -(y*0.15)
  }

  switch(time_unit,
         "m2y" = {
           u <- 12
           unit <- "Years"},
         "d2m" = {
           u <- 365.25/12
           unit <- "Months"},
         "d2y" = {
           u <- 365.25
           unit <- "Years"
         },
         "days" = ,
         "months" = ,
         "years" = {
           u <- 1
           unit <- str_to_title(time_unit)
         })

  #PLOT BODY
  p <-
    ggplot(plot, aes(x=time, y=est, color = !!sym(group), fill = !!sym(group))) +
    geom_segment(x = -1, xend=horizon*1.04, y=-(y*0.04), yend=-(y*0.04), color = "Black", linewidth = 1.5) +
    geom_segment(x = -1, xend=-1, y=-(y*0.04), yend=y, color = "Black", linewidth = 1.5) +
    geom_step(linewidth = linewidth) +
    scale_color_manual(values = c(col[1:length(levels)]), labels = labs) +
    scale_fill_manual(values = c(col[1:length(levels)]), labels = labs)
  if(se) p <- p + geom_stepribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA)
  p <- p +
    coord_cartesian(xlim=c(horizon*-0.1-y.title.shift,horizon), ylim = c(zmin,1.2*y)) +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "inside",
          legend.position.inside = legend.pos,
          legend.direction = "horizontal",
          legend.title = element_blank(),
          legend.text = element_text(size=16*tscale),
          plot.margin = margin(0,1,0,0, unit = "cm"))
  #Labels
  p <-
    #X-title
    p + annotate("text", x=horizon/2, y = y*-0.2+x.title.shift, label = x.title, size = x.title.size*tscale) +
    #Y-title
    annotate("text", x=-(horizon*0.11)-y.title.shift, y = y/2, label = y.title, size = y.title.size*tscale, angle = 90) +
    #X-breaks
    annotate("text", x=seq(0,horizon,breaks), y=-(y*0.1), label=round(seq(0,horizon,breaks)/u,0), size = x.text.size*tscale)

  yscale <- case_when(y>=0.5 ~ 1/10,
                      y<=0.01 ~ 2.5/1000,
                      y<=0.1 ~ 1/100,
                      T ~ 5/100)
  #Y-breaks
  p <- p + annotate("text", x=-(horizon*0.03), y=seq(0,y,yscale), label = paste(seq(0,y*100,yscale*100), "%", sep=""), size = y.text.size*tscale, hjust="right") +

  #Title
    annotate("text", x=-1, y=y*1.06, label=title, size = title.size*tscale, hjust="left")

  #Risk table
  if(any(table %nin% "none")) {
    tablabs <- str_replace_all(table, c("risk" = "At Risk",
                                     "event" = "Cumulative Events"))

    #Grid
    for(i in 1:length(lines)) {

      if(length(lines) == 2) {
        lengths <- c(2,horizon*1.04)

      } else {
        lengths <- c(2,2,horizon*1.04)
      }

      p <- p +
        suppressWarnings(geom_segment(x = 0, xend=lengths[i], y=lines[i], yend=lines[i], color = table.col, linewidth = table.linewidth))

      if(i < 3) {
        p <- p +
          suppressWarnings(annotate("text", label = tablabs[i], x = 3, y = lines[i], color = table.col, size = table.title.size*tscale, hjust="left"))
      }


    }

    #Numbers
    tcols <- str_replace_all(table, c("risk" = "n.risk",
                                      "event" = "cumsum"))

    for(i in 1:length(levels)) {

      for(j in 1:length(table)) {
        p <- p +
          suppressWarnings(annotate("text", label = tab[tab[, group] == levels[i], tcols[j]], x = seq(0,horizon,12), y = rows[[j]][i], color = col[i], size = table.text.size*tscale))

      }

    }
  }

  #Result labels
  if(length(levels) == 1) {
    p <- p + theme(legend.position = "none")
  }

  if(class(list) == "incidencR") {
    contrast <- "none"
  }

  xstart <- horizon*0.05
  rows <- (y*(seq(0.92, 0.92-((0.07*res.spacing)*(length(levels)+1)), -0.07*res.spacing)))+res.shift[2]

  if(survscale == "OS") {
    rows <- rev(1-rows)
  }

  if(print.est) {

    #Header
    p <- p + annotate("text",
                      x=xstart+res.shift[1],
                      y=rows[1],
                      label = paste0(horizon/u, "-", ifelse(unit %in% "Years", str_remove(unit, "s"), unit), " Risk", collapse=""),
                      fontface = 2,
                      hjust="left",
                      size = 5*tscale)

    for(i in 1:length(levels)) {

      #Segments
      p <- p +
        annotate("segment",
                 x=xstart*0.3+res.shift[1],
                 xend=xstart*0.8+res.shift[1],
                 y=rows[i+1],
                 yend=rows[i+1], color = col[i], linewidth = 1.5)

      #Groups > 2 with contrasts
      if(length(levels) > 2 & contrast != "none") {
        p <- p +annotate("text",
                         x=xstart+res.shift[1],
                         y=rows[i+1],
                         label = suppressWarnings(bquote(.(numbR(res$est[i]*100, res.digits))~"%"~.(c_labels[[i]]))),
                         fontface=1,
                         hjust="left",
                         size = res.size*tscale)

      }

      #Groups == 2 or >2 without contrasts
      else{
        p <- p +
          annotate("text",
                   x=xstart+res.shift[1],
                   y=rows[i+1],
                   label = paste(c(numbR(res$est[i]*100,res.digits),"% (95%CI ", numbR(res$lower[i]*100, res.digits),"-", numbR(res$upper[i]*100, res.digits),")"), collapse=""),
                   fontface=1,
                   hjust="left",
                   size = res.size*tscale)

        if(contrast != "none") {

          p <-  p+ annotate("text",
                            x=xstart+res.shift[1],
                            y = rows[length(levels)+2],
                            label = c_labels[[1]],
                            hjust = "left",
                            size = res.size*tscale)
        }
      }

    }
  }

  if(border) {

    top <- rows[1] + (rows[1]-rows[2])*res.spacing
    buttom <- rows[length(rows)] - (rows[length(rows)-1]-rows[length(rows)])*res.spacing
    left <- (xstart*0.2-0.1)+res.shift[1]
    right <- (xstart*0.2+horizon*0.39)+res.shift[1]
    if(!is.null(style)) right <- right + horizon/60

    p <- p + annotate("segment", x=left, xend = right, y=top, yend = top, linewidth = 0.8) +
      annotate("segment", x=left, xend = right, y=buttom, yend = buttom, linewidth = 0.8) +
      annotate("segment", x=left, xend = left, y = top, yend = buttom, linewidth = 0.8) +
      annotate("segment", x=right, xend = right, y = top, yend = buttom, linewidth = 0.8)
  }

  p$y <- y*100
  p$grps <- length(levels)

 return(p)


}


