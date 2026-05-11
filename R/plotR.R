#' Autoplot for estimatR, inferencR and clustR
#'
#'
#'
#' @param list an object of class estimatR, inferencR or clustR
#' @param y Upper limit for y-axis
#' @param col Vector of colors
#' @param table.col Grid color
#' @param risk.col Whether risk table numbers should be colored (T/F)
#' @param time.unit Specification of the time-unit and optional conversion. Conversions include Months to years ("m2y"), days to years ("d2y") and days to months ("d2m")
#' @param labs Character vector of similar length to the number of levels in the group with labels. Reference is first.
#' @param print.est Whether absolute risks at the time horizon should be printet. Defaults to TRUE
#' @param contrast The type of contrast that should be provided. Includes risk difference ("rd", default), risk ratio ("rr"), hazard ratio ("hr") or "none".
#' @param se whether the confidence interval should be shown
#' @param p.values whether p-values should be printed in the results, default = T
#' @param style the formatting style of the contrast. Currently JAMA and italic
#' @param linewidth thickness of the risk curve lines
#' @param title Plot title
#' @param title.size Plot title size
#' @param title.shift vector of XY shifting of the plot title
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
#' @param box whether there should be a box around the results
#' @param box.shift Horizontal shifting of the right end of the box
#' @param box.fill fill color for the box
#' @param box.color border color for the box
#' @param box.linewidth Results box linewidth
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
#' @examples
#' #Risk in one group
#'
#' t1 <- estimatR(analysis_df,
#' timevar = ttt,
#' event = event)
#'
#' plotR(t1)
#'
#' #Risks in multiple groups
#' t2 <- estimatR(analysis_df,
#' timevar = ttt,
#' event = event,
#' group = X2)
#'
#' plotR(t2)
#'
#'

# t0 <- estimatR(analysis_df, ttt, event2, time = 120)
# t1 <- estimatR(analysis_df, ttt, event2, g2, time = 120, vars = c(X6,X7))
# t2 <- estimatR(analysis_df, ttt, event2, g3, time = 120, vars = c(X6,X7), pl=T)
# t3 <- estimatR(analysis_df, ttt, event2, g4, time = 120, vars = c(X6,X7), pl=T)
# i1 <- inferencR(analysis_df,treatment = g2,
#                   timevar = ttt,
#                   event = event2,
#                   vars = c(g3, g4, X6, X7),
#                 estimator = "GFORMULA")
# plotR(t0)
# plotR(t1, style = "jama")
# plotR(t2, print.est = F)
# plotR(t3)
# plotR(i1)

plotR <- function(list,
                  y=100,
                  col=cancR_palette,
                  table.col = "#616161",
                  risk.col = F,
                  time.unit = "m2y",
                  labs = levels,
                  print.est = TRUE,
                  contrast = "rd",
                  se = T,
                  p.values = T,
                  style = NULL,
                  linewidth = 0.8,
                  title = "",
                  title.size = 7,
                  title.shift = c(0,0),
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
                  box = T,
                  box.shift = 0,
                  box.fill = "White",
                  box.color = "Black",
                  box.linewidth = 0.8,
                  contrast.digits = 1,
                  table = c("event", "risk"),
                  table.space = 1,
                  table.padding = 1,
                  table.title.size = 6,
                  table.text.size = 5,
                  table.linewidth = 0.8,
                  legend.pos = c(0.5,0.9),
                  legend.size = 16,
                  tscale = 1,
                  censur=F) {

  if(class(list) %nin% c("estimatR", "clustR", "inferencR")) {
    return(cat("Data not generated with the functions estimatR, inferencR or clustR from the cancR package"))
  }

  time.unit <- match.arg(time.unit, c("m2y", "d2m", "d2y", "days", "months", "years"))
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
  tab <- list$table %>% filter(time %in% seq(0, round(horizon,0), round(breaks,0)))
  res <- est %>% filter(time %in% horizon)
  event.digits <- list$info$event.digits

  if(list$info$method == "aalen") {

    list[["ratio"]] <- list[["difference"]]

  }

  if(length(levels) == 1) {
    contrast <- "none"
    tab <- tab %>% mutate(grp = " ")
    plot <- plot %>% mutate(grp = " ")

  }

  if(censur) tab <- tab %>% mutate(across(c(cumsum, n.risk), ~ ifelse(between(., 1, 3), "≤ 3", .)))

  if(missing(y)) {
    if(list$info$survscale == "AM") {
      y <- closR(pmin(max(plot$upper[plot$time == round(horizon, event.digits)]*2), 1)*100, c(seq(1,5), seq(10,50,5), seq(60, 100, 10)))
    } else {
      y <- 100
    }
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

  switch(time.unit,
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
           unit <- str_to_title(time.unit)
         })


  #PLOT BODY
  p <-
    ggplot(plot, aes(x=time, y=est, color = !!sym(group), fill = !!sym(group))) +
    geom_step(linewidth = linewidth) +
    #X-axis
    geom_segment(x = -(horizon*0.0075), xend=horizon*1.04, y=-(y*0.01), yend=-(y*0.01), color = "Black", linewidth = linewidth) +
    #Y-axis
    geom_segment(x = 0, xend=0, y=-(y*0.0375), yend=y, color = "Black", linewidth = linewidth) +
    scale_color_manual(values = c(col[1:length(levels)]), labels = labs) +
    scale_fill_manual(values = c(col[1:length(levels)]), labels = labs)
  if(se) p <- p + pammtools::geom_stepribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA)
  p <- p +
    coord_cartesian(xlim=c(horizon*-0.1-y.title.shift,horizon), ylim = c(zmin,1.2*y+pmax(res.shift[2],0))) +
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
          legend.text = element_text(size=legend.size*tscale),
          plot.margin = margin(0,1,0,0, unit = "cm"))
  #Labels
  p <-
    #X-title
    p + annotate("text", x=horizon/2, y = y*-0.18+x.title.shift, label = x.title, size = x.title.size*tscale) +
    #Y-title
    annotate("text", x=-(horizon*0.10)-y.title.shift, y = y/2, label = y.title, size = y.title.size*tscale, angle = 90) +
    #X-breaks
    annotate("text", x=seq(0,horizon,breaks), y=-(y*0.08), label=round(seq(0,horizon,breaks)/u,0), size = x.text.size*tscale)

  yscale <- case_when(y>=0.5 ~ 1/10,
                      y<=0.01 ~ 2.5/1000,
                      y<=0.05 ~ 5/1000,
                      y<=0.1 ~ 1/100,
                      T ~ 5/100)
  #Y-breaks/labels
  p <- p + annotate("text", x=-(horizon*0.01), y=seq(0,y,yscale), label = paste(seq(0,y*100,yscale*100), "%", sep=""), size = y.text.size*tscale, hjust="right") +

    #Title
    annotate("text", x=0+title.shift[1], y=y*1.09+title.shift[2], label=title, size = title.size*tscale, hjust="left")

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

      #  p <- p +
      # suppressWarnings(geom_segment(x = 0, xend=lengths[i], y=lines[i], yend=lines[i], color = table.col, linewidth = table.linewidth))

      if(i < 3) {
        p <- p +
          suppressWarnings(annotate("text", label = tablabs[i], x = -(horizon*0.075), y = lines[i], color = table.col, size = table.title.size*tscale, hjust="left"))
      }


    }



    #Numbers
    tcols <- str_replace_all(table, c("risk" = "n.risk",
                                      "event" = "cumsum"))


    if(risk.col) {

      rc <- col
    } else {
      rc <- rep(table.col,length(levels))
    }

    for(i in 1:length(levels)) {

      for(j in 1:length(table)) {
        p <- p +
          suppressWarnings(annotate("text", label = tab[tab[, group] == levels[i], tcols[j]], x = seq(0,horizon,breaks), y = rev(rows[[j]])[i], color = rc[i], size = table.text.size*tscale))

        if(!risk.col) {

          p <- p +
            annotate("segment", x=-(horizon*0.075), xend =-(horizon*0.045), y = rev(rows[[j]])[i], yend = rev(rows[[j]])[i], color=col[i], linewidth = linewidth*1.5)

        }

      }

    }
  }


  #Labels
  if(print.est) {

    #Header
    p.header <- paste0(horizon/u, "-", ifelse(unit %in% "Years", str_remove(unit, "s"), unit), " Risk", collapse="")

    #Contrast
    if(contrast != "none") {

      switch(contrast,
             "rd" = c_var <- "difference",
             "rr" = c_var <- "ratio")

      if(p.values) {
        pval <- list[[c_var]][["p.value"]]
      } else {
        pval <- NULL
      }

      c_label <- str_c(str_to_upper(contrast),
                       " = ",
                       numbR(list[[c_var]][,which(names(list[[c_var]]) %in% c("hr", "ratio", "diff"))], contrast.digits),
                       " (95%CI ",
                       numbR(list[[c_var]][["lower"]], contrast.digits),
                       " to ",
                       numbR(list[[c_var]][["upper"]], contrast.digits),
                       "), ",
                       pval)[1:(length(levels)-1)]

      if(!is.null(style)) {

        if(style == "jama") {
          c_label <- str_replace_all(c_label, "RD =", "ARD =")
          c_label <- str_remove(c_label, "(?<=(p.{3}))0")
        }
      }

    } else {c_label <- NULL}

    #If more than 2 groups, contrasts are appended to the ends of risks
    if(length(levels) > 2 & contrast != "none") {
      p.labs <- map_chr(seq_along(levels), ~ {
        paste(c(numbR(res$est[.x]*100,res.digits), "%, ", c("Reference", c_label)[.x]), collapse="")
      })
    } else {
      p.labs <- c(map_chr(seq_along(levels), ~ {
        paste(c(numbR(res$est[.x]*100,res.digits),"% (95%CI ", numbR(res$lower[.x]*100, res.digits),"-", numbR(res$upper[.x]*100, res.digits),")"), collapse="")
      }), c_label)
    }

    p.labs <- c(p.header, p.labs)

  #PLOT LABELS
  if(length(levels) == 1) {
    p <- p + theme(legend.position = "none")
  }

  xstart <- horizon*0.1
  rows <- (y*(seq(0.92, 0.92-((0.07*res.spacing)*(length(p.labs)-1)), -0.07*res.spacing)))+res.shift[2]

  if(survscale == "OS") {
    rows <- rev(1-rows)
  }


  #box settings customization
  buttom <- rows[length(rows)] - (rows[1]-rows[2])
  width<- ifelse(horizon == 60, 0.95, 1.9)
  right <- max(str_count(p.labs))*width+res.shift[1]+box.shift
  top <- rows[1] + (rows[1]-rows[2])
  left <- (xstart*0.4)+res.shift[1]


  if(box) {


    if(!is.null(style)) right <- right + horizon/60

    p <- p + annotate("segment", x=left, xend = right, y=top, yend = top, linewidth = box.linewidth) +
      annotate("segment", x=left, xend = right, y=buttom, yend = buttom, linewidth = box.linewidth) +
      annotate("segment", x=left, xend = left, y = top, yend = buttom, linewidth = box.linewidth) +
      annotate("segment", x=right, xend = right, y = top, yend = buttom, linewidth = box.linewidth)

    p <- p + annotate("rect", xmin = left, xmax = right, ymin = buttom, ymax = top, linewidth = box.linewidth, fill = box.fill, color = box.color)
  }

  p <- p +
    annotate("text",
             x = xstart + res.shift[1],
             y=rows,
             label = p.labs,
             fontface = c(2, rep(1, length(p.labs)-1)),
             hjust="left",
             size = res.size*tscale)

  #Segments
  for(i in 2:(length(levels)+1)) {

    p <- p +
      annotate("segment",
               x=xstart*0.6+res.shift[1],
               xend=xstart*0.9+res.shift[1],
               y=rows[i],
               yend=rows[i], color = col[i-1], linewidth = linewidth*1.5)

  }

  }

  p$y <- y*100
  p$grps <- length(levels)
  p$coords <- list(table.rows = list(z1,z2),
                   table.lines.x = lengths,
                   table.lines.y = lines,
                   table.segments = c(-(horizon*0.075), -(horizon*0.045)),
                   results.rows = rows,
                   axis = list(x = c(0,horizon*1.04),
                               y = c(-(y*0.02), y)),
                   dimensions = list(x = c(horizon*-0.1-y.title.shift,horizon),
                                     y = c(zmin,1.2*y+pmax(res.shift[2],0))))





  return(p)


}
