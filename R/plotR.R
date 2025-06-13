#' plotR
#'
#' @description
#' Automatic plot function for the functions estimatR, incidencR and clustR
#'
#'
#' @param list an object of class estimatR, incidencR or clustR
#' @param y Upper limit for y-axis
#' @param col Vector of colors
#' @param table.col Grid color
#' @param time_unit Specification of the time-unit and optional conversion. Conversions include Months to years ("m2y"), days to years ("d2y") and days to months ("d2m")
#' @param labgrp Character vector of similar length to the number of levels in the group with labels. Reference is first.
#' @param print.est Whether absolute risks at the time horizon should be printet. Defaults to TRUE
#' @param contrast The type of contrast that should be provided. Includes risk difference ("rd", default), risk ratio ("rr"), hazard ratio ("hr") or "none".
#' @param title Plot title
#' @param title.size Plot title size
#' @param x.title X-axis title
#' @param x.title.size X-axis title size
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
#' @param table.size Risk table text size
#' @param legend.pos XY vector of legend position in percentage
#' @param tscale Global size scaler
#' @param censur Whether values <= 3 should be censored. Default = FALSE
#'
#' @return Plot of the adjusted cumulative incidence or Kaplan-Meier curve
#' @export
#'
#'


plotR <- function(list,
                   y=100,
                   col=c("#9B62B8", "#224B87", "#67A8DC", "#D66ACE"),
                   table.col = "#616161",
                   time_unit = "m2y",
                   labgrp = levels,
                   print.est = TRUE,
                   contrast = "rd",
                   title = "",
                   title.size = 7,
                   x.title = unit,
                   x.title.size = 6,
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
                   table.size = 5,
                   legend.pos = c(0.5,0.95),
                   tscale = 1,
                   censur=F) {

  if(class(list) %nin% c("estimatR", "clustR", "incidencR")) {
    return(cat("Data not generated with the functions estimatR, clustR or incidencR from the cancR package"))
  }

  time_unit <- match.arg(time_unit, c("m2y", "d2m", "d2y", "days", "months", "years"))
  diff <- match.arg(contrast, c("rd", "rr", "hr", "none"))

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

  if(class(list) %in% c("estimatR", "clustR"))
    reslist <- lapply(list("hr" = list$hr %>% mutate(type = "hr"),
                           "rd" = list$difference %>% mutate(type = "rd"),
                           "rr" = list$ratio %>% mutate(type = "rr")), function(x) {
                             x %>% rename_with(~ paste0(str_replace(.x, "hr|ratio", "diff")), matches("hr|ratio")) %>%
                               mutate(print = str_c(str_to_upper(type),
                                                    " = ",
                                                    numbR(diff, contrast.digits),
                                                    " (95%CI ",
                                                    numbR(lower, contrast.digits),
                                                    "-",
                                                    numbR(upper, contrast.digits),
                                                    "), ",
                                                    p.value))
                           })[contrast][[1]]

  if(censur) {
    tab <- tab %>% mutate(across(c(cumsum, n.risk), ~ ifelse(between(., 1, 3), "≤3", .)))
  }

  y=y/100
  tot = y+(y*0.6)

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
    geom_step(linewidth = 1.5) +
    scale_color_manual(values = c(col[1:length(levels)]), labels = labgrp) +
    scale_fill_manual(values = c(col[1:length(levels)]), labels = labgrp) +
    geom_stepribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
    coord_cartesian(xlim=c(-(horizon*0.1),horizon), ylim = c(-(y*0.6),1.2*y)) +
    scale_y_continuous(limits = c(-(0.8*y),1.06)) +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.position = legend.pos,
          legend.direction = "horizontal",
          legend.title = element_blank(),
          legend.text = element_text(size=16*tscale),
          plot.margin = margin(0,1,0,0, unit = "cm"))
  #Labels
  p <-
    #X-title
    p + annotate("text", x=horizon/2, y = -(tot*0.10), label = x.title, size = x.title.size*tscale) +
    #Y-title
    annotate("text", x=-(horizon*0.11)-y.title.shift, y = y/2, label = y.title, size = y.title.size*tscale, angle = 90) +
    #X-breaks
    annotate("text", x=seq(0,horizon,breaks), y=-(tot*0.06), label=round(seq(0,horizon,breaks)/u,0), size = x.text.size*tscale)

  yscale <- case_when(y>=0.5 ~ 1/10,
                      y<=0.1 ~ 1/100,
                      y<=0.01 ~ 1/1000,
                      T ~ 5/100)
  #Y-breaks
  p <- p + annotate("text", x=-(horizon*0.03), y=seq(0,y,yscale), label = paste(seq(0,y*100,yscale*100), "%", sep=""), size = y.text.size*tscale, hjust="right") +
    #Title
    annotate("text", x=-1, y=y*1.06, label=title, size = title.size*tscale, hjust="left")

  #Risk table segments
  p <- p +
    geom_segment(x = 0, xend=2, y=-(tot*0.27), yend=-(tot*0.27), color = "#616161", linewidth = 1) +               #Kort midt
    geom_segment(x = 0, xend=0, y=-(tot*0.16), yend=-(tot*0.15), color = "#616161", linewidth = 1) +               #Ve oppe
    geom_segment(x = 0, xend=2, y=-(tot*0.15), yend=-(tot*0.15), color = "#616161", linewidth = 1) +               #Kort oppe
    geom_segment(x = 0, xend=horizon*1.04, y=-(tot*0.39), yend=-(tot*0.39), color = "#616161", linewidth = 1) +        #Lang nede
    geom_segment(x = 0, xend=0, y=-(tot*0.39), yend=-(tot*0.38), color = "#616161", linewidth = 1) +          #Ve nede
    geom_segment(x = 0, xend=0, y=-(tot*0.255), yend=-(tot*0.285), color = "#616161", linewidth = 1) +        #Ve midt
    annotate("text", label = "Cumulative events", x = 3, y = -(tot*0.148), color = "#616161", size = 6*tscale, hjust="left") +
    annotate("text", label = "At risk", x = 3, y = -(tot*0.268), color = "#616161", size = 6*tscale, hjust="left")

  #Risk table numbers
  for(i in 1:length(levels)) {

    r1 <- -(tot*seq(.16,.26,.1/(length(levels)+1)))[2:(length(levels)+1)]
    r2 <- -(tot*seq(.28,.38,.1/(length(levels)+1)))[2:(length(levels)+1)]

    p <- p +
      annotate("text", label = tab[tab[, group] == levels[i], "cumsum"], x = seq(0,horizon,12), y = rev(r1)[i], color = col[i], size = table.size*(0.9^(length(levels)-2))*tscale) +
      annotate("text", label = tab[tab[, group] == levels[i], "n.risk"], x = seq(0,horizon,12), y = rev(r2)[i], color = col[i], size = table.size*(0.9^(length(levels)-2))*tscale)
  }

  #Result labels
  if(length(levels) == 1) {
    p <- p + theme(legend.position = "none")
  }

  if(class(list) == "incidencR") {
    contrast <- "none"
  }

  rows <- seq(0.95, 0.95-((0.05*res.spacing)*(length(levels)+1)), -0.05*res.spacing)

  if(survscale == "OS") {
    rows <- rev(1-rows)
  }

  if(print.est) {

    #Header
    p <- p + annotate("text",
                      x=horizon*0.04+res.shift[1],
                      y=rows[1]+res.shift[2],
                      label = paste0(horizon/u, "-", ifelse(unit %in% "Years", str_remove(unit, "s"), unit), " Risk", collapse=""),
                      fontface = 2,
                      hjust="left",
                      size = 5*tscale)

    for(i in 1:length(levels)) {

      #Segments
      p <- p +
        annotate("segment",
                 x=horizon*0.01+res.shift[1],
                 xend=horizon*0.03+res.shift[1],
                 y=rows[i+1],
                 yend=rows[i+1], color = col[i], linewidth = 1.5)

      #Groups > 2 with contrasts
      if(length(levels) > 2 & contrast != "none") {
        p <- p +annotate("text",
                         x=horizon*0.04+res.shift[1],
                         y=rows[i+1]+res.shift[2],
                         label = paste0(numbR(res$est[i]*100, res.digits), "%, ", c("reference", reslist$print[1:length(levels)-1])[i]),
                         fontface=1,
                         hjust="left",
                         size = res.size*tscale)

      }

      #Groups == 2 or >2 without contrasts
      else{
        p <- p +
          annotate("text",
                   x=horizon*0.04+res.shift[1],
                   y=rows[i+1],
                   label = paste(c(numbR(res$est[i]*100,res.digits),"% (95%CI ", numbR(res$lower[i]*100, res.digits),"-", numbR(res$upper[i]*100, res.digits),")"), collapse=""),
                   fontface=1,
                   hjust="left",
                   size = res.size*tscale)

        if(contrast != "none") {

          p <-  p+ annotate("text",
                            x=horizon*0.04+res.shift[1],
                            y = rows[length(levels)+2],
                            label = reslist$print,
                            fontface=3,
                            hjust = "left",
                            size = res.size*tscale)
        }
      }

    }
  }

  return(p)

}

