#' plotR
#'
#' @description
#' Automatic plot of adjusted Kaplan-Meier and cumulative incidence curves
#'
#'
#' @param list crrlist or plist object from the crrstrat function
#' @param y Value of y-axis in whole number (eg. 50)
#' @param col color palette as a vector
#' @param outcome Labels for outcome
#' @param ylab Label for y-axis
#' @param labpos adjustments for point estimates on the plot. Can be removed with NA
#' @param printres Which results should be printed. Estimates="est", hazard ratio = "hr", risk difference = "rd" and risk ratio = "rr"
#' @param labgrp group labels
#' @param fu length of follow-up (default = 120 months)
#' @param breaks the subdivisions between time = 0 and follow-up length (defaults to 12)
#' @param time_unit Whether the time unit should be transformed to years or months. If no transformation choose days/months/years (defaults to "m2y")
#' @param respos.x shift of the result label on the x-axis
#' @param respos.y shift of the result label on the y-axis
#' @param ylab.pos shifting of y-axis label position
#' @param legend.pos legend position as vector
#' @param tscale Global scaling parameter if text needs to change size
#' @param ndigits_est No of digitis in the estimates
#' @param ndigits_res No of digits in the results
#' @param censur Censuring of counts < 5. Default = FALSE
#'
#' @return Adjusted Kaplan-Meier or cumulative incidence curves
#' @export
#'

# library(cancR)
#
# #Competing risks
# n <- 300
# set.seed(1)
# cdf <- riskRegression::sampleData(n, outcome="competing.risks")
# cdf$time <- round(cdf$time,1)*12
# cdf$X1 <- factor(rbinom(n, prob = c(0.3,0.4) , size = 2), labels = paste0("T",0:2))
#
# mod <- crrstrat(cdf, time, event, X2, type = "uni", surv=F)
#
# #ClusterCI
# set.seed(1)
# n=1000
# df <- riskRegression::sampleData(n) %>%
#   mutate(id = sample(seq(1,700), size = n, replace=TRUE),
#          time = time*30,
#          across(c(X6:X10), ~ cut(., breaks = unique(quantile(., c(0, 0.25,0.5,0.75, 1))))),
#          across(c(X6:X10), ~ as.factor(.))) %>%
#   drop_na(time, event, X1, X2, X3, X6, X7) %>%
#   as.data.frame()
# (t <- clusterci(df, time, event, X1, vars = c(X2, X3, X6), cluster = id, time = 90, breaks = 10))
#
# #Pstrat
# pmod <- pstrat(cdf, time, event, X2)
#
#
# crrplot(mod, outcome = "Test", printres = "estrd")
#
# crrplot(t, fu = 90, breaks = 10, time_unit = "days", outcome = "Explantation", labgrp = c("Subpectoral", "Prepectoral"), printres = "estrd")
#
# crrplot(pmod, outcome = "Test")

plotR <- function(list,
                    y=100,
                    col=c("#9B62B8", "#224B87", "#67A8DC", "#D66ACE"),
                    outcome = "",
                    ylab = "Risk of Event (%)",
                    labpos = rep(0,4),
                    printres = "est",
                    labgrp = grps,
                    fu = 120,
                    breaks = 12,
                    time_unit = "m2y",
                    respos.x = 0,
                    respos.y = 0,
                    ylab.pos = 0,
                    legend.pos = c(0.5,0.95),
                    tscale = 1,
                    ndigits_est = 1,
                    ndigits_res = 1,
                    censur=F) {


  if (max(list$Life_table$time) < fu) {
    stop(
      "Follow-up time is not equal in the crrstrat/pstrat/clusterci and the crrplot"
    )
  }

  time_unit <- match.arg(time_unit, c("m2y", "d2m", "d2y", "days", "months", "years"))

  tab <- as.data.frame(list$Life_table) %>% filter(time %in% seq(0,fu,breaks))

  if(ncol(tab)==7) {
    tab$stratum = "test"
    est <- list$Life %>% mutate(stratum = "test") %>%
      mutate(across(c(est:upper), ~ ./100))
    plot <- list$Plot_data %>% mutate(stratum = "test")
    gr = "stratum"
    grps = "test"
  } else if(class(list) == "CRlist") {
    est <- list$Risks %>% rename(stratum = 2)
    plot <- list$Plot_data %>% rename(stratum = 2)
    diff <- list$Risk_differences
    hr <- list$HR
    gr <- colnames(list$Life_table[1])
  }  else if(class(list) == "CLUSTERCI") {
    est <- list$Risks %>% rename(stratum = 2)
    plot <- list$Plot_data %>% rename(stratum = 2)
    diff <- list$Risk_differences
    gr <- colnames(list$Life_table[1])

  } else {
    est <- list$Life_table %>% rename(stratum = 2) %>% select(time, stratum, est, lower, upper) %>%
      mutate(across(c(est:upper), ~ ./100))
    plot <- list$Plot_data %>% rename(stratum = 2)
    gr <- colnames(list$Life_table[2])
  }

  if(censur) {
    tab <- tab %>% mutate(across(c(cumsum, n.risk), ~ ifelse(between(., 1, 5), "<5", .)))
  }

  grps <- paste(unique(est[, "stratum"]))

  surv = list$Surv

  if(list$Plot_data[1,"est"] == 1) {
    survscale <- "OS"
  } else {
    survscale <- "AM"
  }

  x=y/100
  tot = x+(x*0.6)

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

  p <-
    ggplot(plot, aes(x=time, y=est, color = stratum, fill = stratum)) +
    geom_segment(x = -1, xend=fu*1.04, y=-(x*0.04), yend=-(x*0.04), color = "Black", linewidth = 1.5) +
    geom_segment(x = -1, xend=-1, y=-(x*0.04), yend=x, color = "Black", linewidth = 1.5) +
    geom_step(linewidth = 1.5) +
    scale_color_manual(values = c(col[length(grps):1]), labels = labgrp) +
    scale_fill_manual(values = c(col[length(grps):1]), labels = labgrp) +
    geom_stepribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
    coord_cartesian(xlim=c(-(fu*0.1),fu), ylim = c(-(x*0.6),1.2*x)) +
    scale_y_continuous(limits = c(-(0.8*x),1.06)) +
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
  if(length(grps) == 1) {
    p + theme(legend.position = "none")
  }

  #Labels
  p <-
  p + annotate("text", x=fu/2, y = -(tot*0.10), label = unit, size = 6*tscale) +
  annotate("text", x=-(fu*0.11)-ylab.pos, y = x/2, label = ylab, size = 6*tscale, angle = 90) +
  annotate("text", x=seq(0,fu,breaks), y=-(tot*0.06), label=round(seq(0,fu,breaks)/u,0), size = 6*tscale)
  if(y>=50){
    p <- p + annotate("text", x=-(fu*0.03), y=seq(0,x,0.1), label = paste(seq(0,x*100,10), "%", sep=""), size = 6*tscale, hjust="right")
  } else if(y<=10) {
    p <- p + annotate("text", x=-(fu*0.03), y=seq(0,x,0.01), label = paste(seq(0,x*100,1), "%", sep=""), size = 6*tscale, hjust="right")
  } else if(y <=1) {
    p <- p + annotate("text", x=-(fu*0.03), y=seq(0,x,0.001), label = paste(seq(0,x*100,0.1), "%", sep=""), size = 6*tscale, hjust="right")
  } else {
    p <- p + annotate("text", x=-(fu*0.03), y=seq(0,x,0.05), label = paste(seq(0,x*100,5), "%", sep=""), size = 6*tscale, hjust="right")
  }
  p <- p + annotate("text", x=-1, y=x*1.06, label=outcome, size = 7*tscale, hjust="left")
  #Risk table numbers
  if(length(grps)==1) {
    p <- p +
      annotate("text", label = tab[,"cumsum"], x = seq(0,fu,12), y = -(tot*0.21), color = col[1], size = 5*tscale) +
      annotate("text", label = tab[,"n.risk"], x = seq(0,fu,12), y = -(tot*0.33), color = col[1], size = 5*tscale)
  }

  if(length(grps)==2) {
    p <- p + annotate("text", label = tab[tab[,gr] %in% grps[2],"cumsum"], x = seq(0,fu,breaks), y = -(tot*0.19), color = col[1], size = 5*tscale) +
      annotate("text", label = tab[tab[,gr] %in% grps[1],"cumsum"], x = seq(0,fu,breaks), y = -(tot*0.23), color = col[2], size = 5*tscale) +
      annotate("text", label = tab[tab[,gr] %in% grps[2],"n.risk"], x = seq(0,fu,breaks), y = -(tot*0.31), color = col[1], size = 5*tscale) +
      annotate("text", label = tab[tab[,gr] %in% grps[1],"n.risk"], x = seq(0,fu,breaks), y = -(tot*0.35), color = col[2], size = 5*tscale)
  }
  if(length(grps)==3) {
    p <- p +
      annotate("text", label = tab[tab[,gr] %in% grps[3],"cumsum"], x = seq(0,fu,12), y = -(tot*0.185), color = col[1], size = 4.5*tscale) +
      annotate("text", label = tab[tab[,gr] %in% grps[2],"cumsum"], x = seq(0,fu,12), y = -(tot*0.21), color = col[2], size = 4.5*tscale) +
      annotate("text", label = tab[tab[,gr] %in% grps[1],"cumsum"], x = seq(0,fu,12), y = -(tot*0.235), color = col[3], size = 4.5*tscale) +
      annotate("text", label = tab[tab[,gr] %in% grps[3],"n.risk"], x = seq(0,fu,12), y = -(tot*0.305), color = col[1], size = 4.5*tscale) +
      annotate("text", label = tab[tab[,gr] %in% grps[2],"n.risk"], x = seq(0,fu,12), y = -(tot*0.33), color = col[2], size = 4.5*tscale) +
      annotate("text", label = tab[tab[,gr] %in% grps[1],"n.risk"], x = seq(0,fu,12), y = -(tot*0.355), color = col[3], size = 4.5*tscale)
  }
  if(length(grps)==4) {
    p <- p +
      annotate("text", label = tab[tab[,gr] %in% grps[4],"cumsum"], x = seq(0,fu,12), y = -(tot*0.18), color = col[1], size = 3.5*tscale) +
      annotate("text", label = tab[tab[,gr] %in% grps[3],"cumsum"], x = seq(0,fu,12), y = -(tot*0.20), color = col[2], size = 3.5*tscale) +
      annotate("text", label = tab[tab[,gr] %in% grps[2],"cumsum"], x = seq(0,fu,12), y = -(tot*0.22), color = col[3], size = 3.5*tscale) +
      annotate("text", label = tab[tab[,gr] %in% grps[1],"cumsum"], x = seq(0,fu,12), y = -(tot*0.24), color = col[4], size = 3.5*tscale) +
      annotate("text", label = tab[tab[,gr] %in% grps[4],"n.risk"], x = seq(0,fu,12), y = -(tot*0.30), color = col[1], size = 3.5*tscale) +
      annotate("text", label = tab[tab[,gr] %in% grps[3],"n.risk"], x = seq(0,fu,12), y = -(tot*0.32), color = col[2], size = 3.5*tscale) +
      annotate("text", label = tab[tab[,gr] %in% grps[2],"n.risk"], x = seq(0,fu,12), y = -(tot*0.34), color = col[3], size = 3.5*tscale) +
      annotate("text", label = tab[tab[,gr] %in% grps[1],"n.risk"], x = seq(0,fu,12), y = -(tot*0.36), color = col[4], size = 3.5*tscale)

  }

  #Risk table segments
  p <- p +
    geom_segment(x = 0, xend=2, y=-(tot*0.27), yend=-(tot*0.27), color = "#616161", linewidth = 1) +               #Kort midt
    geom_segment(x = 0, xend=0, y=-(tot*0.17), yend=-(tot*0.15), color = "#616161", linewidth = 1) +               #Ve oppe
    geom_segment(x = 0, xend=2, y=-(tot*0.15), yend=-(tot*0.15), color = "#616161", linewidth = 1) +               #Kort oppe
    geom_segment(x = 0, xend=fu*1.04, y=-(tot*0.39), yend=-(tot*0.39), color = "#616161", linewidth = 1) +        #Lang nede
    geom_segment(x = 0, xend=0, y=-(tot*0.39), yend=-(tot*0.37), color = "#616161", linewidth = 1) +          #Ve nede
    geom_segment(x = 0, xend=0, y=-(tot*0.255), yend=-(tot*0.285), color = "#616161", linewidth = 1) +        #Ve midt
    annotate("text", label = "Cumulative events", x = 3, y = -(tot*0.148), color = "#616161", size = 6*tscale, hjust="left") +
    annotate("text", label = "At risk", x = 3, y = -(tot*0.268), color = "#616161", size = 6*tscale, hjust="left")

  #1 GROUP
  if(length(grps)==1) {
    p <- p + theme(legend.position = "none")
    #Estimates
    # annotate("text", x=60, y=ifelse(surv & survscale == "OS",est$lower[est$time %in% 60]-0.05*x,est$upper[est$time %in% 60]+0.05*x)+labpos[1], label = paste(c(format(round(est$est[est$time%in%60]*100,ndigits_est), nsmall=1),"%"),collapse=""), size = 5*tscale, col = col[1])

    if(str_detect(printres, "est")) {
      p <- p + annotate("text", x=fu*0.04+respos.x, y=ifelse(surv & survscale == "OS", 0.2*x+respos.y, 0.95*x+respos.y), label = paste0(fu/u, "-", ifelse(unit %in% "Years", str_remove(unit, "s"), unit), " Risk", collapse=""), fontface = 2, hjust="left", size = 5*tscale) +
        annotate("text", x=fu*0.04+respos.x, y=ifelse(surv & survscale == "OS", 0.15*x+respos.y, 0.90*x+respos.y), label = paste(c(format(round(est$est[est$time%in%fu]*100,ndigits_est), nsmall=1),"% (95%CI ", format(round(est$lower[est$time%in%fu]*100, ndigits_res), nsmall=1),"-", format(round(est$upper[est$time%in%fu]*100, ndigits_res), nsmall=1),")"), collapse=""), size = 5*tscale, hjust="left", fontface=1) +
        annotate("segment", x=fu*0.01+respos.x, xend=fu*0.03+respos.x, y=ifelse(surv & survscale == "OS", 0.15*x, 0.90*x), yend=ifelse(surv & survscale == "OS", 0.15*x, 0.90*x), color = c(col[2:1]), linewidth = 1.5)
    }
  }

  # 2 groups
  if(length(grps)==2) {

    #Line labels
    # p <- p +  annotate("text", x=60, y=ifelse(surv & survscale == "OS", pmin(0.995,est$upper[est$time %in% 60 & est$stratum %in% first(est$stratum)]+x*0.05),pmax(0.015,est$lower[est$time %in% 60 & est$stratum %in% first(est$stratum)]-x*0.05)) + labpos[1], label = paste(c(format(round(est$est[est$time%in%60 & est$stratum %in% first(est$stratum)]*100,ndigits_est), nsmall=1),"%"),collapse=""), size = 5*tscale, col = col[2]) +
    #   annotate("text", x=60, y=ifelse(surv & survscale == "OS", est$lower[est$time %in% 60 & est$stratum %in% last(est$stratum)]-x*0.05,est$upper[est$time %in% 60 & est$stratum %in% last(est$stratum)]+x*0.05) + labpos[1], label = paste(c(format(round(est$est[est$time%in%60 & est$stratum %in% last(est$stratum)]*100,ndigits_est), nsmall=1),"%"),collapse=""), size = 5*tscale, col = col[1])
    if(str_detect(printres, "est")) {
      p <- p + annotate("text", x=fu*0.04+respos.x, y=ifelse(surv & survscale == "OS", 0.2*x+respos.y, 0.95*x+respos.y), label = paste0(fu/u, "-", ifelse(unit %in% "Years", str_remove(unit, "s"), unit), " Risk", collapse=""), fontface = 2, hjust="left", size = 5*tscale) +
        annotate("text", x=fu*0.04+respos.x, y=ifelse(surv & survscale == "OS", 0.15*x+respos.y, 0.90*x+respos.y), label = paste(c(format(round(est$est[est$time%in%fu & est$stratum %in% grps[1]]*100,ndigits_est), nsmall=1),"% (95%CI ", format(round(est$lower[est$time%in%fu & est$stratum %in% grps[1]]*100, ndigits_res), nsmall=1),"-", format(round(est$upper[est$time%in%fu & est$stratum %in% grps[1]]*100, ndigits_res), nsmall=1),")"), collapse=""), size = 5*tscale, hjust="left", fontface=1) +
        annotate("text", x=fu*0.04+respos.x, y=ifelse(surv & survscale == "OS", 0.1*x+respos.y, 0.85*x+respos.y), label = paste(c(format(round(est$est[est$time%in%fu & est$stratum %in% grps[2]]*100,ndigits_est), nsmall=1),"% (95%CI ", format(round(est$lower[est$time%in%fu & est$stratum %in% grps[2]]*100, ndigits_res), nsmall=1),"-", format(round(est$upper[est$time%in%fu & est$stratum %in% grps[2]]*100, ndigits_res), nsmall=1),")"), collapse=""), size = 5*tscale, hjust="left", fontface=1) +
        annotate("segment", x=fu*0.01+respos.x, xend=fu*0.03+respos.x, y=ifelse(rep(surv & survscale == "OS",2), (c(0.15*x,0.1*x)), c(0.90*x,0.85*x)), yend=ifelse(rep(surv & survscale == "OS",2), (c(0.15*x,0.1*x)), c(0.90*x,0.85*x)), color = c(col[2:1]), linewidth = 1.5)
    }
    if(class(list) %in% c("CRlist", "CLUSTERCI")) {
      if(str_detect(printres, "hr")) {
        p <-  p+ annotate("text", x=fu*0.04+respos.x, y = ifelse(surv & survscale == "OS", x*0.05+respos.y, x*0.8+respos.y), label = hr$print, size = 5*tscale, hjust = "left", fontface=3)
      }
      if(str_detect(printres, "rd")) {
        p <- p+ annotate("text", x=fu*0.04+respos.x, y = ifelse(surv & survscale == "OS", x*0.05+respos.y, x*0.8+respos.y), label = paste0(ifelse(str_detect(printres, "est"), "RD = ", paste0(fu/u, "-", ifelse(unit %in% "Years", str_remove(unit, "s"), unit), " RD = ", collapse="")), format(round(diff$diff[diff$time%in%fu], ndigits_res), nsmall=1), "% (95%CI ", format(round(diff$lower[diff$time%in%fu], ndigits_res), nsmall=1),"-", format(round(diff$upper[diff$time%in%fu], ndigits_res), nsmall=1),"), ", diff$p.value[diff$time%in%fu]), size = 5*tscale, hjust = "left", fontface=3)
      }
    }

  }

  if(length(grps) == 3) {

    if(printres=="est") {
      p <- p + annotate("text", x=fu*0.04, y=ifelse(surv & survscale == "OS", 0.2*x+respos.y, 0.95*x+respos.y), label=paste0(fu/u, "-", ifelse(unit %in% "Years", str_remove(unit, "s"), unit), " Risk", collapse=""), fontface = 2, hjust="left", size = 5*tscale) +
        annotate("text", x=fu*0.04, y=ifelse(surv & survscale == "OS", 0.15*x+respos.y, 0.90*x+respos.y), label = paste(c(format(round(est$est[est$time%in%fu & est$stratum %in% grps[1]]*100,ndigits_est), nsmall=1),"% (95%CI ", format(round(est$lower[est$time%in%fu & est$stratum %in% grps[1]], ndigits_res), nsmall=1),"-", format(round(est$upper[est$time%in%fu & est$stratum %in% grps[1]], ndigits_res), nsmall=1),")"), collapse=""), size = 5*tscale, hjust="left", fontface=1) +
        annotate("text", x=fu*0.04, y=ifelse(surv & survscale == "OS", 0.1*x+respos.y, 0.85*x+respos.y), label = paste(c(format(round(est$est[est$time%in%fu & est$stratum %in% grps[2]]*100,ndigits_est), nsmall=1),"% (95%CI ", format(round(est$lower[est$time%in%fu & est$stratum %in% grps[2]], ndigits_res), nsmall=1),"-", format(round(est$upper[est$time%in%fu & est$stratum %in% grps[2]], ndigits_res), nsmall=1),")"), collapse=""), size = 5*tscale, hjust="left", fontface=1) +
        annotate("text", x=fu*0.04, y=ifelse(surv & survscale == "OS", 0.05*x+respos.y, 0.80*x+respos.y), label = paste(c(format(round(est$est[est$time%in%fu & est$stratum %in% grps[3]]*100,ndigits_est), nsmall=1),"% (95%CI ", format(round(est$lower[est$time%in%fu & est$stratum %in% grps[3]], ndigits_res), nsmall=1),"-", format(round(est$upper[est$time%in%fu & est$stratum %in% grps[3]], ndigits_res), nsmall=1),")"), collapse=""), size = 5*tscale, hjust="left", fontface=1) +
        annotate("segment", x=fu*0.01, xend=fu*0.03, y=ifelse(rep(surv & survscale == "OS",3), (c(0.15*x,0.1*x,0.05*x)), c(0.90*x,0.85*x,0.80*x)), yend=ifelse(rep(surv & survscale == "OS",3), (c(0.15*x,0.1*x,0.05*x)), c(0.90*x,0.85*x,0.80*x)), color = c(col[3:1]), linewidth = 1.5)
    }

    if(printres=="rd" & class(list) %in% "CRlist") {
      p <- p + annotate("text", x=fu*0.04, y=ifelse(surv & survscale == "OS", 0.2*x+respos.y, 0.95*x+respos.y), label=paste0(fu/u, "-", ifelse(unit %in% "Years", str_remove(unit, "s"), unit), " Risk", collapse=""), fontface = 2, hjust="left", size = 5*tscale) +
        annotate("text", x=fu*0.04, y=ifelse(surv & survscale == "OS", 0.15*x+respos.y, 0.90*x+respos.y), label = paste(c(format(round(est$est[est$time%in%fu & est$stratum %in% grps[1]]*100,ndigits_est), nsmall=1),"%, reference"),collapse=""), size = 5*tscale, hjust="left", fontface=1) +
        annotate("text", x=fu*0.04, y=ifelse(surv & survscale == "OS", 0.1*x+respos.y, 0.85*x+respos.y), label = paste(c(format(round(est$est[est$time%in%fu & est$stratum %in% grps[2]]*100,ndigits_est), nsmall=1),"%, ",
                                                                                                                         "RD = ",
                                                                                                                         format(round(first(diff$diff[diff$time%in%fu]), ndigits_res), nsmall=1), "% (95%CI ", format(round(first(diff$lower[diff$time%in%fu]), ndigits_res), nsmall=1),"-", format(round(first(diff$upper[diff$time%in%fu]), ndigits_res), nsmall=1),"), ", first(diff$p.value[diff$time%in%fu])),collapse=""), size = 5*tscale, hjust="left", fontface=1) +
        annotate("text", x=fu*0.04, y=ifelse(surv & survscale == "OS", 0.05*x+respos.y, 0.80*x+respos.y), label = paste(c(format(round(est$est[est$time%in%fu & est$stratum %in% grps[3]]*100,ndigits_est), nsmall=1),"%, ",
                                                                                                                          "RD = ",
                                                                                                                          format(round(nth(diff$diff[diff$time%in%fu], 2), ndigits_res), nsmall=1), "% (95%CI ", format(round(nth(diff$lower[diff$time%in%fu],2), ndigits_res), nsmall=1),"-", format(round(nth(diff$upper[diff$time%in%fu],2), ndigits_res), nsmall=1),"), ", nth(diff$p.value[diff$time%in%fu],2)),collapse=""), size = 5*tscale, hjust="left", fontface=1) +
        annotate("segment", x=fu*0.01, xend=fu*0.03, y=ifelse(rep(surv,3), (c(0.15*x,0.1*x,0.05*x)), c(0.90*x,0.85*x,0.80*x)), yend=ifelse(rep(surv,3), (c(0.15*x,0.1*x,0.05*x)), c(0.90*x,0.85*x,0.80*x)), color = c(col[3:1]), linewidth = 1.5)
    }

  }

  if(length(grps) == 4) {
    p <- p + annotate("text", x=fu*0.04, y=ifelse(surv & survscale == "OS", 0.25*x+respos.y, 0.95*x+respos.y), label=paste0(fu/u, "-", ifelse(unit %in% "Years", str_remove(unit, "s"), unit), " Risk", collapse=""), fontface = 2, hjust="left", size = 5*tscale) +
      annotate("text", x=fu*0.04, y=ifelse(surv & survscale == "OS", 0.20*x+respos.y, 0.90*x+respos.y), label = paste(c(format(round(est$est[est$time%in%fu & est$stratum %in% grps[1]]*100,ndigits_est), nsmall=1),"% (95%CI ", format(round(est$lower[est$time%in%fu & est$stratum %in% grps[1]], ndigits_res), nsmall=1),"-", format(round(est$upper[est$time%in%fu & est$stratum %in% grps[1]], ndigits_res), nsmall=1),")"), collapse=""), size = 5*tscale, hjust="left", fontface=1) +
      annotate("text", x=fu*0.04, y=ifelse(surv & survscale == "OS", 0.15*x+respos.y, 0.85*x+respos.y), label = paste(c(format(round(est$est[est$time%in%fu & est$stratum %in% grps[2]]*100,ndigits_est), nsmall=1),"% (95%CI ", format(round(est$lower[est$time%in%fu & est$stratum %in% grps[2]], ndigits_res), nsmall=1),"-", format(round(est$upper[est$time%in%fu & est$stratum %in% grps[2]], ndigits_res), nsmall=1),")"), collapse=""), size = 5*tscale, hjust="left", fontface=1) +
      annotate("text", x=fu*0.04, y=ifelse(surv & survscale == "OS", 0.10*x+respos.y, 0.80*x+respos.y), label = paste(c(format(round(est$est[est$time%in%fu & est$stratum %in% grps[3]]*100,ndigits_est), nsmall=1),"% (95%CI ", format(round(est$lower[est$time%in%fu & est$stratum %in% grps[3]], ndigits_res), nsmall=1),"-", format(round(est$upper[est$time%in%fu & est$stratum %in% grps[3]], ndigits_res), nsmall=1),")"), collapse=""), size = 5*tscale, hjust="left", fontface=1) +
      annotate("text", x=fu*0.04, y=ifelse(surv & survscale == "OS", 0.05*x+respos.y, 0.75*x+respos.y), label = paste(c(format(round(est$est[est$time%in%fu & est$stratum %in% grps[4]]*100,ndigits_est), nsmall=1),"% (95%CI ", format(round(est$lower[est$time%in%fu & est$stratum %in% grps[4]], ndigits_res), nsmall=1),"-", format(round(est$upper[est$time%in%fu & est$stratum %in% grps[4]], ndigits_res), nsmall=1),")"), collapse=""), size = 5*tscale, hjust="left", fontface=1) +
      annotate("segment", x=fu*0.01, xend=fu*0.03, y=ifelse(rep(surv,4), (c(0.20*x,0.15*x,0.10*x,0.05*x)), c(0.90*x,0.85*x,0.80*x, 0.75*x)), yend=ifelse(rep(surv,4), (c(0.20*x,0.15*x,0.10*x,0.05*x)), c(0.90*x,0.85*x,0.80*x, 0.75*x)), color = c(col[4:1]), linewidth = 1.5)
  }


 return(p)

}
