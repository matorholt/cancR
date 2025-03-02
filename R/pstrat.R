#' Life-table
#' @description
#' Convenience wrapper for the prodlim function in riskRegression
#'
#'
#' @param data dataframe
#' @param timevar time to event column
#' @param event event column with either 0/1 structure or 0/1/2 in case of competing risks
#' @param group optional grouping column
#' @param survscale Whether estimates should be presented as Overall survival or All-Cause Mortality
#' @param horizon time horizon (default=120 months)
#'
#' @return
#' Life_table: Event table \cr
#' Plot_data: Data for plotting CIF curves \cr
#' Median_surv: Median survival time \cr
#' Surv: Indicator for the type of time-to-event analysis
#'
#' @export
#'
#'
pstrat <- function(data,
                   timevar,
                   event,
                   group,
                   survscale = "OS",
                   horizon=120) {


  timepoints <- seq(0,horizon,12)

  #Formula generation
  if(missing(group)) {
    lhs <- paste(c("Hist(", paste(substitute(timevar)), ",", paste(substitute(event)), ") ~"), collapse = "")
    rhs_null <- "1"
    form_null <- as.formula(paste(c(lhs, rhs_null), collapse = ""))
    pobj <- prodlim(form_null, data = data)
  }

  else {
    lhs <- paste(c("Hist(", paste(substitute(timevar)), ",", paste(substitute(event)), ") ~"), collapse = "")
    rhs_g <- paste(substitute(group))
    form_g <- as.formula(paste(c(lhs, rhs_g), collapse = ""))
    pobj <- prodlim(form_g, data=data)

  }

  if(pobj$model == "survival"){
    tab <- as.data.frame(summary(pobj, cause=1, intervals=T, times = timepoints))

    if(survscale != "OS") {
      tab <- tab %>% group_by({{group}}) %>%
        mutate(across(c(surv, lower, upper), ~ 1 - .),
               cumsum = cumsum(n.event),
               lower = ifelse(surv == 0, 0, lower)) %>%
        rename(upr = lower,
               lower = upper) %>%
        rename(upper = upr)
    } else {
      tab <- tab %>% group_by({{group}}) %>%
        mutate(cumsum = cumsum(n.event),
               lower = ifelse(surv == 1, 1, lower))
    }
    tab <- tab %>%
      select(time1, {{group}}, n.risk, n.event, surv, lower, upper, cumsum)%>%
      rename(time = time1,
             est = surv) %>%
      ungroup()
    plot <- as.data.frame(summary(pobj, cause=1, intervals=T, times = seq(0,horizon,0.1)))

    if(survscale != "OS") {
      plot <- plot %>% group_by({{group}}) %>%
        mutate(across(c(surv, lower, upper), ~ 1 - .),
               cumsum = cumsum(n.event),
               lower = ifelse(surv == 0, 0, lower)) %>%
        rename(upr = lower,
               lower = upper) %>%
        rename(upper = upr)
    } else {
      plot <- plot %>% group_by({{group}}) %>%
        mutate(cumsum = cumsum(n.event),
               lower = ifelse(surv == 1, 1, lower))
    }
    plot <- plot %>%
      select(time1, {{group}}, n.risk, n.event, surv, lower, upper, cumsum)%>%
      rename(time = time1,
             est = surv) %>%
      ungroup()
    if(missing(group)) {
      qs <- quantile(pobj, 0.5)
      msurv <- data.frame(q = qs$q,
                          time = qs$quantile,
                          q1 = qs$lower,
                          q3 = qs$upper)
    } else {
      qs <- quantile(pobj, as.numeric(unlist(tab %>%filter(time %in%horizon)%>%select(est)))/2)
      msurv <- data.frame(grp = qs[1],
                          q = qs$q,
                          time = qs$quantile,
                          q1 = qs$lower,
                          q3 = qs$upper)%>%
        slice(1,3,5,8)
    }

  }
  else {
    tab <- as.data.frame(summary(pobj, cause=1, intervals=T, times = as.numeric(timepoints))) %>% group_by({{group}}) %>%
      mutate(cumsum = cumsum(n.event))%>%
      select(time1, {{group}}, n.risk, n.event, cuminc, lower, upper, cumsum)%>%
      rename(time = time1,
             est = cuminc) %>%
      ungroup()
    plot <- as.data.frame(summary(pobj, cause=1, intervals=T, times = seq(0,horizon,0.1)))%>% group_by({{group}}) %>%
      mutate(cumsum = cumsum(n.event))%>%
      select(time1, {{group}}, n.risk, n.event, cuminc, lower, upper, cumsum)%>%
      rename(time = time1,
             est = cuminc) %>%
      ungroup()
    if(missing(group)) {
      qs <- quantile(pobj, as.numeric(unlist(tab %>%filter(time %in%horizon)%>%select(est)))/2)
      msurv <- data.frame(q = qs$q,
                          time = qs$quantile,
                          q1 = qs$lower,
                          q3 = qs$upper)
    } else {
      qs <- quantile(pobj, as.numeric(unlist(tab %>%filter(time %in%horizon)%>%select(est)))/2)
      msurv <- data.frame(grp = qs[2],
                          q = qs$q,
                          time = qs$quantile,
                          q1 = qs$lower,
                          q3 = qs$upper)%>%
        slice(1,3,5,8)
    }
  }

  tab <- as.data.frame(tab %>% mutate(across(c(est:upper), ~ as.numeric(format(round(.*100, 2), nsmall=1)))) %>% ungroup())

  if(pobj$model %in% "survival") {
    surv = TRUE
  } else { surv = FALSE}



  list <- list(tab, plot, msurv, surv)
  names(list)<- c("Life_table", "Plot_data", "Msurv", "Surv")
  class(list)<- "Plist"
  return(list)

}
