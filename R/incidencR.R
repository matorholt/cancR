#' Estimate absolute risks in a single group with competing risks
#'
#' @param data dataframe
#' @param timevar time to event column
#' @param event event column with either 0/1 structure or 0/1/2 in case of competing risks
#' @param group optional grouping column
#' @param survscale Whether estimates should be presented as Overall survival or All-Cause Mortality
#' @param time time horizon (default=60 months)
#' @param breaks Interim time points of interest. Defaults to 12 months (1-year gaps)
#' @param digits for rounding of eventtimes
#' @param event.digits whether eventtimes should be rounded. Default is 2 to preserve exact times
#' @param quantiles Whether median time to event should be calculated (default = TRUE)
#'
#' @return
#' Life_table: Event table \cr
#' Plot_data: Data for plotting CIF curves \cr
#' Median_surv: Median survival time \cr
#' Surv: Indicator for the type of time-to-event analysis
#'
#' @export
#'

# i <- incidencR(analysis_df,
#           timevar = ttt,
#           event = event,
#           group = X3,
#           time = 60)
#
# df <- df_out %>%
#   factR(subgroup)
#
# t <- incidencR(df,
#                t_rec,
#                rec,
#                subgroup)
# #
# plotR(i)

incidencR <- function(data,
                   timevar,
                   event,
                   group,
                   survscale = "AM",
                   time=60,
                   breaks = 12,
                   digits = 4,
                   event.digits = 2,
                   quantiles = T) {

  dat <- data

  suppressWarnings(timevar_c <- data %>% select({{timevar}}) %>% names())
  suppressWarnings( event_c <- data %>% select({{event}}) %>% names())
  suppressWarnings(group_c <- data %>% select({{group}}) %>% names())
  horizon <- time

  if(length(levels(dat[,event_c])) == 2) {
    dat[,event_c] <- as.numeric(dat[,event_c]) - 1
  }

  if(length(unique(dat[, event_c])) == 2) {
    surv = TRUE
  } else {
    surv = FALSE
  }

  #Formula generation
  if(missing(group)) {

    group <- sym("grp")
    group_c <- "grp"

    dat <-
      dat %>% mutate(grp = " ")

  } else {
    group_c <- dat %>% select({{group}}) %>% names()

  }

    dat <-
      dat %>% drop_na(!!sym(group_c)) %>%
      mutate(!!sym(group_c) := as.factor(!!sym(group_c)))

    group_levels <- levels(dat[,group_c])

    if(length(group_levels > 2)) {
      multi = TRUE
    } else {
      multi = FALSE
    }

    prod <- prodlim(as.formula(paste0("Hist(", timevar_c, ",", event_c, ") ~", group_c, collapse = "")), data=dat)

    tab <- summary(prod, times = seq(0,horizon,1), intervals = TRUE, cause=1) %>%
      group_by(!!sym(group_c)) %>%
      mutate(cumsum = cumsum(n.event)) %>%
      ungroup() %>%
      rename(time = time1) %>%
      rename_with(~ paste0(str_replace(.x, "cuminc|surv", "est")), matches("cuminc|surv")) %>%
      select(-time0, -n.lost) %>%
      as.data.frame()

    est <- tab %>%
      filter(time %in% seq(0,horizon,breaks)) %>%
      select(time, !!sym(group_c), est:upper) %>%
      rename(se = se.est) %>%
      mutate(across(c(est:upper), ~round(.,digits)))


    plot <-
      summary(prod, times = unique(round(sort(c(0,dat[dat[, event_c] == 1 & dat[,timevar_c] < horizon, timevar_c],horizon)),event.digits)), intervals = TRUE, cause=1) %>%
      select(-time0, -n.lost, -contains("se.")) %>%
      rename(time = time1) %>%
      rename_with(~ paste0(str_replace(.x, "cuminc|surv", "est")), matches("cuminc|surv")) %>%
      as.data.frame()

    if(surv & survscale == "AM") {
      est <- est %>% mutate(across(c(est, lower, upper), ~1 - .)) %>%
        rename(upr = lower,
               lwr = upper) %>%
        rename(upper = upr, lower = lwr)

      plot <- plot %>% mutate(across(c(est, lower, upper), ~1 - .))%>%
        rename(upr = lower,
               lwr = upper) %>%
        rename(upper = upr, lower = lwr)

    }

    if(quantiles) {
      qs <-
        as.data.frame(quantile(prod, tab[tab[, "time"] == horizon, "est"]/2))
      msurv <- tibble(!!sym(group_c) := qs[,group_c],
                      q = qs$q,
                      time = qs$quantile,
                      lwr = qs$lower,
                      upr = qs$upper) %>%
        slice(1,3,5,8)
    }

    counts <-
      bind_cols(
        dat %>% group_by(!!sym(group_c), .drop=FALSE) %>%
          filter(!!sym(event_c) == 1) %>%
          summarise(n.events = n()) %>%
          drop_na(!!sym(group_c)),
        dat %>% group_by(!!sym(group_c)) %>%
          summarise(total = n()) %>%
          select(-!!sym(group_c)))

    prop <- est %>%group_by(!!sym(group_c))%>%
      mutate(est = ifelse(rep(survscale == "OS", n()), 1-est, est),
             before = est / last(est)* 100,
             after = 100-before,
             window = before - lag(before),
             window = ifelse(is.na(window), before, window),
             residual = (last(est)- est),
             rse = sqrt(last(se)^2 +se^2),
             rescil = (pmax((residual - (1.96*rse)), 0)),
             resciu = (pmin((residual +(1.96*rse)),100)),
             across(c(residual, rescil, resciu), ~. * 100))%>%
      select(time, !!sym(group_c), before, after, window, residual, rescil, resciu)%>%
      filter(time < horizon)

    conditional <- rbindlist(lapply(group_levels, function(x){
      tab2 <- tab %>%filter(!!sym(group_c) %in% x)
      risk2 <- est %>%filter(!!sym(group_c) %in% x)
      condi <- rbindlist(lapply(seq(breaks,horizon,breaks), function(t1){

        if(surv & survscale == "OS") {
          cs <- ((risk2$est[risk2$time %in% horizon])/ (risk2$est[risk2$time %in%t1]))
        } else {
          cs <- ((1-risk2$est[risk2$time %in% horizon])/ (1-risk2$est[risk2$time %in% t1]))
        }

        cs.sq <- cs^2
        temp <- tab2 %>% select(n.event, time, n.risk) %>% as.data.frame()
        d <- temp$n.event[temp$time >= t1 &
                            temp$time <= horizon &
                            temp$n.event > 0]

        r <- temp$n.risk[temp$time >= t1 &
                           temp$time <= horizon &
                           temp$n.event > 0]
        dr <- d / (r * (r - d))
        var.cs <- 1 / (log(cs)^2)* sum(dr)
        ci.cs <- round(cs^(exp(c(1, -1)* stats::qnorm(0.975)* sqrt(var.cs))),3)
        if(surv & survscale == "OS") {
          cond <- data.frame(est = round(cs,3),
                             upper = ci.cs[1],
                             lower = ci.cs[2])
        } else {
          cond <- data.frame(est = round(1-cs,3),
                             upper = 1-ci.cs[1],
                             lower = 1-ci.cs[2])

        }
        return(cond)
      })) %>% rbind(risk2[risk2$time %in% horizon ,c("est","lower","upper")], .)%>%
        mutate(grp = x,
               time = seq(0,horizon,breaks))%>%
        rename(!!sym(group_c) := grp) %>%
        select(time, !!sym(group_c), est, lower, upper) %>%
        tibble::remove_rownames()

      return(condi)
    }))

    list <- list("table" = tab,
                 "plot_data" = plot,
                 "risks" = est,
                 "counts" = counts,
                "event_proportions" = prop,
                "conditional" = conditional,
                 "info" = list("timevar" = timevar_c,
                               "event" = event_c,
                               "group" = group_c,
                               "group_levels" = group_levels,
                               "surv" = surv,
                               "survscale" = survscale,
                               "time" = horizon,
                               "breaks" = breaks,
                               "event.digits" = event.digits,
                               "multi" = multi))
  class(list)<- "incidencR"
  return(list)


}

