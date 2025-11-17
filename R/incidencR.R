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
#' @param cause cause of interest, default = 1
#' @param survtime Whether median time to event should be calculated (default = F)
#' @param proportions Whether risk of event in different windows should be estimated (default = F)
#' @param conditional Whether conditional risk at the time horizon should be calculated (default = F)
#'
#' @return
#' Life_table: Event table \cr
#' Plot_data: Data for plotting CIF curves \cr
#' Median_surv: Median survival time \cr
#' Surv: Indicator for the type of time-to-event analysis
#'
#' @export
#'
#
# i <- incidencR(analysis_df,
#                timevar = ttt,
#                event = event,
#                time = 60,
#                proportions = T,
#                conditional = T)

incidencR <- function(data,
                   timevar,
                   event,
                   group,
                   survscale = "AM",
                   time=60,
                   breaks = 12,
                   digits = 4,
                   event.digits = 2,
                   cause = 1,
                   survtime = T,
                   proportions = F,
                   conditional = F) {

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
      dat %>% mutate(grp = as.factor(" "))

  } else {
    group_c <- dat %>% select({{group}}) %>% names()

    if(!is.factor(data[[group_c]])) {

      return(cat("Error:", group_c, "is not a factor. Convert using the factR() function"))

    }

  }



  dat <-
    dat %>% drop_na(!!sym(group_c))%>%
    as.data.frame()

    group_levels <- levels(dat[,group_c])

    if(length(group_levels > 2)) {
      multi = TRUE
    } else {
      multi = FALSE
    }

    out.list <- list()

    prod <- prodlim(as.formula(paste0("Hist(", timevar_c, ",", event_c, ") ~", group_c, collapse = "")), data=dat)

    tab <- summary(prod, times = seq(0,horizon,1), intervals = TRUE, cause=cause) %>%
      group_by(!!sym(group_c)) %>%
      mutate(cumsum = cumsum(n.event)) %>%
      ungroup() %>%
      rename(time = time1) %>%
      rename_with(~ paste0(str_replace(.x, "cuminc|surv", "est")), matches("cuminc|surv")) %>%
      select(-time0, -n.lost) %>%
      as.data.frame()

    out.list[["table"]] <- tab

    est <- tab %>%
      filter(time %in% seq(0,horizon,breaks)) %>%
      select(time, !!sym(group_c), est:upper) %>%
      rename(se = se.est) %>%
      mutate(across(c(est:upper), ~round(.,digits)))




    plot_data <-
      summary(prod, times = unique(round(sort(c(0,dat[dat[, event_c] == 1 & dat[,timevar_c] < horizon, timevar_c],horizon)),event.digits)), intervals = TRUE, cause=cause) %>%
      select(-time0, -n.lost, -contains("se.")) %>%
      rename(time = time1) %>%
      rename_with(~ paste0(str_replace(.x, "cuminc|surv", "est")), matches("cuminc|surv")) %>%
      as.data.frame()

    if(surv & survscale == "AM") {
      est <- est %>% mutate(across(c(est, lower, upper), ~1 - .)) %>%
        rename(upr = lower,
               lwr = upper) %>%
        rename(upper = upr, lower = lwr)

      plot_data <- plot_data %>% mutate(across(c(est, lower, upper), ~1 - .))%>%
        rename(upr = lower,
               lwr = upper) %>%
        rename(upper = upr, lower = lwr)

    }

    out.list[["risks"]] <- est
    out.list[["plot_data"]] <- plot_data

    if(survtime) {

      msurv <- list2DF(quantile(prodlim(as.formula(paste0("Hist(", timevar_c, ", ", event_c, ") ~", group_c, collapse = "")), data = dat[dat[[event_c]] == 1,]), 0.5)) %>% select(-q)

      out.list[["time_to_event"]] <- msurv


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

    out.list[["counts"]] <- counts

    #Risks
    if(length(group_levels) > 1) {
    est_horizon <- est[est$time == horizon,]
    combos <- combn(seq_along(group_levels), 2)

    rd <- bind_rows(
      lapply(c(1:ncol(combos)), function(g) {

        r <- est_horizon[as.vector(combos[, c(g)]),]

        diff <- r$est[2] - r$est[1]
        diffse <- sqrt(r$se[2]^2 + r$se[1]^2)
        pval <- 2*pnorm(abs(diff/diffse), lower=FALSE)

        data.frame(time = 120,
                   A = r[[group_c]][1],
                   B = r[[group_c]][2],
                   diff = diff,
                   se = diffse,
                   lower = diff - (1.96 * diffse),
                   upper = diff + (1.96 * diffse),
                   p.value = pvertR(pval),
                   p.exact = pval) %>%
          mutate(across(c(diff:upper), ~.*100),
                 across(c(diff:upper), ~ round(.,digits-2)))

      })
    )

    out.list[["difference"]] <- rd

    }

    if(proportions) {
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

      out.list[["event_proportions"]] <- prop

    }

    if(conditional) {
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


      out.list[["conditional"]] <- conditional

    }

    out.list[["info"]] <- list("timevar" = timevar_c,
                               "event" = event_c,
                               "group" = group_c,
                               "group_levels" = group_levels,
                               "surv" = surv,
                               "survscale" = survscale,
                               "time" = horizon,
                               "breaks" = breaks,
                               "event.digits" = event.digits,
                               "multi" = multi,
                               "cause" = cause)
  class(out.list)<- "incidencR"
  return(out.list)


}

