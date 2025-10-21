#' Causal inference of time-to-event data
#'
#' @description
#' Average treatment effect of point intervention. Wrapper for the ATE function in riskRegression
#'
#'
#' @param data dataframe
#' @param treatment column indicating treatment variable. Currently only binary treatments are supported.
#' @param timevar Time variable
#' @param event Status indicator
#' @param vars variables that the treatment, censoring and event models should include
#' @param outcome.vars variables that should only be included in the outcome models
#' @param time Time horizon of interest. Defaults to 60 (e.g. 5-years)
#' @param breaks Interim time points of interest. Defaults to 12 months (1-year gaps)
#' @param estimator whether the estimator should be "IPTW", "GFORMULA" or "AIPTW" (default)
#' @param survtime Whether median time to event should be calculated (default = F)
#' @param survscale Whether overall survial should be estimated as survival or all-cause mortality (1-survival)
#' @param bins bins for the weights plot, default = 0.5
#' @param digits for rounding of eventtimes
#' @param event.digits whether eventtimes should be rounded. Default is 2 to preserve exact times
#'
#' @return
#' time_to_event: Median survival time \cr
#' table: Event table \cr
#' plot_data: Data for plotting CIF curves \cr
#' models: Model objects (cause1 and cause2) \cr
#' diag: Diagnostics for assessing proportionality \cr
#' risks: Absolute risk estimates at the specified time points \cr
#' differences: Absolute risk difference at the specified time horizons \cr
#' ratios: Absolute risk ratios at the the specified time horizon \cr
#' counts: Event and group counts in the contrasted groups \cr
#' info: information on arguments for extraction
#' @export
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
# tdf <- analysis_df %>%
#   drop_na(X2) %>%
#   mutate(X2 = fct_drop(X2))
#
# t1 <- inferencR(analysis_df,
#                 treatment = X2,
#                 timevar = ttt,
#                 event = event2,
#                 vars = c(X1, X3, X6, X7),
#                 outcome.vars = X10)
#
# plotR(t1)
#
# t2 <- inferencR(analysis_df,
#                 treatment = X2,
#                 timevar = ttt,
#                 event = event,
#                 vars = c(X1, X3, X6, X7),
#                 survscale = "AM")
#
# plotR(t2)

inferencR <- function(data,
                      treatment,
                      timevar,
                      event,
                      vars,
                      outcome.vars,
                      censoring.vars,
                      time = 120,
                      breaks = 12,
                      estimator = "AIPTW",
                      survtime = T,
                      survscale = "AM",
                      bins = 0.5,
                      digits = 4,
                      event.digits = 2) {


  cat("\ninferencR initialized: ", tickR(), "\n")

  dat <- data %>%
    drop_na({{treatment}})
  horizon <- time

  treat_c <- data %>% select({{treatment}}) %>% names()
  timevar_c <- data %>% select({{timevar}}) %>% names()
  event_c <- data %>% select({{event}}) %>% names()

  vars_c <- data %>% select({{vars}}) %>% names()

  ovars_c <- data %>% select({{outcome.vars}}) %>% names()
  cvars_c <- data %>% select({{censoring.vars}}) %>% names()

  levels <- levels(data[[treat_c]])

  #Detect survival outcome
  if(length(levels(dat[,event_c])) == 2) {
    dat[,event_c] <- as.numeric(dat[,event_c]) - 1
  }

  if(length(unique(dat[, event_c])) == 2) {
    surv = TRUE
  } else {
    surv = FALSE
  }

  #RISK TABLE
  prod <- prodlim(as.formula(paste0("Hist(", timevar_c, ", ", event_c, ") ~", treat_c, collapse = "")), data=dat)
  tab <- summary(prod, times = seq(0,horizon,1), intervals = TRUE, cause=1) %>%
    group_by(!!sym(treat_c)) %>%
    mutate(cumsum = cumsum(n.event)) %>%
    ungroup() %>%
    rename(time = time1) %>%
    select(-time0, -n.lost, -contains("se.")) %>%
    as.data.frame()

  if(survtime) {

    msurv <- list2DF(quantile(prodlim(as.formula(paste0("Hist(", timevar_c, ", ", event_c, ") ~", treat_c, collapse = "")), data = dat[dat[[event_c]] == 1,]), 0.5)) %>% select(-q)

  }

  #MODELS

  tvars <- paste0(vars_c, collapse = " + ")
  cvars <- paste0(c(treat_c, vars_c, cvars_c), collapse = " + ")
  ovars <- paste0(c(treat_c, vars_c, ovars_c), collapse = " + ")


  #Treatment
  treat.form <- paste0(treat_c, " == '", levels[2], "' ~ ", tvars)
  treat.model <- glm(as.formula(treat.form), data=dat,family=binomial(link="logit"))

  weights <- dat %>% mutate(ps = predict(treat.model, newdata=dat, type="response"),
                            w = ifelse(!!sym(treat_c) %in% levels[2], 1/ps, 1/(1-ps))) %>%
    select({{treatment}}, ps, w)

  plot_weights <- summarisR(weights,
                            vars = c(ps, w),
                            group = !!sym(treat_c),
                            headings = list("w" = "Weights",
                                            "ps" = "Propensity Scores"),
                            bins = bins)

  #Censoring
  censor.form <- paste0("Surv(", timevar_c, ", ", event_c, "==0) ~ ", cvars)
  censor.model <- coxph(as.formula(censor.form), data=dat,  x=TRUE)
  censor.model$call$formula <- censor.form

  #Event
  if(surv) {
    event.form <- paste0("Surv(", timevar_c, ", ", event_c, ") ~ ", ovars)
    event.model <- coxph(as.formula(event.form), data=dat, x=TRUE)

    #Diagnostics
    c <- cox.zph(event.model)
    pc <- survminer::ggcoxzph(c)
    diaglist <- list(c,pc)
    names(diaglist)<- c("Cox_tests", "Cox_plots")

  } else {
    event.form <- paste0("Hist(", timevar_c, ", ", event_c, ") ~ ", ovars)
    event.model <- CSC(as.formula(event.form), data=dat)

    #Diagnostics
    c1 <- event.model$models$`Cause 1`
    c2 <- event.model$models$`Cause 2`
    ct1 <- cox.zph(c1)
    ct2 <- cox.zph(c2)
    pct1 <- survminer::ggcoxzph(ct1)
    pct2 <- survminer::ggcoxzph(ct2)
    diaglist <- list(ct1,pct1,ct2,pct2)
    names(diaglist)<- c("Cause1_tests", "Cause1_plots", "Cause2_tests", "Cause2_plots")
  }

  event.model$call$formula <- as.formula(event.form)



  ate.frame <- ate(event = event.model,
                treatment = treat.model,
                censor = censor.model,
                estimator = estimator,
                data = dat,
                times = horizon,
                cause=1)

  plot.frame <- ate(event = event.model,
                    treatment = treat.model,
                    censor = censor.model,
                    estimator = estimator,
                    data = dat,
                    times = unique(round(sort(c(0,dat[dat[, event_c] == 1 & dat[,timevar_c] < horizon, timevar_c],horizon)),event.digits)),
                    cause=1)

  est <-
    as.data.frame(summary(ate.frame, short=T, type = "meanRisk")$meanRisk)

  plot <-
    as.data.frame(summary(plot.frame, short=TRUE, type = "meanRisk")$meanRisk)

  if(surv & survscale == "OS") {
    est <- est %>% mutate(across(c(estimate, lower, upper), ~1 - .)) %>%
      rename(upr = lower,
             lwr = upper) %>%
      rename(!!sym(treat_c) := treatment, est=estimate, upper = upr, lower = lwr)

    plot <- plot %>% mutate(across(c(estimate, lower, upper), ~1 - .))%>%
      rename(upr = lower,
             lwr = upper) %>%
      rename(!!sym(treat_c) := treatment, est=estimate, upper = upr, lower = lwr)

  } else {
    est <- est %>%
      rename(!!sym(treat_c) := treatment, est=estimate)

    plot <- plot %>%
      rename(!!sym(treat_c) := treatment, est=estimate)
  }

  est <- est %>%
    select(time:se, lower, upper) %>%
    mutate(across(c(est:upper), ~round(.,digits)))

  plot <- plot %>%
    select(time:se, lower, upper) %>%
    group_by(!!sym(treat_c)) %>%
    slice(1:n(),n()) %>%
    mutate(time = ifelse(row_number() == n(), time+0.6, time)) %>%
    as.data.frame()

  if(survscale == "OS") {
    plot <- plot %>%
      group_by(!!sym(treat_c)) %>%
      #Fix drops in risk but retain CI width
      mutate(across(c(lower, upper), ~ . + (cummin(est) - est)),
             est = cummin(est),
             across(c(est, lower, upper), ~ cummax(.))) %>%
  ungroup() %>%
  as.data.frame()

  } else {
    plot <- plot %>%
      group_by(!!sym(treat_c)) %>%
      mutate(across(c(lower, upper), ~ . + (cummax(est) - est)),
             est = cummax(est)) %>%
      ungroup() %>%
      as.data.frame()
  }




  rd <-
    as.data.frame(summary(ate.frame, short=T, type = "diffRisk")$diffRisk) %>%
    filter(time == horizon) %>%
    rename(diff = estimate) %>%
    select(time, A, B,diff:p.value) %>%
    mutate(across(c(diff:upper), ~.*100),
           across(c(diff:upper), ~round(.,digits-2)),
           p.exact = p.value,
           p.value = sapply(p.value, pvertR))

  rr <- as.data.frame(summary(ate.frame, short=T, type = "ratioRisk")$ratioRisk) %>%
    filter(time == horizon) %>%
    rename(ratio = estimate) %>%
    select(time,A, B, ratio:p.value)%>%
    mutate(across(c(ratio:upper), ~round(.,digits-2)),
           p.exact = p.value,
           p.value = sapply(p.value, pvertR))

  counts <-
    bind_cols(
      dat %>% group_by(!!sym(treat_c)) %>%
        filter(!!sym(event_c) == 1) %>%
        summarise(n.events = n()),
      dat %>% group_by(!!sym(treat_c)) %>%
        summarise(total = n()) %>%
        select(-!!sym(treat_c)))


  list <- list("table" = tab,
               "plot_data" = plot,
               "models" =  lst("treatment" = summary(treat.model),
                               "event" = event.model,
                               "censoring" = censor.model,
                               "formulas" = data.frame(model = c("treatment", "event", "censoring"),
                                                       formula = c(treat.form, event.form, censor.form))),
               "weights" = list(weights, plot_weights),
               "risks" = est,
               "difference" = rd,
               "ratio" = rr,
               "counts" = counts,
               "diag" = diaglist,
               "info" = list("timevar" = timevar_c,
                             "event" = event_c,
                             "group" = treat_c,
                             "group_levels" = levels,
                             "surv" = surv,
                             "survscale" = survscale,
                             "time" = horizon,
                             "breaks" = breaks,
                             "event.digits" = event.digits))

  if(survtime) {
    list <- append(list, list("time_to_event" = msurv))
  }

  cat(paste0("\nTotal runtime: \n"))
  cat(tockR("diff"))

  class(list)<- "inferencR"
  return(list)


}


