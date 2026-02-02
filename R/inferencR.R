#' Causal inference of time-to-event data
#'
#' @description
#' Average treatment effect of point intervention with either IPTW, GFORMULA or AIPTW. Wrapper for the ATE function in riskRegression.
#'
#'
#' @param data dataframe
#' @param treatment column indicating treatment variable. Currently only binary treatments are supported.
#' @param timevar Time variable
#' @param event Status indicator
#' @param vars vector of variables that should be included in the treatment, censoring and outcome models
#' @param outcome.vars vector of variables that should only be included in the outcome model
#' @param censoring.vars vector of variables that should only be included in the censoring model
#' @param method whether the outcome is "tte" (time-to-event) or "binary" (default = "tte")
#' @param treat.form custom formula for the RHS of the treatment model
#' @param event.form custom formula for the RHS of the outcome model
#' @param cens.form custom formula for the RHS of the censoring model
#' @param time Time horizon of interest. Defaults to 60 (e.g. 5-years)
#' @param breaks Interim time points of interest. Defaults to 12 months (1-year gaps)
#' @param cause cause of interest, default = 1
#' @param estimator whether the estimator should be "IPTW", "GFORMULA" or "AIPTW" (default)
#' @param plot whether plot data should be estimated for time optimization (default = T)
#' @param survtime Whether median time to event should be calculated (default = F)
#' @param survscale Whether overall survial should be estimated as survival or all-cause mortality (1-survival)
#' @param bins bins for the weights plot, default = 0.5
#' @param digits for rounding of eventtimes
#' @param event.digits whether eventtimes should be rounded. Default is 2 to preserve exact times
#' @param alpha alpha level for the estimation of confidence intervals and p-values. Default = 0.05
#' @param weights.digits the number of digits for categorization of weights
#' @param weights.breaks breaks for categorization of weights
#'
#' @return
#' table: Event table \cr
#' time_to_event: Median survival time \cr
#' plot_data: Data for plotting CIF curves \cr
#' risks: Absolute risk estimates at the specified time points \cr
#' differences: Absolute risk difference at the specified time horizons \cr
#' ratios: Absolute risk ratios at the the specified time horizon \cr
#' counts: Event and group counts in the contrasted groups \cr
#' weights: Object containing: \cr
#' data: the raw dataset with propensity scores (ps), weights (w) and categorized weighting groups (wgroup) \cr
#' table_iptw: a table of the crude and iptw adjusted covariates \cr
#' table_strat: a table of the weigthing groups stratified on treatment groups \cr
#' plot: plot of the propensity scores and weights for each group
#' models: Model objects (cause1 and cause2) \cr
#' diagnostics: Diagnostics for assessing proportionality \cr
#' info: information on arguments for extraction
#' @export
#'

# tdf <- analysis_df %>%
#   drop_na(X2) %>%
#   mutate(X2 = fct_drop(X2))
#
# t1 <- inferencR(analysis_df,
#                 treatment = X2,
#                 timevar = ttt,
#                 event = event2,
#                 vars = c(X1, X3, X6, X7),
#                 outcome.vars = X10,
#                 estimator = "GFORMULA",
#                 plot=F)

#plotR(t1)
#
# # Binary outcome
# t2 <- inferencR(analysis_df,
#                 treatment = X2,
#                 event = event2,
#                 vars = c(X1, X3, X6, X7),
#                 estimator = "AIPTW",
#                 method = "binary",
#                 treat.form = "X1 * X3 + X4",
#                 cens.form = "X2 + X1 * X3",
#                 event.form = "X2 * X3")
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
                      method = "tte",
                      treat.form = NULL,
                      event.form = NULL,
                      cens.form = NULL,
                      time = 120,
                      breaks = 12,
                      cause = 1,
                      estimator = "AIPTW",
                      plot = T,
                      survtime = T,
                      survscale = "AM",
                      bins = 0.5,
                      digits = 4,
                      event.digits = 2,
                      alpha = 0.05,
                      weights.digits = 1,
                      weights.breaks = 1) {


  cat("\ninferencR initialized: ", tickR(), "\n")

  dat <- data %>%
    drop_na({{treatment}})

  for(i in c("data",
             "treatment",
             "timevar",
             "event")) {

    if(i %nin% names(match.call())) {

      if(i == "timevar" & method != "tte") next

      return(cat(paste0("Error: Argument ", i, " is not specified")))
    }

  }

  treat_c <- data %>% select({{treatment}}) %>% names()
  event_c <- data %>% select({{event}}) %>% names()



  #Vars extracted from custom forms if specified
  if(missing(vars)) {
    vars_c <- unique(c(unlist(str_extract_all(str_remove_all(c(treat.form, cens.form, event.form), "\\+|\\*|\\:|\\(|\\)"), "\\w+"))))
  } else {
    vars_c <- data %>% select({{vars}}) %>% names()
  }

  #Outcome vars extracted from custom form if specified
  if(missing(outcome.vars)) {
    ovars_c <- unique(c(unlist(str_extract_all(str_remove_all(c(event.form), "\\+|\\*|\\:|\\(|\\)"), "\\w+"))))
  } else {
    ovars_c <- data %>% select({{outcome.vars}}) %>% names()
  }

  levels <- levels(data[[treat_c]])

  #Outputs
  out.list <- list()

  #Treatment model
  if(is.null(treat.form)) {
    treat.form <- paste0(treat_c, " == '", levels[2], "' ~ ", paste0(vars_c, collapse = " + "))
  } else {
    treat.form <- paste0(treat_c, " == '", levels[2], "' ~ ", treat.form)
  }

  treat.model <- glm(as.formula(treat.form), data=dat,family=binomial(link="logit"))

  #Weighting diagnostics
  w_object <-  WeightIt::weightit(as.formula(treat.form),
                                                    data = dat,
                                                    estimand = "ATE",
                                                    method = "glm")

  dat_w <- dat %>% mutate(ps = w_object$ps,
                          w = w_object$weights) %>%
        cutR(w, seq(0,100,weights.breaks), digits = weights.digits,
              name.list = "wgroup")



  plot_weights <- summarisR(dat_w,
                            vars = c(ps, w),
                            group = !!sym(treat_c),
                            headings = list("w" = "Weights",
                                            "ps" = "Propensity Scores"),
                            bins = bins)

  balance_plot <- cobalt::love.plot(w_object)

  bal_tab <-  cobalt::bal.tab(w_object,
                              disp = "means",
                              un=T)


  tab_w <- cbind(tablR(dat_w,
                           group = treat_c,
                           vars = c(vars_c, ovars_c),
                           ),
                     tablR(dat_w,
                 group = treat_c,
                 vars = c(vars_c, ovars_c),
                 weights = w)[,-1])



  dat_w <- dat_w %>%
    factR(num.vars = "wgroup")

  strat_w <-
    iteratR(eval(parse(text = paste0("split(dat_w, ~", treat_c, ")"))),
            group = "wgroup",
            vars = c(vars_c, ovars_c),
            method = "tablR",
            total=T,
            test=F,
            labels = levels)

  out.list[["weights"]] <- list(data = dat_w,
                                table_iptw = tab_w,
                                table_strat = strat_w,
                                plot = plot_weights,
                                bal_plot = balance_plot,
                                bal_tab = bal_tab)


  if(method == "tte") {

    horizon <- time
    timevar_c <- data %>% select({{timevar}}) %>% names()
    cvars_c <- data %>% select({{censoring.vars}}) %>% names()

    #Detect survival outcome
    if(length(levels(dat[,event_c])) == 2) {
      dat[,event_c] <- as.numeric(dat[,event_c]) - 1
    }

    #Survival outcome
    surv <- length(unique(dat[, event_c])) == 2

    #Rounding of event times
    dat <- dat %>%
      mutate(!!sym(timevar_c) := round(!!sym(timevar_c), event.digits))

    #RISK TABLE
    prod <- prodlim(as.formula(paste0("Hist(", timevar_c, ", ", event_c, ") ~", treat_c, collapse = "")), data=dat)
    tab <- summary(prod, times = seq(0,horizon,1), intervals = TRUE, cause=cause) %>%
      group_by(!!sym(treat_c)) %>%
      mutate(cumsum = cumsum(n.event)) %>%
      ungroup() %>%
      rename(time = time1) %>%
      select(-time0, -n.lost, -contains("se.")) %>%
      as.data.frame()

    out.list[["table"]] <- tab

    if(survtime) {

      msurv <- list2DF(quantile(prodlim(as.formula(paste0("Hist(", timevar_c, ", ", event_c, ") ~", treat_c, collapse = "")), data = dat[dat[[event_c]] == 1,]), 0.5)) %>% select(-q)

      out.list[["time_to_event"]] <- msurv

    }

  #Censoring
  cvars <- paste0(c(treat_c, vars_c, cvars_c), collapse = " + ")

  if(is.null(cens.form)) {
    censor.form <- paste0("Surv(", timevar_c, ", ", event_c, "==0) ~ ", cvars)
  } else {
    censor.form <- paste0("Surv(", timevar_c, ", ", event_c, "==0) ~ ", cens.form)
  }

  censor.model <- coxph(as.formula(censor.form), data=dat,  x=TRUE)
  censor.model$call$formula <- censor.form

  #Event
  if(surv) {

    if(is.null(event.form)) {
      event.form <- paste0("Surv(", timevar_c, ", ", event_c, ") ~ ", paste0(c(treat_c, vars_c, ovars_c), collapse = " + "))
    } else {
      event.form <- paste0("Surv(", timevar_c, ", ", event_c, "==0) ~ ", event.form)
    }

    event.model <- coxph(as.formula(event.form), data=dat, x=TRUE)

    #Diagnostics
    c <- cox.zph(event.model)
    pc <- survminer::ggcoxzph(c)
    diaglist <- list(c,pc)
    names(diaglist)<- c("Cox_tests", "Cox_plots")

  } else {

    if(is.null(event.form)) {
      event.form <- paste0("Hist(", timevar_c, ", ", event_c, ") ~ ", paste0(c(treat_c, vars_c, ovars_c), collapse = " + "))
    } else {
      event.form <- paste0("Hist(", timevar_c, ", ", event_c, ") ~ ", event.form)
    }

    event.model <- CSC(as.formula(event.form), data=dat)

    #Diagnostics
    out.list[["diagnostics"]] <- list(cox.zph(event.model$models$`Cause 1`),
                                      survminer::ggcoxzph(cox.zph(event.model$models$`Cause 1`)),
                                      cox.zph(event.model$models$`Cause 2`),
                                      survminer::ggcoxzph(cox.zph(event.model$models$`Cause 2`))) %>%
      set_names(c("Cause1_tests", "Cause1_plots", "Cause2_tests", "Cause2_plots"))

  }

  event.model$call$formula <- as.formula(event.form)

  } else if(method == "binary") {

    #GLM
    if(is.null(event.form)) {
      event.form <- paste0(event_c, " == '", 1, "' ~ ", paste0(c(treat_c, vars_c, ovars_c), collapse = " + "))
    } else {
      event.form <- paste0(event_c, " == '", 1, "' ~ ", event.form)
    }

    event.model <- glm(as.formula(event.form), data=dat,family=binomial(link="logit"))

    censor.form <- "-"
    censor.model <- NULL
    horizon <- NA
    timevar_c <- "-"
    surv <- F
    survscale <- "-"
    breaks <- "-"
    plot <- F

  }

  out.list[["models"]] <- list("treatment" = summary(treat.model),
                               "event" = event.model,
                               "censoring" = censor.model,
                               "formulas" = data.frame(model = c("treatment", "event", "censoring"),
                                                       formula = c(treat.form, event.form, censor.form)))


  ate.frame <- ate(event = event.model,
                treatment = treat.model,
                censor = censor.model,
                estimator = estimator,
                data = dat,
                times = horizon,
                cause=cause)

  est <-
    as.data.frame(confint(ate.frame, level = 1-alpha)$meanRisk)


  if(plot) {

    plot.frame <- ate(event = event.model,
                      treatment = treat.model,
                      censor = censor.model,
                      estimator = estimator,
                      data = dat,
                      times = unique(sort(c(0,dat[dat[, event_c] == 1 & dat[,timevar_c] < horizon, timevar_c],horizon))),
                      cause=cause)

    plot_data <-
      as.data.frame(confint(plot.frame, level = 1-alpha)$meanRisk)

    if(surv & survscale == "OS") {
      plot <- plot %>% mutate(across(c(estimate, lower, upper), ~1 - .))%>%
        rename(upr = lower,
               lwr = upper) %>%
        rename(!!sym(treat_c) := treatment, est=estimate, upper = upr, lower = lwr)

    } else {

      plot <- plot %>%
        rename(!!sym(treat_c) := treatment, est=estimate)
    }

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

    out.list[["plot_data"]] <- plot

  }

  if(surv & survscale == "OS") {


    est <- est %>% mutate(across(c(estimate, lower, upper), ~1 - .)) %>%
      rename(upr = lower,
             lwr = upper) %>%
      rename(!!sym(treat_c) := treatment, est=estimate, upper = upr, lower = lwr)

  } else {
    est <- est %>%
      rename(!!sym(treat_c) := treatment, est=estimate)
  }

  est <- est %>%
    select(time:se, lower, upper) %>%
    mutate(across(c(est:upper), ~round(.,digits)))

  out.list[["risks"]] <- est

  rd <-
    as.data.frame(confint(ate.frame, level = 1-alpha)$diffRisk) %>%
      rename(diff = estimate) %>%
      select(time, A, B,diff:p.value) %>%
      mutate(across(c(diff:upper), ~.*100),
             across(c(diff:upper), ~round(.,digits-2)),
             p.exact = pmin(1, p.value * 0.05/alpha),
             p.value = sapply(p.value * 0.05/alpha, pvertR))

  out.list[["difference"]] <- rd

  rr <-
    as.data.frame(confint(ate.frame, level = 1-alpha)$ratioRisk) %>%
    rename(ratio = estimate) %>%
    select(time,A, B, ratio:p.value)%>%
    mutate(across(c(ratio:upper), ~round(.,digits-2)),
           p.exact = pmin(1, p.value * 0.05/alpha),
           p.value = sapply(p.value * 0.05/alpha, pvertR))

  out.list[["ratio"]] <- rr

  counts <-
    bind_cols(
      dat %>% group_by(!!sym(treat_c)) %>%
        filter(!!sym(event_c) == 1) %>%
        summarise(n.events = n()),
      dat %>% group_by(!!sym(treat_c)) %>%
        summarise(total = n()) %>%
        select(-!!sym(treat_c)))

  out.list[["counts"]] <- counts


  out.list[["info"]] <- list(method = method,
                             timevar = timevar_c,
                             event = event_c,
                             group = treat_c,
                             group_levels = levels,
                             surv = surv,
                             survscale = survscale,
                             time = horizon,
                             breaks = breaks,
                             event.digits = event.digits)

  cat(paste0("\nTotal runtime: \n"))
  cat(tockR("diff"))

  class(out.list)<- "inferencR"
  return(out.list)


}

