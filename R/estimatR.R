#' Absolute risk estimation of time-to-event data with competing risks
#'
#' @description
#' Absolute risk estimation and comparison of two or more groups with a time-to-event outcome.
#' Wrapper for the prodlim and ate-functions in riskRegression.
#'
#' @param data dataset
#' @param timevar Time variable
#' @param event Status indicator
#' @param group Grouping variable
#' @param survscale Whether overall survial should be estimated as survival or all-cause mortality (1-survival)
#' @param type Model specification, Can be univariate ("uni"), Age- and sex standardized ("age-sex"), Multivariate with variable selection ("select"). In this case vars should be a vector of the covariates. "custom" allows for free modelling where the form-argument contains the formula.
#' @param vars Only applicable when "select" is chosen as type. The variables are added to the model as an additive model
#' @param form Only applicable when "custom" is chosen as type. Free specification of the model as the right-hand side of the formula.
#' @param time Time horizon of interest. Defaults to 60 (e.g. 5-years)
#' @param breaks Interim time points of interest. Defaults to 12 months (1-year gaps)
#' @param cause cause of interest, default = 1
#' @param pl Whether product.limit in ATE shoulde be T or F (default = T)
#' @param digits number of digits risk estimates in the returned results
#' @param event.digits Rounding of event times. Default is 2 to preserve exact times
#' @param alpha alpha level for the estimation of confidence intervals and p-values. Default = 0.05
#' @param survtime Whether median time to event should be calculated (default = T)
#' @param proportions Whether risk of event in different windows should be estimated (default = F)
#' @param conditional Whether conditional risk at the time horizon should be calculated (default = F)
#' @param diagnostics whether Scaled Schoenfield residuals should be visualized (default = F)
#' @param plot whether estimates for plotR should be performed (default = T)
#'
#' @return
#' List of class "estimatR" containing the following:
#' table: Event table \cr
#' models: Model objects (cause1 and cause2) \cr
#' risks: Absolute risk estimates at the specified time points \cr
#' plot_data: Data for plotting CIF curves \cr
#' time_to_event: Median survival time \cr
#' hr: Hazard ratio between the groups \cr
#' difference: Absolute risk difference at the specified time horizons \cr
#' ratio: Absolute risk ratios at the the specified time horizon \cr
#' counts: Event and group counts in the contrasted groups \cr
#' info: information on arguments for extraction
#' Optional:
#' diag: Diagnostics for assessing proportionality \cr
#' proportions: \cr
#' "Before" estimates the risk of event within a certain timepoint (e.g. x% of the events occurred within). \cr
#' "After" estimates the risk of residual events between timepoint x and ten years. \cr
#' "Window" estimates the percentage of events within six month windows (e.g. x% of the events occurred between t1 and t2) \cr
#' conditional: Conditional risk estimates \cr
#'
#'
#' @export
#'
#' @examples
#' estimatR(analysis_df, ttt, event, X1, type = "select", vars = c(X6,X7))
#'
#'
#t2 <- estimatR(analysis_df, ttt, event, X1, type = "select", vars = c(X6,X7))

#Multiple causes
# t2 <- estimatR(analysis_df,
#                timevar=ttt,
#                event=event3,
#                group=X1,
#                type = "select",
#                vars = c(X6,X7),
#                cause = 3,
#                plot=T)

#extractR(t2)

#plotR(t2)

# extractR(t2)

estimatR <- function(data,
                     timevar,
                     event,
                     group,
                     survscale = "AM",
                     type = "uni",
                     vars,
                     form,
                     time=120,
                     breaks = 12,
                     cause = 1,
                     pl = T,
                     digits = 4,
                     event.digits = 2,
                     alpha = 0.05,
                     survtime = T,
                     proportions = F,
                     conditional = F,
                     diagnostics = F,
                     plot=T) {

  cat("\nestimatR initialized: ", tickR(), "\n")

  for(i in c("data",
             "treatment",
             "timevar",
             "event")) {

    if(i %nin% names(match.call())) {

      return(cat(paste0("Error: Argument ", i, " is not specified")))
    }

  }

  suppressWarnings(timevar_c <- data %>% select({{timevar}}) %>% names())
  suppressWarnings(event_c <- data %>% select({{event}}) %>% names())
  suppressWarnings(group_c <- data %>% select({{group}}) %>% names())
  horizon <- time

  out.list <- list()

  type <- match.arg(type, c("uni", "age-sex", "select", "custom"))

  if(!is.factor(data[[group_c]])) {

    return(cat("Error:", group_c, "is not a factor. Convert using the factR() function"))

  }

  dat <-
    data %>% drop_na(!!sym(group_c))%>%
    as.data.frame()

  group_levels <- levels(dat[,group_c])

  if(group_levels > 10) {
    cat("Error: Number of levels in group exceeding 10, wrong specification of the grouping variable?")
  }

  if(length(levels(dat[,event_c])) == 2) {
    dat[,event_c] <- as.numeric(dat[,event_c]) - 1
  }

  #Survival outcome
  surv <- length(unique(dat[, event_c])) == 2

  #Multiple groups
  multi <- length(group_levels) > 2

  #Rounding of event times
  dat <- dat %>%
    mutate(!!sym(timevar_c) := round(!!sym(timevar_c), event.digits))

  if(type %in%"uni"){
    rhs <- group_c

  }
  if(type %in%"age-sex"){
    rhs <- paste0(group_c, " + age + sex", collapse="")
  }
  if(type %in%"select"){
    suppressWarnings(vars_c <- data %>% select({{vars}}) %>% names())
    rhs <- paste(c(group_c, vars_c), collapse = " + ")
    dat <- dat %>% drop_na(!!!syms(vars_c))
  }
  if(type %in%"custom"){
    rhs <- paste(c(form), collapse = "")
  }

  rhs_strat <- str_replace(rhs, group_c, paste0("strata(", group_c, ")"))


  #Formula generation
  form_h <- as.formula(paste(c(paste0("Hist(", timevar_c, ", ", event_c, ") ~", collapse = ""), rhs_strat), collapse = ""))
  form_hhr <- as.formula(paste(c(paste0("Hist(", timevar_c, ", ", event_c, ") ~", collapse = ""), rhs), collapse = ""))
  form_s <- as.formula(paste(c(paste0("Surv(", timevar_c, ", ", event_c, ") ~", collapse = ""), rhs_strat), collapse = ""))
  form_shr <- as.formula(paste(c(paste0("Surv(", timevar_c, ", ", event_c, ") ~", collapse = ""), rhs), collapse = ""))

  #Life-table
  prod <- prodlim(as.formula(paste0("Hist(", timevar_c, ", ", event_c, ") ~", group_c, collapse = "")), data=dat)
  tab <- summary(prod, times = seq(0,horizon,1), intervals = TRUE, cause=cause) %>%
    group_by(!!sym(group_c)) %>%
    mutate(cumsum = cumsum(n.event)) %>%
    ungroup() %>%
    rename(time = time1) %>%
    select(-time0, -n.lost, -contains("se.")) %>%
    as.data.frame()

  out.list[["table"]] <- tab

  #Model specification and diagnostics
  if(surv){

    hr <-
      coxph(form_shr, data=dat, x=TRUE, y=TRUE)

    cr <-
      coxph(form_s, data=dat, x=TRUE, y=TRUE)
    cr$call$formula <- form_s

    hres <- data.frame(level = rownames(summary(hr)$coefficients),
                       hr =   summary(hr)$coefficients[,2],
                       lower = summary(hr)$conf.int[,3],
                       upper = summary(hr)$conf.int[,4],
                       pval_ex = summary(hr)$coefficients[,5])
    if(diagnostics) {
      out.list[["diagnostics"]] <-
        list(cox.zph(hr),
             survminer::ggcoxzph(cox.zph(hr))) %>% set_names(c("Cox_tests", "Cox_plots"))


    }

  } else {

    hr <- CSC(form_hhr, data = dat)
    cr <- CSC(as.formula(form_h), data = dat)
    cr$call$formula <- form_h

    hres <- data.frame(level = rownames(summary(hr$models$`Cause 1`)$coefficients),
                       hr =   summary(hr$models$`Cause 1`)$coefficients[,2],
                       lower = summary(hr$models$`Cause 1`)$conf.int[,3],
                       upper = summary(hr$models$`Cause 1`)$conf.int[,4],
                       pval_ex =   summary(hr$models$`Cause 1`)$coefficients[,5])

     if(diagnostics) {

    out.list[["diagnostics"]] <-
      list(cox.zph(hr$models$`Cause 1`),
           survminer::ggcoxzph(cox.zph(hr$models$`Cause 1`)),
           cox.zph(hr$models$`Cause 2`),
           survminer::ggcoxzph(hr$models$`Cause 2`)) %>%
      set_names(c("Cause1_tests", "Cause1_plots", "Cause2_tests", "Cause2_plots"))


    }

  }

  out.list[["models"]] <- cr

  CRest <-
    invisible(ate(cr,
                  treatment = group_c,
                  data=dat,
                  times = seq(0,horizon,breaks),
                  product.limit = pl,
                  cause = cause))

  est <-
    as.data.frame(confint(CRest, level = 1-alpha)$meanRisk)

  if(plot) {

    CRplot <-
      invisible(ate(cr,
                    treatment = group_c,
                    data=dat,
                    times = unique(sort(c(0,dat[dat[, event_c] == 1 & dat[,timevar_c] < horizon, timevar_c],horizon))),
                    product.limit = pl,
                    cause = cause))

    plot_data <-
      as.data.frame(confint(CRplot, level = 1-alpha)$meanRisk)

  }


  if(surv & survscale == "OS") {
    est <- est %>% mutate(across(c(estimate, lower, upper), ~1 - .)) %>%
      rename(upr = lower,
             lwr = upper) %>%
      rename(!!sym(group_c) := treatment, est=estimate, upper = upr, lower = lwr)

    if(plot) {
    plot_data <- plot_data %>% mutate(across(c(estimate, lower, upper), ~1 - .))%>%
      rename(upr = lower,
             lwr = upper) %>%
      rename(!!sym(group_c) := treatment, est=estimate, upper = upr, lower = lwr)
    }

  } else {
    est <- est %>%
      rename(!!sym(group_c) := treatment, est=estimate)

    if(plot) {
    plot_data <- plot_data %>%
      rename(!!sym(group_c) := treatment, est=estimate)
    }
  }
  est <- est %>%
    select(time:se, lower, upper) %>%
    mutate(across(c(est:upper), ~round(.,digits)))

  out.list[["risks"]] <- est

  if(plot) {
  plot_data <- plot_data %>%
    select(time:se, lower, upper) %>%
    group_by(!!sym(group_c)) %>%
    slice(1:n(),n()) %>%
    mutate(time = ifelse(row_number() == n(), time+0.6, time)) %>%
    as.data.frame()

  out.list[["plot_data"]] <- plot_data

  }

  if(survtime) {

    msurv <- list2DF(quantile(prodlim(as.formula(paste0("Hist(", timevar_c, ", ", event_c, ") ~", group_c, collapse = "")), data = dat[dat[[event_c]] == 1,]), 0.5)) %>% select(-q)

    out.list[["time_to_event"]] <- msurv


  }

  hres <- hres %>% filter(str_detect(level, group_c)) %>% mutate(level = str_remove_all(level, group_c),
                                                                 p.value = pvertR(pval_ex * 0.05/alpha),
                                                                 across(c(hr:upper), ~ as.numeric(format(round(.,2), nsmall=1)))) %>%
    rename(!!sym(group_c) := level) %>%
    tibble::remove_rownames()

  out.list[["hr"]] <- hres


  rd <-
    as.data.frame(confint(CRest, level = 1-alpha)$diffRisk) %>%
    filter(time == horizon) %>%
    rename(diff = estimate) %>%
    select(time, A, B,diff:p.value) %>%
    mutate(across(c(diff:upper), ~.*100),
           across(c(diff:upper), ~round(.,digits-2)),
           p.exact = pmin(1, p.value * 0.05/alpha),
           p.value = sapply(p.value * 0.05/alpha, pvertR))



  out.list[["difference"]] <- rd

  rr <-
    as.data.frame(confint(CRest, level = 1-alpha)$ratioRisk) %>%
    filter(time == horizon) %>%
    rename(ratio = estimate) %>%
    select(time,A, B, ratio:p.value)%>%
    mutate(across(c(ratio:upper), ~round(.,digits-2)),
           p.exact = pmin(1, p.value * 0.05/alpha),
           p.value = sapply(p.value * 0.05/alpha, pvertR))

  out.list[["ratio"]] <- rr

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

  if(proportions) {
  prop <- est %>%group_by(!!sym(group_c))%>%
    mutate(est = ifelse(rep(survscale == "OS", n()), 1-est, est),
           before = est / last(est)* 100,
           after = 100-before,
           window = before - lag(before),
           window = ifelse(is.na(window), before, window),
           residual = (last(est)- est),
           rse = sqrt(last(se)^2 +se^2),
           rescil = (pmax((residual - (stats::qnorm(1-(alpha/2))*rse)), 0)),
           resciu = (pmin((residual +(stats::qnorm(1-(alpha/2))*rse)),100)),
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
      ci.cs <- round(cs^(exp(c(1, -1)* stats::qnorm(1-(alpha/2))* sqrt(var.cs))),3)
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

  out.list[["info"]] <- list(timevar = timevar_c,
                             event = event_c,
                             group = group_c,
                             group_levels = group_levels,
                             surv = surv,
                             survscale = survscale,
                             time = horizon,
                             breaks = breaks,
                             event.digits = event.digits,
                             alpha = alpha,
                             multi = multi,
                             cause = cause)

  cat(paste0("\nTotal runtime: \n"))
  cat(tockR("diff"))

  class(out.list)<- "estimatR"
  return(out.list)

}
