#' estimatR
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
#' @param survtime Whether median time to event should be calculated (default = F)
#' @param proportions Whether risk of event in different windows should be estimated (default = F)
#' @param conditional Whether conditional risk at the time horizon should be calculated (default = F)
#' @param cores Number of cores for parallel processing
#' @param pl Whether product.limit in ATE shoulde be T or F (default = T)
#'
#' @return
#' time_to_event: Median survival time \cr
#' table: Event table \cr
#' plot_data: Data for plotting CIF curves \cr
#' hr: Hazard ratio between the groups \cr
#' models: Model objects (cause1 and cause2) \cr
#' diag: Diagnostics for assessing proportionality \cr
#' risks: Absolute risk estimates at the specified time points \cr
#' differences: Absolute risk difference at the specified time horizons \cr
#' ratios: Absolute risk ratios at the the specified time horizon \cr
#' counts: Event and group counts in the contrasted groups \cr
#' proportions: \cr
#' "Before" estimates the risk of event within a certain timepoint (e.g. x% of the events occurred within). \cr
#' "After" estimates the risk of residual events between timepoint x and ten years. \cr
#' "Window" estimates the percentage of events within six month windows (e.g. x% of the events occurred between t1 and t2) \cr
#' conditional: Conditional risk estimates \cr
#' info: information on arguments for extraction
#'
#'
#' @export
#'
#'
# n <- 1000
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
# t2 <- estimatR(df2, ttt, event, X3, type = "select", vars = c(X6,X7))
#
# extractR(t2)
# extractR(t3)

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
                     survtime = F,
                     proportions = F,
                     conditional = F,
                     cores = pmin(detectCores(), 4),
                     pl = T){

  cat("\nestimatR initialized: ", tickR(), "\n")

  suppressWarnings(timevar_c <- data %>% select({{timevar}}) %>% names())
  suppressWarnings( event_c <- data %>% select({{event}}) %>% names())
  suppressWarnings(group_c <- data %>% select({{group}}) %>% names())
  horizon <- time

  type <- match.arg(type, c("uni", "age-sex", "select", "custom"))

  cl <- parallel::makeCluster(cores)
  parallel::clusterEvalQ(cl, {
    library(tidyverse)
    library(riskRegression)
    library(prodlim)
    library(data.table)
  })

  dat <-
    data %>% drop_na(!!sym(group_c))%>%
    mutate(!!sym(group_c) := as.factor(!!sym(group_c))) %>%
    as.data.frame()

  #Break ties
  dat[, timevar_c] <- pmax(0, (dat[, timevar_c] + rnorm(nrow(dat), 0, 0.00000001)))

  group_levels <- levels(dat[,group_c])

  if(length(levels(dat[,event_c])) == 2) {
    dat[,event_c] <- as.numeric(dat[,event_c]) - 1
  }

  if(length(unique(dat[, event_c])) == 2) {
    surv = TRUE
  } else {
    surv = FALSE
  }

  if(length(group_levels > 2)) {
    multi = TRUE
  } else {
    multi = FALSE
  }

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
  tab <- summary(prod, times = seq(0,horizon,1), intervals = TRUE, cause=1) %>%
    group_by(!!sym(group_c)) %>%
    mutate(cumsum = cumsum(n.event)) %>%
    ungroup() %>%
    rename(time = time1) %>%
    select(-time0, -n.lost, -contains("se.")) %>%
    as.data.frame()

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
    c <- cox.zph(hr)
    pc <- survminer::ggcoxzph(c)
    diaglist <- list(c,pc)
    names(diaglist)<- c("Cox_tests", "Cox_plots")

  } else {

    hr <- CSC(form_hhr, data = dat)
    cr <- CSC(as.formula(form_h), data = dat)
    cr$call$formula <- form_h

    hres <- data.frame(level = rownames(summary(hr$models$`Cause 1`)$coefficients),
                       hr =   summary(hr$models$`Cause 1`)$coefficients[,2],
                       lower = summary(hr$models$`Cause 1`)$conf.int[,3],
                       upper = summary(hr$models$`Cause 1`)$conf.int[,4],
                       pval_ex =   summary(hr$models$`Cause 1`)$coefficients[,5])
    c1 <- hr$models$`Cause 1`
    c2 <- hr$models$`Cause 2`
    ct1 <- cox.zph(c1)
    ct2 <- cox.zph(c2)
    pct1 <- survminer::ggcoxzph(ct1)
    pct2 <- survminer::ggcoxzph(ct2)
    diaglist <- list(ct1,pct1,ct2,pct2)
    names(diaglist)<- c("Cause1_tests", "Cause1_plots", "Cause2_tests", "Cause")

  }

  CRest <-
    invisible(ate(cr, treatment = group_c, data=dat, times = seq(0,horizon,breaks), cl=cl, product.limit = pl))

  CRplot <-
    invisible(ate(cr, treatment = group_c, data=dat, times = sort(unique(c(0,dat[dat[, event_c] == 1, timevar_c],horizon))), cl=cl, product.limit = pl))

  est <-
    as.data.frame(summary(CRest, short=T, type = "meanRisk")$meanRisk)

  plot <-
    as.data.frame(summary(CRplot, short=TRUE, type = "meanRisk")$meanRisk)


  if(surv & survscale == "OS") {
    est <- est %>% mutate(across(c(estimate, lower, upper), ~1 - .)) %>%
      rename(upr = lower,
             lwr = upper) %>%
      rename(!!sym(group_c) := treatment, est=estimate, upper = upr, lower = lwr)

    plot <- plot %>% mutate(across(c(estimate, lower, upper), ~1 - .))%>%
      rename(upr = lower,
             lwr = upper) %>%
      rename(!!sym(group_c) := treatment, est=estimate, upper = upr, lower = lwr)

  } else {
    est <- est %>%
      rename(!!sym(group_c) := treatment, est=estimate)

    plot <- plot %>%
      rename(!!sym(group_c) := treatment, est=estimate)
  }
  est <- est %>%
    select(time:se, lower, upper) %>%
    mutate(across(c(est:upper), ~round(.,3)))

  plot <- plot %>%
    select(time:se, lower, upper)


  if(survtime) {
    qs <-
      quantile(prod, tab[tab[, "time"] == horizon, colnames(tab)[which(colnames(tab) %in% c("surv", "cuminc"))]]/2)
    msurv <- tibble(!!sym(group_c) := unlist(qs[,1]),
                    q = qs$q,
                    time = qs$quantile,
                    lwr = qs$lower,
                    upr = qs$upper) %>%
      slice(1,3,5,8)
  }

  hres <- hres %>% filter(str_detect(level, group_c)) %>% mutate(level = str_remove_all(level, group_c),
                                                                 p.value = pvertR(pval_ex),
                                                                 across(c(hr:upper), ~ as.numeric(format(round(.,2), nsmall=1)))) %>%
    rename(!!sym(group_c) := level) %>%
    tibble::remove_rownames()


  rd <-
    as.data.frame(summary(CRest, short=T, type = "diffRisk")$diffRisk) %>%
    filter(time == horizon) %>%
    rename(diff = estimate) %>%
    select(time, A, B,diff:p.value) %>%
    mutate(across(c(diff:upper), ~.*100),
           across(c(diff:upper), ~round(.,1)),
           p.exact = p.value,
           p.value = sapply(p.value, pvertR))

  rr <- as.data.frame(summary(CRest, short=T, type = "ratioRisk")$ratioRisk) %>%
    filter(time == horizon) %>%
    rename(ratio = estimate) %>%
    select(time,A, B, ratio:p.value)%>%
    mutate(across(c(ratio:upper), ~round(.,2)),
           p.exact = p.value,
           p.value = sapply(p.value, pvertR))

  counts <-
    bind_cols(
      dat %>% group_by(!!sym(group_c)) %>%
        filter(!!sym(event_c) == 1) %>%
        summarise(n.events = n()),
      dat %>% group_by(!!sym(group_c)) %>%
        summarise(total = n()) %>%
        select(-!!sym(group_c)))

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
  }

  list <- list("table" = tab,
               "plot_data" = plot,
               "hr" =  hres,
               "models" =  cr,
               "diag" = diaglist,
               "risks" = est,
               "difference" = rd,
               "ratio" = rr,
               "counts" = counts,
               "info" = list("timevar" = timevar_c,
                             "event" = event_c,
                             "group" = group_c,
                             "group_levels" = group_levels,
                             "surv" = surv,
                             "survscale" = survscale,
                             "time" = horizon,
                             "breaks" = breaks,
                             "multi" = multi))

  if(proportions) {
    list <- append(list, list("event_proportions" = prop))
  }
  if(conditional) {
    list <- append(list, list("conditional" = conditional))
  }
  if(survtime) {
    list <- append(list, list("time_to_event" = msurv))
  }

  cat(paste0("\nTotal runtime: \n"))
  cat(tockR("diff"))

  stopCluster(cl)
  class(list)<- "estimatR"
  return(list)

}
