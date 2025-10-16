#' Causal inference on time-to-event data with clustering and competing risks
#'
#' @description
#' Wrapper for the binregATE in the mets package for performing causal inference on time-to-event data in a competing risk setting with clustering
#'
#'
#' @param data dataset
#' @param timevar time-to-event variable
#' @param event status indicator as either 0/1 or 0/1/2
#' @param treatment treatment variable, must be coded as factor
#' @param vars vector of covariates. All variables must be coded as factors and continouous variables must be binned
#' @param cluster column indicating cluster variable (e.g. ID)
#' @param time time horizon
#' @param breaks subdivisions between time = 0 and fu
#' @param event.digits whether eventtimes should be rounded. Default is 2 to preserve exact times
#' @param cores number of cores for parallel processing. Defaults to half of the available cores
#'
#' @return
#' Life_table: Event table \cr
#' Plot_data: Data for plotting CIF curves \cr
#' Risks: Absolute risk estimates at the specified time points \cr
#' Risk_differences: Absolute risk difference at the specified time point \cr
#' Surv: Indicator of the surv-argument
#' @export
#'
#'

# set.seed(1)
# n=1000
# df <- riskRegression::sampleData(n) %>%
#   mutate(id = sample(seq(1,700), size = n, replace=TRUE),
#          time = time*30,
#          across(c(X6:X10), ~ cut(., breaks = unique(quantile(., c(0, 0.25,0.5,0.75, 1))))),
#          across(c(X6:X10), ~ as.factor(.))) %>%
#   drop_na(time, event, X1, X2, X3, X6, X7) %>%
#   as.data.frame()
# (t <- clustR(df, time, event, X1, vars = c(X2, X3, X6), cluster = id, time = 90, breaks = 10))

clustR <- function(data,
                      timevar,
                      event,
                      treatment,
                      vars,
                      cluster,
                      time,
                      breaks,
                   event.digits = 2,
                      cores = detectCores()/2) {

  cl <- makeCluster(cores)
  clusterEvalQ(cl, {
    library(tidyverse)
    library(mets)
    library(cancR)
  })

  if(length(unique(data %>% pull({{event}}) == 2))) {
    surv <- TRUE
  } else {surv <- FALSE}

  suppressWarnings(timevar_c <- data %>% select({{timevar}}) %>% names())
  suppressWarnings(event_c <- data %>% select({{event}}) %>% names())
  suppressWarnings(treat_c <- data %>% select({{treatment}}) %>% names())
  vars_c <-
    data %>% select({{vars}}) %>% names()
  cluster <- data %>% select({{cluster}}) %>% names()

  group_levels <- levels(data[,treat_c])

  data <- data %>% drop_na(!!sym(treat_c))%>%
    mutate(!!sym(treat_c) := as.factor(!!sym(treat_c))) %>%
    as.data.frame()

  out.mod <-
    paste0("Event(", substitute(timevar), ",", substitute(event), ")~", substitute(treatment), "+",
           paste0(vars_c, collapse="+"), "+", paste0("cluster(", cluster, ")", collapse = ""))


 treat.mod <-
    paste0(substitute(treatment), "~", paste0(vars_c, collapse="+"), collapse = "")


  c.mod <-
    paste0("~strata(", substitute(treatment), ",", paste0(vars_c, collapse=","), ")", collapse = "")

  risk <-
    rbindlist(parLapply(cl, seq(0,time,breaks), function(x){
    bin_obj <- binregATE(formula = as.formula(out.mod),
                        data=data,cause=1,
                        treat.model = as.formula(treat.mod),
                        time=x,cens.model= as.formula(c.mod))

    as.data.frame(estimate(coef=bin_obj$riskDR,vcov=bin_obj$var.riskDR)$coefmat) %>%
      rename(est = "Estimate",
             lower = "2.5%",
             upper = "97.5%") %>%
      mutate(lower = pmax(0,lower),
             upper = pmin(1, upper),
             est = pmax(0, est),
             !!sym(treat_c) := as.factor(group_levels),
             time = x) %>%
       select(time, !!sym(treat_c), est, lower, upper)
    })) %>%
    arrange(!!sym(treat_c), time) %>%
    as.data.frame()

      bin_obj <- binregATE(data=data,
                           formula = as.formula(out.mod),
                           treat.model = as.formula(treat.mod),
                           cens.model= as.formula(c.mod),
                           cause=1,
                           time=time)

      dif <-
      as.data.frame(estimate(coef=bin_obj$difriskDR,vcov=as.matrix(bin_obj$var.difriskDR))$coefmat) %>%
        rename(diff = "Estimate",
               lower = "2.5%",
               upper = "97.5%",
               p.value = "P-value") %>%
        mutate(across(c(diff, lower, upper), ~ round(.*100,2)),
               p.exact = p.value,
               p.value = pvertR(p.value),
               time=time) %>%
        select(time, diff, lower, upper, p.value, p.exact) %>%
        as.data.frame()

  plot <-
    rbindlist(parLapply(cl, unique(round(sort(c(0,data[data[, event_c] == 1 & data[,timevar_c] < time, timevar_c],time)),event.digits)), function(x){
    object <- binregATE(formula = as.formula(out.mod),
                        data=data,cause=1,
                        treat.model = as.formula(treat.mod),
                        time=x,cens.model= as.formula(c.mod))

    as.data.frame(estimate(coef=object$riskDR,vcov=object$var.riskDR)$coefmat) %>%
      rename(est = "Estimate",
             lower = "2.5%",
             upper = "97.5%") %>%
      mutate(lower = pmax(0,lower),
             upper = pmin(1, upper),
             est = pmax(0, est),
             !!sym(treat_c) := as.factor(as.factor(group_levels)),
             time = x) %>%
      select(time, !!sym(treat_c), est, lower, upper)
  })) %>% group_by(!!sym(treat_c)) %>%
    slice(1:n(), n()) %>%
    mutate(across(c(time, est, lower, upper), ~ ifelse(row_number() == 1, 0, .)),
         time = ifelse(row_number() == n(), time+0.6, time),
         across(c(lower, upper), ~ . + (cummax(est) - est)),
         est = cummax(est)) %>%
   ungroup() %>%
   as.data.frame()

  lhs <- paste(c("Hist(", paste(substitute(timevar)), ",", paste(substitute(event)), ") ~"), collapse = "")
  rhs_g <- paste(substitute(treatment))
  form_g <- as.formula(paste(c(lhs, rhs_g), collapse = ""))
  pobj <- prodlim(form_g, data=data)

  tab <- summary(pobj, interval=T, times = seq(0,time, breaks), cause = 1) %>%
    group_by({{treatment}}) %>%
    mutate(cumsum = cumsum(n.event)) %>%
    ungroup() %>%
    rename(time = time1) %>%
    select({{treatment}}, time, cumsum, n.risk) %>%
    as.data.frame()


  stopCluster(cl)
  l <- list("table" = tab,
            "risks" = risk,
            "difference" = dif,
            "plot_data" = plot,
            "info" = list("timevar" = timevar_c,
                          "event" = event_c,
                          "group" = treat_c,
                          "group_levels" = group_levels,
                          "surv" = surv,
                          "survscale" = "AM",
                          "time" = time,
                          "breaks" = breaks))
  class(l) <- "clustR"

  return(l)

}

