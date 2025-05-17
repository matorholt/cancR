#' clusterci
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

# library(cancR)
#
#
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

clusterci <- function(data,
                      timevar,
                      event,
                      treatment,
                      vars,
                      cluster,
                      time,
                      breaks,
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

  vars_c <-
    data %>% select({{vars}}) %>% names()

  cluster <-
    substitute(cluster)

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
      mutate(treatment = levels(data %>% pull({{treatment}})),
             time = x) %>%
      select(time, treatment, est, lower, upper)
    })) %>% arrange(treatment, time) %>% as.data.frame()

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
               p.value = pfun(p.value),
               time=time) %>%
        select(time, diff, lower, upper, p.value)

  plot <-
    rbindlist(parLapply(cl, sort(unique(data %>% filter({{event}} == 1) %>% pull({{timevar}}))), function(x){
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
             time = x,
             treatment = levels(data %>% pull({{treatment}}))) %>%
      select(time, treatment, est, lower, upper)
  })) %>% group_by(treatment) %>% mutate(across(c(est, lower, upper), ~ cummax(.))) %>%
    slice(1,1:n()) %>%
    mutate(across(c(time, est, lower, upper), ~ ifelse(row_number() == 1, 0, .)))

  lhs <- paste(c("Hist(", paste(substitute(timevar)), ",", paste(substitute(event)), ") ~"), collapse = "")
  rhs_g <- paste(substitute(treatment))
  form_g <- as.formula(paste(c(lhs, rhs_g), collapse = ""))
  pobj <- prodlim(form_g, data=data)

  tab <- summary(pobj, interval=T, times = seq(0,time, breaks), cause = 1) %>%
    group_by({{treatment}}) %>%
    mutate(cumsum = cumsum(n.event)) %>%
    ungroup() %>%
    rename(time = time1) %>%
    select({{treatment}}, time, cumsum, n.risk)


  stopCluster(cl)
  l <- list(Life_table = tab, Risks = risk, Risk_differences = dif, Plot_data = plot, Surv = surv)
  class(l) <- "CLUSTERCI"

  return(l)

}
