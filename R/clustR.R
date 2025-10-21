#' Causal inference on time-to-event data with clustering and competing risks
#'
#' @description
#' Wrapper for the binregATE in the mets package for performing causal inference on time-to-event data in a competing risk setting with clustering. All covariates needs to be factorized.
#'
#'
#' @param data dataset
#' @param timevar time-to-event variable
#' @param event status indicator as either 0/1 or 0/1/2
#' @param treatment treatment variable, must be coded as factor
#' @param vars vector of covariates. All variables must be coded as factors and continouous variables must be binned
#' @param outcome.vars variables that should only be included in the outcome models
#' @param censoring.vars variables that should only be included in the censoring models
#' @param cluster column indicating cluster variable (e.g. ID)
#' @param time time horizon
#' @param breaks subdivisions between time = 0 and fu
#' @param digits for rounding of eventtimes
#' @param event.digits whether eventtimes should be rounded. Default is 2 to preserve exact times
#' @param cores number of cores for parallel processing. Defaults to half of the available cores
#'
#' @return
#' table: Event table \cr
#' plot_data: Data for plotting CIF curves \cr
#' risks: Absolute risk estimates at the specified time points \cr
#' differences: Absolute risk difference at the specified time point \cr
#' info: information on arguments for extraction
#' @export
#'
#'

# t <- clustR(analysis_df, ttt, event2, X1, vars = c(X3, X6_bin, X7_bin), outcome.vars = X8_bin, censoring.vars = X8_bin, cluster = id, time = 60, breaks = 12)
# plotR(t)

clustR <- function(data,
                   timevar,
                   event,
                   treatment,
                   vars,
                   outcome.vars,
                   censoring.vars,
                   cluster,
                   time=60,
                   breaks = 12,
                   digits = 4,
                   event.digits = 2,
                   cores = detectCores()/2) {

  cat("\nclustR initialized: ", tickR(), "\n")



  suppressWarnings(timevar_c <- data %>% select({{timevar}}) %>% names())
  suppressWarnings(event_c <- data %>% select({{event}}) %>% names())
  suppressWarnings(treat_c <- data %>% select({{treatment}}) %>% names())
  vars_c <- data %>% select({{vars}}) %>% names()
  cluster <- data %>% select({{cluster}}) %>% names()
  ovars_c <- data %>% select({{outcome.vars}}) %>% names()
  cvars_c <- data %>% select({{censoring.vars}}) %>% names()

  group_levels <- levels(data[,treat_c])

  horizon <- time

  classes <- c()

  for(v in c(vars_c, ovars_c, cvars_c, treat_c)) {

    if(any(class(data[[v]]) %nin% c("factor"))) {
      classes <- c(classes, v)
    }


  }

  if(length(classes) > 0) {
    return(cat(paste0("Error: The following variables are not factors: ", paste0(classes, collapse = ", "))))
  }

  if(length(levels(data[,event_c])) == 2) {
    data[,event_c] <- as.numeric(data[,event_c]) - 1
  }

  data <- data %>% drop_na(!!sym(treat_c)) %>%
    mutate(!!sym(treat_c) := as.factor(!!sym(treat_c))) %>%
    as.data.frame()

  if(length(unique(data %>% pull({{event}}) == 2))) {
    surv <- TRUE
  } else {surv <- FALSE}

  out.mod <-
    paste0("Event(", timevar_c, ",", event_c, ")~", treat_c, "+",
           paste0(c(vars_c, ovars_c) , collapse="+"), "+", paste0("cluster(", cluster, ")", collapse = ""))


 treat.mod <-
    paste0(treat_c, "~", paste0(vars_c, collapse="+"), collapse = "")


  c.mod <-
    paste0("~strata(", treat_c, ",", paste0(c(vars_c, cvars_c), collapse=","), ")", collapse = "")

  #Prepare parallelization
  cl <- makeCluster(cores)
  clusterEvalQ(cl, {
    library(tidyverse)
    library(mets)
    library(cancR)
  })

  risk <-
    rbindlist(parLapply(cl, seq(0,horizon,breaks), function(x){
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
    mutate(across(c(est:upper), ~round(.,digits))) %>%
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
        mutate(across(c(diff:upper), ~.*100),
               across(c(diff:upper), ~round(.,digits-2)),
               p.exact = p.value,
               p.value = pvertR(p.value),
               time=horizon) %>%
        select(time, diff, lower, upper, p.value, p.exact) %>%
        as.data.frame()

  plot <-
    rbindlist(parLapply(cl, unique(round(sort(c(0,data[data[, event_c] == 1 & data[,timevar_c] < horizon, timevar_c],horizon)),event.digits)), function(x){
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
         across(c(est, lower, upper), ~ cummax(.))) %>%
   ungroup() %>%
   as.data.frame()

  # lhs <- paste(c("Hist(", timevar_c), ",", event_c), ") ~"), collapse = "")
  # rhs_g <- treat_c
  # form_g <- as.formula(paste(c(lhs, rhs_g), collapse = ""))
  # pobj <- prodlim(form_g, data=data)
  prod <- prodlim(as.formula(paste0("Hist(", timevar_c, ", ", event_c, ") ~", treat_c, collapse = "")), data=data)

  tab <- summary(prod, interval=T, times = seq(0,horizon, breaks), cause = 1) %>%
    group_by(!!sym(treat_c)) %>%
    mutate(cumsum = cumsum(n.event)) %>%
    ungroup() %>%
    rename(time = time1) %>%
    select(!!sym(treat_c), time, cumsum, n.risk) %>%
    as.data.frame()

  counts <-
    bind_cols(
      data %>% group_by(!!sym(treat_c)) %>%
        filter(!!sym(event_c) == 1) %>%
        summarise(n.events = n()),
      data %>% group_by(!!sym(treat_c)) %>%
        summarise(total = n()) %>%
        select(-!!sym(treat_c)))


  stopCluster(cl)
  l <- list("table" = tab,
            "risks" = risk,
            "difference" = dif,
            "counts" = counts,
            "plot_data" = plot,
            "models" =  data.frame(model = c("treatment", "event", "censoring"),
                                   formula = c(treat.mod, out.mod, c.mod)),
            "info" = list("timevar" = timevar_c,
                          "event" = event_c,
                          "group" = treat_c,
                          "group_levels" = group_levels,
                          "surv" = surv,
                          "survscale" = "AM",
                          "time" = horizon,
                          "breaks" = breaks,
                          "event.digits" = event.digits))
  class(l) <- "clustR"

  cat(paste0("\nTotal runtime: \n"))
  cat(tockR("diff"))

  return(l)

}

