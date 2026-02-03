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
                     method = "cox",
                     vars,
                     event.form = NULL,
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

  dat <- data


  for(i in c("data",
             "timevar",
             "event")) {

    if(i %nin% names(match.call())) {

      return(cat(paste0("Error: Argument ", i, " is not specified")))
    }

  }

  if(method %nin% c("cox", "aalen")) {

    cat("Error: Invalid choice of method. Choose between cox and aalen")

  }

  if(!missing(vars)) method <- "cox"
  if(missing(group)) method <- "aalen"


  suppressWarnings(timevar_c <- dat %>% select({{timevar}}) %>% names())
  suppressWarnings(event_c <- dat %>% select({{event}}) %>% names())
  if(!missing(group)) {

    group_c <- dat %>% select({{group}}) %>% names

    #Remove NAs in group
    dat <-
      dat %>% drop_na(!!sym(group_c))%>%
      as.data.frame()

    if(!is.factor(data[[group_c]])) {

      return(cat("Error:", group_c, "is not a factor. Convert using the factR() function"))

    }

  } else {

    #Artificial group for Aalen
    group <- sym("grp")
    group_c <- "grp"
    dat <-
      dat %>% mutate(grp = as.factor(" "))

  }

  #Unique group levels
  group_levels <- levels(dat[,group_c])

  if(length(group_levels) > 10) {
    cat("Error: Number of levels in group exceeding 10, wrong specification of the grouping variable?")
  }


  if(!missing(vars)) {

    vars_c <- data %>% select({{vars}}) %>% names()

    if(!is.null(event.form)) {
      rhs <- paste(c(form), collapse = "")
    } else {
      rhs <- paste(c(group_c, vars_c), collapse = " + ")
      dat <- dat %>% drop_na(!!!syms(vars_c))
    }

  } else {
    rhs <- group_c
  }

  rhs_strat <- str_replace(rhs, group_c, paste0("strata(", group_c, ")"))



  horizon <- time

  out.list <- list()


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

  #Life-table
  prod <- prodlim(as.formula(paste0("Hist(", timevar_c, ", ", event_c, ") ~", group_c, collapse = "")), data=dat)

  tab <- summary(prod, times = seq(0,horizon,1), intervals = TRUE, cause=cause) %>%
    group_by(!!sym(group_c)) %>%
    mutate(cumsum = cumsum(n.event)) %>%
    ungroup() %>%
    rename(time = time1) %>%
    rename_with(~ paste0(str_replace(.x, "cuminc|surv", "est")), matches("cuminc|surv")) %>%
    select(-time0, -n.lost) %>%
    as.data.frame()

  out.list[["table"]] <- tab

  if(method == "cox") {

    #Formula generation
    form_h <- as.formula(paste(c(paste0("Hist(", timevar_c, ", ", event_c, ") ~", collapse = ""), rhs_strat), collapse = ""))
    form_hhr <- as.formula(paste(c(paste0("Hist(", timevar_c, ", ", event_c, ") ~", collapse = ""), rhs), collapse = ""))
    form_s <- as.formula(paste(c(paste0("Surv(", timevar_c, ", ", event_c, ") ~", collapse = ""), rhs_strat), collapse = ""))
    form_shr <- as.formula(paste(c(paste0("Surv(", timevar_c, ", ", event_c, ") ~", collapse = ""), rhs), collapse = ""))

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

    hres <- hres %>% filter(str_detect(level, group_c)) %>% mutate(level = str_remove_all(level, group_c),
                                                                   p.value = pvertR(pval_ex * 0.05/alpha),
                                                                   across(c(hr:upper), ~ as.numeric(format(round(.,2), nsmall=1)))) %>%
      rename(!!sym(group_c) := level) %>%
      tibble::remove_rownames()

    out.list[["hr"]] <- hres

    #ATE object
    CRest <-
      confint(ate(cr,
                    treatment = group_c,
                    data=dat,
                    times = seq(0,horizon,breaks),
                    product.limit = pl,
                    cause = cause), level = 1-alpha)

    est <-
      as.data.frame(CRest$meanRisk)



    if(plot) {

      plot_data <-
        as.data.frame(confint(ate(cr,
                      treatment = group_c,
                      data=dat,
                      times = unique(sort(c(0,dat[dat[, event_c] == 1 & dat[,timevar_c] < horizon, timevar_c],horizon))),
                      product.limit = pl,
                      cause = cause), level = 1-alpha)$meanRisk)

    }


  }

  if(method == "aalen") {

      est <- tab %>%
      filter(time %in% seq(0,horizon,breaks)) %>%
      select(time, !!sym(group_c), est:upper) %>%
      rename(se = se.est) %>%
      rename(treatment = !!sym(group_c), estimate = est)

      plot_data <-
        summary(prod, times = unique(sort(c(0,dat[dat[, event_c] == 1 & dat[,timevar_c] < horizon, timevar_c],horizon))), intervals = TRUE, cause=cause) %>%
        select(-time0, -n.lost) %>%
        rename_with(~ paste0(str_replace(.x, "cuminc|surv", "estimate")), matches("cuminc|surv")) %>%
        rename(time = time1,
               treatment = !!sym(group_c),
               se = se.estimate) %>%
        as.data.frame()

  }

  if(surv & ((method == "cox" & survscale == "OS") | method == "aalen" & survscale == "AM"))  {
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
    select(time, !!sym(group_c), est, se, lower, upper) %>%
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

  if(method == "cox") {
  rd <-
    as.data.frame(CRest$diffRisk) %>%
    filter(time == horizon) %>%
    rename(diff = estimate) %>%
    select(time, A, B,diff:p.value)
  }

  if(method == "aalen") {

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

          data.frame(time = horizon,
                     A = r[[group_c]][1],
                     B = r[[group_c]][2],
                     diff = diff,
                     se = diffse,
                     lower = diff - (stats::qnorm(1-(alpha/2)) * diffse),
                     upper = diff + (stats::qnorm(1-(alpha/2)) * diffse),
                     p.value = pval)

        })
      )

    }
  }

  rd <-
    rd %>%
    mutate(across(c(diff:upper), ~.*100),
           across(c(diff:upper), ~round(.,digits-2)),
           p.exact = pmin(1, p.value * 0.05/alpha),
           p.value = sapply(p.value * 0.05/alpha, pvertR))

  out.list[["difference"]] <- rd


  #Risk ratios
  if(method == "cox") {
  rr <-
    as.data.frame(CRest$ratioRisk) %>%
    filter(time == horizon) %>%
    rename(ratio = estimate) %>%
    select(time,A, B, ratio:p.value)%>%
    mutate(across(c(ratio:upper), ~round(.,digits-2)),
           p.exact = pmin(1, p.value * 0.05/alpha),
           p.value = sapply(p.value * 0.05/alpha, pvertR))

  out.list[["ratio"]] <- rr
  }

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
           rescil = (pmax((residual - (stats::qnorm(1-(alpha/2))*rse)), 0)),
           resciu = (pmin((residual +(stats::qnorm(1-(alpha/2))*rse)),100)),
           across(c(residual, rescil, resciu), ~. * 100))%>%
    select(time, !!sym(group_c), before, after, window, residual, rescil, resciu)%>%
    filter(time < horizon) %>%
    ungroup

  out.list[["proportions"]] <- prop

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

  out.list[["info"]] <- list(method = method,
                             timevar = timevar_c,
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

  #Remove empty group cols
  if(length(group_levels) == 1) {

    elements <- names(out.list)[names(out.list) %in% c("conditional",
                                                       "proportions",
                                                       "table",
                                                       "risks",
                                                       "time_to_event",
                                                       "counts",
                                                       "plot_data"
                                                       )]


    for(i in elements) {
      out.list[[i]] <- out.list[[i]] %>%
        select(-grp)

    }
  }

  cat(paste0("\nTotal runtime: \n"))
  cat(tockR("diff"))

  class(out.list) <- "estimatR"

  return(out.list)

}

