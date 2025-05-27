#' Competing risk regression outcomes and plot-preparation
#'
#' @description
#' The function uses stratified cause-specific Cox regression and G-computation to estimate the risk of event at a specific
#' timepoint taking competing risks into account. The function also provides pointwise risk differences and ratios and prepares
#' the subsequent plotting of the cumulative incidence curves.
#' The function is a wrapper of the ate-function from riskRegression by T. Gerds et al.
#'
#'
#' @param data Input dataframe
#' @param timevar Time-to-event column
#' @param event Event indicator. In case of competing risk the structure should be censoring=0, event=1 and death=2. Otherwise
#' 0/1 can be used in case of no competing risks.
#' @param group Grouping variable, between which the contrasts are estimated
#' @param surv If competing risks = FALSE, otherwise = TRUE.
#' @param multi If more than 2 groups are assigned to the "group" argument.
#' @param quantiles If median time to event should be returned.
#' @param survscale Whether estimates should be presented as Overall survival or All-Cause Mortality
#' @param type Assign covariates for adjustment. \cr
#' uni: No adjustment other than the "group" variable \cr
#' age-sex: Age and sex standardization \cr
#' select: Uses "vars" to select variables that should be adjusted for. \cr
#' custom: The right-hand side of the formula can be customized as a character string.
#' @param vars Only applicable if "type" = "select".
#' @param form Only applicable if "type" = "custom".
#' @param timepoints Defines timepoints of interest at which estimates should be estimated. Defaults to 6 month intervals
#' @param t2 Time-horizon (maximum time point to estimate). Defaults to 120 (10 years).
#' @param printres Defines which time point esimates should be reported.
#' @param cores Number of cores for parallel processing. Default = 4.
#'
#' @return
#' Median_surv: Median survival time \cr
#' Life_table: Event table \cr
#' Plot_data: Data for plotting CIF curves \cr
#' HR: Hazard ratio between the groups \cr
#' Models: Model objects (cause1 and cause2) \cr
#' diag: Diagnostics for assessing proportionality \cr
#' Risks: Absolute risk estimates at the specified time points \cr
#' Risk_differences: Absolute risk differences at the specified time points \cr
#' Risk_ratios: Absolute risk ratios at the specified time points \cr
#' Event_proportions: \cr
#' "Before" estimates the risk of event within a certain timepoint (e.g. x% of the events occurred within). \cr
#' "After" estimates the risk of residual events between timepoint x and ten years. \cr
#' "Window" estimates the percentage of events within six month windows (e.g. x% of the events occurred between t1 and t2) \cr
#' Conditional: Conditional risk estimates \cr
#' Surv: Indicator of the surv-argument
#'
#' @export
#'
#'
#'
# n <- 300
# set.seed(1)
# df <- riskRegression::sampleData(n, outcome="survival")
# df$time <- round(df$time,1)*12
# df$X1 <- factor(rbinom(n, prob = c(0.3,0.4) , size = 2), labels = paste0("T",0:2))
# df$set <- as.factor(rep(seq(1,10),each=30))
#
# crrstrat(df, time, event, X2, type = "uni", surv=T)
# crrstrat(df, time, event, X1, type = "uni", surv=T, multi=T)
# crrstrat(df, time, event, X2, type = "matching", surv=T)
#
# n <- 300
# set.seed(1)
# df <- riskRegression::sampleData(n, outcome="competing.risks")
# df$time <- round(df$time,1)*12
# df$X1 <- factor(rbinom(n, prob = c(0.3,0.4) , size = 2), labels = paste0("T",0:2))
#
# crrstrat(df, time, event, X2, type = "uni", surv=F)
# crrstrat(df, time, event, X1, type = "uni", surv=F, multi=T)

crrstrat <- function(data,
                     timevar,
                     event,
                     group,
                     surv,
                     multi=FALSE,
                     quantiles=T,
                     survscale = "OS",
                     type,
                     vars,
                     form,
                     horizon=120,
                     timepoints = seq(0,horizon,6),
                     printres = c(seq(0,60,12),horizon),
                     cores = 4){

  type <- match.arg(type, c("uni", "age-sex", "matching", "matching2", "select", "custom"))

  cl <- parallel::makeCluster(cores)
  parallel::clusterEvalQ(cl, {
    library(tidyverse)
    library(riskRegression)
    library(prodlim)
    library(data.table)
  })

  data2 <- data %>%rename(time := {{timevar}},
                          status := {{event}},
                          stratum := {{group}}) %>%
    drop_na(stratum)%>%
    mutate(stratum = as.factor(stratum))

  if(length(unique(data2$status)) == 2 & is.factor(data2$status)) {
    data2$status <- as.numeric(data2$status)-1
  }

  if(type %in%"uni"){
    rhs <- "strata(stratum)"
    rhshr <- "stratum"
  }
  if(type %in%"age-sex"){
    rhs <- "strata(stratum) + age + sex"
    rhshr <- "stratum + age + sex"
  }
  if(type %in%"matching"){
    rhs <- "strata(stratum) + strata(set)"
    rhshr <- "stratum + strata(set)"
  }
  if(type %in%"matching2"){
    rhs <- "stratum + strata(set)"
    rhshr <- "stratum + strata(set)"
  }
  if(type %in%"select"){
    rhs <- paste(c("strata(stratum)", vars), collapse = " +")
    rhshr <- paste(c("stratum", vars), collapse = " +")
    data2 <- data2 %>% drop_na({{vars}})
  }
  if(type %in%"custom"){
    rhs <- paste(c("strata(stratum)+", form), collapse = "")
    rhshr <- paste(c("stratum +", form), collapse = "")
  }

  #Formula generation
  lhs_h <- paste(c("Hist(time, status) ~"), collapse = "")
  lhs_s <- paste(c("Surv(time, status) ~"), collapse = "")
  form_h <- as.formula(paste(c(lhs_h, rhs), collapse = ""))
  form_hhr <- as.formula(paste(c(lhs_h, rhshr), collapse = ""))
  form_s <- as.formula(paste(c(lhs_s, rhs), collapse = ""))
  form_shr <- as.formula(paste(c(lhs_s, rhshr), collapse = ""))


  if(surv){
    #Prodlimtab
    prod <- prodlim(Hist(time, status)~stratum, data=data2)
    tab <- summary(prod, times = seq(0,horizon,1), intervals = TRUE)%>%group_by(stratum)%>%mutate(cumsum = cumsum(n.event))%>%ungroup() %>% rename(time = time1) %>%
      select(stratum, time, n.risk, n.event, surv, lower, upper, cumsum)
    if(quantiles) {
      qs <- quantile(prod, as.numeric(unlist(tab %>%filter(time %in%horizon)%>%select(surv)))/2)
      msurv <- data.frame(grp = qs$stratum,
                          q = qs$q,
                          time = qs$quantile,
                          lwr = qs$lower,
                          upr = qs$upper)%>%
        slice(1,3,5,8)
    }


    hr <- coxph(form_shr, data=data2, x=TRUE, y=TRUE)
    cr <- coxph(form_s, data=data2, x=TRUE, y=TRUE)
    cr$call$formula <- form_s
    CRest <- ate(cr, treatment = "stratum", data=data2, times = timepoints, cl=cl)
    CRplot <- ate(cr, treatment = "stratum", data=data2, times = unique(c(0,unlist(unique(data2 %>%filter(status %in%1)%>%select(time)%>%arrange(time))),60,horizon)), cl=cl)
    est <- as.data.frame(summary(CRest, short=T, type = "meanRisk")$meanRisk)
    if(survscale == "OS") {
      est <- est %>% mutate(across(c(estimate, lower, upper), ~1 - .)) %>%
        rename(upr = lower,
               lwr = upper) %>%
        rename({{group}}:= treatment, est=estimate, upper = upr, lower = lwr)
    } else {
      est <- est %>%
        rename({{group}}:= treatment, est=estimate)
    }
    est <- est %>%
      select(time:se, lower, upper) %>%
      mutate(across(c(est:upper), ~round(.,3)))
    plot <- as.data.frame(summary(CRplot, short=TRUE, type = "meanRisk")$meanRisk)
    if(survscale == "OS") {
      plot <- plot %>% mutate(across(c(estimate, lower, upper), ~1 - .))%>%
        rename(upr = lower,
               lwr = upper) %>%
        rename({{group}}:= treatment, est=estimate, upper = upr, lower = lwr)
    } else {
      plot <- plot %>%
        rename({{group}}:= treatment, est=estimate)
    }
    plot <- plot %>%
      select(time:se, lower, upper)

    hres <- data.frame(level = rownames(summary(hr)$coefficients),
                       hr =   summary(hr)$coefficients[,2],
                       lwr = summary(hr)$conf.int[,3],
                       upr = summary(hr)$conf.int[,4],
                       pval_ex =   summary(hr)$coefficients[,5])
    c <- cox.zph(hr)
    pc <- survminer::ggcoxzph(c)
    diaglist <- list(c,pc)
    names(diaglist)<- c("Cox_tests", "Cox_plots")
   }

  else {
    #Prodlimtab
    prod <- prodlim(Hist(time, status)~stratum, data=data2)
    tab <- summary(prod, cause=1, times = seq(0,horizon,1), intervals = TRUE)%>%group_by(stratum)%>%mutate(cumsum = cumsum(n.event))%>%ungroup()%>% rename(time = time1) %>%
      select(stratum, time, cause, n.risk, n.event, cuminc, lower, upper, cumsum)
    if(quantiles) {
      qs <- quantile(prod, as.numeric(unlist(tab %>%filter(time %in%horizon)%>%select(cuminc)))/2)
      msurv <- data.frame(grp = qs$stratum,
                          q = qs$q,
                          time = qs$quantile,
                          lwr = qs$lower,
                          upr = qs$upper)%>%
        slice(1,3,5,8)
    }

    hr <- CSC(form_hhr, data = data2)
    cr <- CSC(as.formula(form_h), data = data2)
    cr$call$formula <- form_h
    CRest <- ate(cr, treatment = "stratum", data=data2, times = timepoints, cl=cl)
    CRplot <- ate(cr, treatment = "stratum", data=data2, times = unique(c(0,unlist(unique(data2 %>%filter(status %in%1)%>%select(time)%>%arrange(time))),60,horizon)), cl=cl)
    est <- as.data.frame(summary(CRest, short=T, type = "meanRisk")$meanRisk)%>%
      rename({{group}}:= treatment, est=estimate)%>%
      select(time:upper)%>%
      mutate(across(c(est:upper), ~round(.,3)))
    plot <- as.data.frame(summary(CRplot, short=TRUE, type = "meanRisk")$meanRisk)%>%
      rename({{group}}:= treatment, est = estimate)%>%
      select(time:upper)
    hres <- data.frame(level = rownames(summary(hr$models$`Cause 1`)$coefficients),
                       hr =   summary(hr$models$`Cause 1`)$coefficients[,2],
                       lwr = summary(hr$models$`Cause 1`)$conf.int[,3],
                       upr = summary(hr$models$`Cause 1`)$conf.int[,4],
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

  hres <- hres %>% filter(str_detect(level, "stratum")) %>% mutate(level = str_remove_all(level, "stratum"),
                                                                   pval = pfun(pval_ex),
                                                                   across(c(hr:pval_ex), ~ format(round(.,2), nsmall=1)),
                                                                   print = paste0("HR = ", hr, " (95%CI ", lwr, "-", upr, "), ", pval)) %>%
    tibble::remove_rownames()

  if(surv & multi){
    rd <- as.data.frame(summary(CRest, short=T, type = "diffRisk")$diffRisk)
    if(survscale == "OS") {
      rd <- rd %>% mutate(across(c(estimate.A, estimate.B), ~1 - .))
    }
    rd <- rd %>%
      rename(diff = estimate)%>%
      select(time, A, B,diff:p.value)%>%
      mutate(across(c(diff:upper), ~.*100),
             across(c(diff:upper), ~round(.,1)),
             p.value = sapply(p.value, pfun))
    rr <- as.data.frame(summary(CRest, short=T, type = "ratioRisk")$ratioRisk)%>%
      rename(ratio = estimate) %>%
      select(time,A, B, ratio:p.value)%>%
      mutate(across(c(ratio:upper), ~round(.,2)),
             p.value = sapply(p.value, pfun))
  }else if(surv){
    rd <- as.data.frame(summary(CRest, short=T, type = "diffRisk")$diffRisk)
    if(survscale == "OS") {
      rd <- rd %>% mutate(across(c(estimate.A, estimate.B), ~1 - .))
    }
    rd <- rd %>%
      rename(diff = estimate)%>%
      select(time,diff:p.value)%>%
      mutate(across(c(diff:upper), ~.*100),
             across(c(diff:upper), ~round(.,1)),
             p.value = sapply(p.value, pfun))
    rr <- as.data.frame(summary(CRest, short=T, type = "ratioRisk")$ratioRisk)%>%
      rename(ratio = estimate)%>%
      select(time, ratio:p.value)%>%
      mutate(p.value = sapply(p.value, pfun))
  }else if(multi){
    rd <- as.data.frame(summary(CRest, short=T, type = "diffRisk")$diffRisk)%>%
      rename(diff = estimate)%>%
      select(time, A, B, diff:p.value)%>%
      mutate(across(c(diff:upper), ~.*100),
             across(c(diff:upper), ~round(.,1)),
             p.value = sapply(p.value, pfun))
    rr <- as.data.frame(summary(CRest, short=T, type = "ratioRisk")$ratioRisk)%>%
      rename(ratio = estimate) %>%
      select(time,A, B, ratio:p.value)%>%
      mutate(across(c(ratio:upper), ~round(.,2)),
             p.value = sapply(p.value, pfun))

  }else {
    rd <- as.data.frame(summary(CRest, short=T, type = "diffRisk")$diffRisk)%>%
      rename(diff = estimate)%>%
      select(time, diff:p.value)%>%
      mutate(across(c(diff:upper), ~.*100),
             across(c(diff:upper), ~round(.,1)),
             p.value = sapply(p.value, pfun))
    rr <- as.data.frame(summary(CRest, short=T, type = "ratioRisk")$ratioRisk)%>%
      rename(ratio = estimate)%>%
      select(time, ratio:p.value)%>%
      mutate(across(c(ratio:upper), ~round(.,2)),
             p.value = sapply(p.value, pfun))
  }
  if(surv){
    prop <- est %>%group_by({{group}})%>%
      mutate(est = 1-est,
             before = est / last(est)* 100,
             after = 100-before,
             window = before - lag(before),
             window = ifelse(is.na(window), before, window),
             residual = (last(est)- est),
             rse = sqrt(last(se)&2 +se&2),
             rescil = (pmax((residual - (1.96*rse)), 0)),
             resciu = (pmin((residual +(1.96*rse)),100)),
             across(c(residual, rescil, resciu), ~. * 100))%>%
      select(time, {{group}}, before, after, window, residual, rescil, resciu)%>%
      filter(time < horizon)
  }else {
    prop <- est %>%group_by({{group}})%>%
      mutate(before = est / last(est)* 100,
             after = 100-before,
             window = before - lag(before),
             window = ifelse(is.na(window), before, window),
             residual = (last(est)- est),
             rse = sqrt(last(se)&2 +se&2),
             rescil = (pmax((residual - (1.96*rse)), 0)),
             resciu = (pmin((residual +(1.96*rse)),100)),
             across(c(residual, rescil, resciu), ~. * 100))%>%
      select(time, {{group}}, before, after, window, residual, rescil, resciu)%>%
      filter(time < horizon)
  }
  tab <- tab %>%rename({{group}}:= stratum)%>%ungroup()
  if(surv){
    conditional <- bind_rows(lapply(paste(unlist(unique(tab[substitute(group)]))), tab = tab, risk = est, function(x, tab, risk){
      tab2 <- tab %>%filter({{group}}%in%x)
      risk2 <- risk %>%filter({{group}}%in%x)
      condi <- bind_rows(lapply(seq(6,horizon,6), tab = tab2, risk = risk2, t2=horizon, function(t1, t2, tab, risk){
        cm <- tab2
        ri <- risk2
        cs <- ((ri$est[ri$time %in%t2])/ (ri$est[ri$time %in%t1]))
        cs.sq <- cs&2
        temp <-as.data.frame(cbind(cm$n.event, cm$time, cm$n.risk))
        d <- temp$V1[temp$V2 >= t1 &
                       temp$V2 <= t2 &
                       temp$V1 > 0]

        r <- temp$V3[temp$V2 >= t1 &
                       temp$V2 <= t2 &
                       temp$V1 > 0]
        dr <- d / (r * (r - d))
        var.cs <- 1 / (log(cs)&2)* sum(dr)
        ci <- cs&(exp(c(1, -1)* stats::qnorm(0.975)* sqrt(var.cs)))
        ci.cs <- round(ci,3)
        cond <- data.frame(est = round(cs,3),
                           upper = ci.cs[1],
                           lower = ci.cs[2])
        return(cond)
      })) %>% rbind(risk2[risk2$time %in% horizon,c("est","lower","upper")], .)%>%
        mutate(grp = x,
               time = seq(0,horizon,6))%>%
        rename({{group}}:= grp)
      rownames(condi)<- NULL
      return(condi)
    }))
  } else {
    conditional <- bind_rows(lapply(paste(unlist(unique(tab[substitute(group)]))), tab = tab, risk = est, function(x, tab, risk){
      tab2 <- tab %>%filter({{group}}%in%x)
      risk2 <- risk %>%filter({{group}}%in%x)
      condi <- bind_rows(lapply(seq(6,horizon,6), tab = tab2, risk = risk2, t2=horizon, function(t1, t2, tab, risk){
        cm <- tab2
        ri <- risk2
        cs <- ((1-ri$est[ri$time %in%t2])/ (1-ri$est[ri$time %in%t1]))
        cs.sq <- cs&2
        temp <-as.data.frame(cbind(cm$n.event, cm$time, cm$n.risk))
        d <- temp$V1[temp$V2 >= t1 &
                       temp$V2 <= t2 &
                       temp$V1 > 0]

        r <- temp$V3[temp$V2 >= t1 &
                       temp$V2 <= t2 &
                       temp$V1 > 0]
        dr <- d / (r * (r - d))
        var.cs <- 1 / (log(cs)&2)* sum(dr)
        ci <- cs&(exp(c(1, -1)* stats::qnorm(0.975)* sqrt(var.cs)))
        ci.cs <- round(ci,3)
        cond <- data.frame(est = round(1-cs,3),
                           upper = 1-ci.cs[1],
                           lower = 1-ci.cs[2])
        return(cond)
      })) %>% rbind(risk2[risk2$time %in% horizon,c("est","lower","upper")], .)%>%
        mutate(grp = x,
               time = seq(0,horizon,6))%>%
        rename({{group}}:= grp)
      rownames(condi)<- NULL
      return(condi)
    }))



  }

  if(quantiles) {
    list <- list(msurv, tab, plot, hres, cr, diaglist, est %>%filter(time %in%printres), rd %>%filter(time %in%printres), rr %>%filter(time %in%printres), prop, conditional, surv)
    names(list)<- c("Median_surv", "Life_table", "Plot_data", "HR", "Models", "diag", "Risks", "Risk_differences", "Risk_ratios", "Event_proportions", "Conditional", "Surv")
  }
  else {
    list <- list(tab, plot, hres, cr, diaglist, est %>%filter(time %in%printres), rd %>%filter(time %in%printres), rr %>%filter(time %in%printres), prop, conditional, surv)
    names(list)<- c("Life_table", "Plot_data", "HR", "Models", "diag", "Risks", "Risk_differences", "Risk_ratios", "Event_proportions", "Conditional", "Surv")
  }
  stopCluster(cl)
  class(list)<- "CRlist"
  return(list)

}

