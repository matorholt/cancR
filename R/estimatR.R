#' Absolute risk estimation of time-to-event data with competing risks
#'
#' @description
#' Absolute risk estimation and comparison of two or more groups with a time-to-event outcome.
#' Wrapper for the prodlim and ate-functions in riskRegression.
#'
#' @param data data.frame
#' @param timevar time to event column
#' @param event event column
#' @param group grouping variable (optional)
#' @param survscale Whether results should be presented as cumulative incidences or overall survival (1-risk)
#' @param method whether the estimator should be Aalen-Johansen or cause specific Cox regression. If no groups, AJ i automatically chosen.
#' @param strata whether the grouping variable should be stratified for separate baseline hazards between groups
#' @param vars vector containing variables to adjust for in the Cox models
#' @param event.form character string for custom right-hand side formula in the Cox models
#' @param time time horizon of interest. Defaults to 60 (e.g. 5-years)
#' @param breaks interim time points of interest. Defaults to 12 months (1-year gaps)
#' @param cause cause of interest, default = 1
#' @param pl Whether product.limit in ATE shoulde be T or F (default = T)
#' @param digits number of digits risk estimates in the returned results
#' @param event.digits rounding of event times. Default is 2 to preserve exact times
#' @param alpha alpha level for the estimation of confidence intervals and p-values. Default = 0.05
#' @param survtime whether median time to event should be calculated (default = T)
#' @param proportions whether risk of event in different windows should be estimated (default = T)
#' @param conditional whether conditional risk at the time horizon should be calculated (default = T)
#' @param diagnostics whether model diagnostics should be beformed
#' @param diag.method the diagnostic method. Choose between "supremum" (default) for the Lin, Wei, Ying test for proportionality or "schoenfeld"
#' @param plot whether estimates for plotR should be performed (default = T)
#' @param unique.events whether only unique event times should be kept for each group (default = T)
#' @param verbose whether status should be printed to the console (default = T)
#' @param dt whether data.tables should be returned (default = F)
#'
#'
#' @return List of class "estimatR" containing risk estimates, see details for content.
#'
#' @details
#' The function returns a named list with the following elements:
#' \itemize{
#'   \item \strong{table}: Life table
#'   \item \strong{models}: Cause-specific model objects
#'   \item \strong{risks}: Absolute risk estimates at the specified time points
#'   \item \strong{plot_data}: Data for plotting CIF/OS curves
#'   \item \strong{time_to_event}: Median survival time
#'   \item \strong{hr}: Hazard ratio between the groups
#'   \item \strong{difference}: Absolute risk difference at the specified time horizons
#'   \item \strong{ratio}: Absolute risk ratio at the specified time horizon
#'   \item \strong{counts}: Event and group counts in the contrasted groups
#'   \item \strong{info}: Information on arguments for extraction
#' }
#' Optionally, the following elements may also be present:
#' \itemize{
#'   \item \strong{diagnostics}: Diagnostics for assessing proportionality
#'   \item \strong{proportions}: Timing of events, with four sub-elements:
#'   \itemize{
#'     \item \strong{before}: Risk of event within a certain timepoint (e.g. x\% of events occurred within)
#'     \item \strong{after}: Risk of residual events between timepoint x and ten years
#'     \item \strong{window}: Percentage of events within six month windows (e.g. x\% of events occurred between t1 and t2)
#'     \item \strong{residual}: Residual event estimates
#'   }
#'   \item \strong{conditional}: Conditional risk estimates
#' }
#'
#' @export
#'
#' @examples
#' estimatR(analysis_df, t_event, event, g2, vars = c(x4, x5))
#'
#'

#t1 <- estimatR(analysis_df, t_event, event, g2)

#Multiple causes
# res <-
#   estimatR(analysis_df,
#            timevar=t_event,
#            event=event,
#            group=g4,
#            strata = F,
#            method = "cox",
#            survscale = "AM",
#            vars = c(x1,x2,x6),
#            #event.form = "g4 * x1 + x3",
#            cause = 1,
#            plot=T,
#            event.digits = 2,
#            diagnostics = F,
#            conditional = T,
#            proportions = T,
#            survtime = T,
#            unique.events = F,
#            dt=F)

estimatR <- function(data,
                     timevar,
                     event,
                     group,
                     survscale = "AM",
                     method = "cox",
                     strata = T,
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
                     proportions = T,
                     conditional = T,
                     diagnostics = F,
                     diag.method = "supremum",
                     plot=T,
                     unique.events = T,
                     verbose = T,
                     dt = F) {

  if(verbose) cli::cli_h2("Initializing estimatR algorithm: {tickR(print=T, cli=F)}")

  start <- tickR.start

  if(verbose) {
  on.exit({
    cli::cli_h3("Estimation complete!")
    cli::cli_text("Total runtime:")
    cli::cli_text(tockR("diff", start))
  })
  }

  ##############################################  Custom Functions  ##############################################

  switchR <- function(input) {
    input[, est   :=  1 - est] %>%
      .[,   lower  :=  1 - lower] %>%
      .[,   upper  :=  1 - upper] %>%
      setnames(c("lower", "upper"), c("upper", "lower"))

    names <- names(input)
    names[which(names %in% c("lower", "upper"))] <- c("lower", "upper")
    setcolorder(input, names)

     # setcolorder(., `[<-`(names(.), c(lower_pos, upper_pos), c("lower", "upper")))
  }

  if(verbose) cli::cli_progress_message("Preparing data:")
  tickR()

  dat <- as.data.table(data)

  for(i in c("data",
             "timevar",
             "event")) {

    if(i %nin% names(match.call())) {

      return(cli::cli_alert_danger("Error: Argument {i} is not specified"))
    }

  }

  if(method %nin% c("cox", "aalen")) {

    cli::cli_alert_danger("Error: Invalid choice of method. Choose between cox and aalen")

  }

  if(!missing(vars)) method <- "cox"
  if(missing(group)) method <- "aalen"

  timevar_c <- defusR(timevar)
  event_c <- defusR(event)

  if(!missing(group)) {

    group_c <- defusR(group)

    if(!is.factor(dat[[group_c]])) {

      return(cli::cli_alert_danger("Error: {group_c} is not a factor. Convert using the factR() function"))

    }

    dat <- dat[!is.na(get(group_c)) & (group_c) != ""]

  } else {

    #Artificial group for Aalen
    group_c <- "grp"
    dat[, grp := factor("grp")]

  }

  #Unique group levels
  group_levels <- levels(dat[[group_c]])

  if(length(group_levels) > 25) {
    return(cli::cli_alert_danger("Error: Number of levels in group exceeding 25, wrong specification of the grouping variable?"))
  }

  if(!missing(vars) || !is.null(event.form)) {

    if(!is.null(event.form)) {

      rhs <- event.form
      vars <- str_trim(unlist(str_split(event.form, "\\+|\\*|\\:")))
      vars_c <- vars[vars != group_c]

    } else {

      vars_c <- defusR(vars)
      rhs <- paste(c(group_c, vars_c), collapse = " + ")
    }

    dat <- dat[complete.cases(dat[, ..vars_c])]

  } else {

    vars_c <- NULL
    rhs <- group_c
  }

  if(strata) rhs <- str_replace(rhs, group_c, paste0("strata(", group_c, ")"))

  horizon <- time

  out.list <- list()

  causes <- length(unique(dat[[event_c]]))-1

  if(cause > causes) return(cli::cli_alert_danger("Error: {cause} causes specified, but only {causes} cause in {event_c}"))

  if(causes == 1) {
    surv <- TRUE
    if(is.factor(dat[[event_c]])) dat[, (event_c) := as.numeric(get(event_c)) - 1]
  } else {
    surv <- FALSE
  }

  #Multiple groups
  multi <- length(group_levels) > 2

  #Rounding of event times
  dat[, (timevar_c) := round(get(timevar_c), event.digits)]

  #Event times
  event_times <- unique(sort(c(0, dat[get(event_c) == cause & get(timevar_c) < horizon][[timevar_c]], horizon)))


  if(verbose) cli::cli_alert_success("Preparing data: Complete {tockR(\'time\', cli=F)}, Runtime = {tockR(cli=F)}")

  ##############################################  Prodlim  ##############################################


  if(verbose) cli::cli_progress_message("Life-tables:")
  tickR()


  prod <- prodlim(as.formula(paste0("Hist(", timevar_c, ", ", event_c, ") ~ ", group_c, collapse = "")), data=dat)

  tab <- as.data.table(summary(prod, times = seq(0,horizon,1), intervals = TRUE, cause=cause)) %>%
    .[, cumsum := cumsum(n.event), by = c(group_c)] %>%
    .[, c("time0") := NULL]

  setnames(tab,
           names(tab),
           str_replace_all(names(tab), c("cuminc|surv" = "est", "time1" = "time")))

  #All risks start at zero
  if(surv) {
    switchR(tab)
  }

  out.list[["table"]] <- tab

  if(verbose) cli::cli_alert_success("Life-tables: Complete {tockR(\'time\', cli=F)}, Runtime = {tockR(cli=F)}")

  ##############################################  Aalen  ##############################################

  if(verbose) cli::cli_progress_message("Modelling:")
  tickR()

  if(method == "aalen") {

    est <- tab[time %in% seq(0,horizon,breaks)] %>%
      .[, c("time", group_c, names(tab)[which(names(tab) == "est"):which(names(tab) == "upper")]), with = FALSE]

    setnames(est,
             "se.est",
             "se")


  } #aalen

  ##############################################  COX  ##############################################


  if(method == "cox") {


    model_form <- as.formula(paste0(c(ifelse(surv, "Surv(", "Hist("), timevar_c, ", ", event_c, ") ~ ", rhs), collapse = ""))

    #Model specification and diagnostics
    if(surv) {
      model <- coxph(model_form, data=dat, x=TRUE, y=TRUE)
    } else {
      model <- CSC(model_form, data = dat)
    }

    model$call$formula <- model_form

    out.list[["models"]] <- model

    #ATE object
    ate_obj <-
      confint(ate(model,
                  treatment = group_c,
                  data=dat,
                  times = seq(0,horizon,breaks),
                  product.limit = pl,
                  cause = cause,
                  verbose = F), level = 1-alpha)

    est <- ate_obj$meanRisk[, c("estimator") := NULL]

    setnames(est,
             c("treatment", "estimate"),
             c(group_c, "est"))

    #Extract Hazard ratios
    if(!strata) {

      if(surv) hr_model <- summary(model) else hr_model <- summary(model$models[[cause]])

      hres <- data.table(level = rownames(hr_model$coefficients),
                         hr =   hr_model$coefficients[,2],
                         lower = hr_model$conf.int[,3],
                         upper = hr_model$conf.int[,4],
                         pval_ex = hr_model$coefficients[,5]) %>%
        .[str_detect(level, group_c),] %>%
        .[, `:=` (level = str_remove_all(level, group_c),
                  p.value = pvertR(pval_ex * 0.05/alpha))] %>%
        .[, (c("hr", "lower", "upper")) := map(.SD, ~ numbR(.x, 2)), .SDcols = c("hr", "lower", "upper")]

      setnames(hres,
               "level",
               group_c)

      out.list[["hr"]] <- hres

    }

  }

  if(verbose) cli::cli_alert_success("Modelling: Complete {tockR(\'time\', cli=F)}, Runtime = {tockR(cli=F)}")

  ##############################################  Plot data  ##############################################

  if(plot) {
    if(verbose) cli::cli_progress_message("Plot data:")
    tickR()

    plot_tab <- as.data.table(summary(prod, times = event_times, intervals = TRUE, cause=cause))

    setnames(plot_tab,
             names(plot_tab),
             str_replace_all(names(plot_tab), c("cuminc|surv" = "est",
                                                 "time1" = "time",
                                                 "se.est" = "se")))

    if(method == "aalen") {

      plot_data <-
        plot_tab %>%
        .[, (intersect(c("time0", "cause", "n.risk", "n.event"), names(.))) := NULL]

      if(surv) switchR(plot_data)

    }

    if(method == "cox") {

      plot_data <-
        confint(ate(model,
                    treatment = group_c,
                    data=dat,
                    times = event_times,
                    product.limit = pl,
                    cause = cause,
                    verbose = F), level = 1-alpha)$meanRisk[, c("estimator") := NULL]


    setnames(plot_data,
               c("treatment", "estimate"),
               c(group_c, "est"))

    plot_data <- joinR(plot_data, plot_tab[, c(group_c, "time", "n.lost"), with = FALSE], by = c(group_c, "time"))

      }

    #Filter other-group events
    if(unique.events) {
    event_times <- dat[get(event_c) == 1, .(time = get(timevar_c)), by = group_c]
    break_times <- dat[, .(time = seq(0,horizon,breaks)), by = group_c]
    keep_times  <- unique(rbind(event_times, break_times))
    plot_data   <- plot_data[keep_times, on = c(group_c, "time"), nomatch = 0]
    }

    #Duplicate last row
    plot_data <- plot_data[order(get(group_c), time)] %>%
      .[, .SD[c(1:.N, .N)], by = group_c] %>%
      .[, time := replace(time, .N, time[.N] + 0.6), by = group_c]

      out.list[["plot_data"]] <- if(survscale == "OS") switchR(plot_data) else plot_data

    if(verbose) cli::cli_alert_success("Plot data: Complete {tockR(\'time\', cli=F)}, Runtime = {tockR(cli=F)}")
  } #plot

  ##############################################  Diagnostics  ##############################################

  if(diagnostics) {
    if(verbose) cli::cli_progress_message("Diagnostics:")
    tickR()

    if(diag.method == "supremum") {

      prop <- timereg::prop

      out.list[["diagnostics"]] <-  map(sort(unique(dat[[event_c]])), ~ {

        form <- as.formula(paste0("Surv(", timevar_c, ", ", event_c, " == ", 1, ") ~ ",  paste0("prop(", c(group_c, vars_c), ")", collapse = " + ")))

        set.seed(1)
        out <- timereg::cox.aalen(
          form,
          data  = dat,
          n.sim = 1000   # number of simulations for the supremum test
        )

        n_props <- length(c(group_c, vars_c))
        ncols   <- min(n_props, 3)
        nrows   <- ceiling(n_props / ncols)

        #par(mfrow = c(nrows, ncols))
        par(mfrow = c(nrows, ncols))
        plot(out, score = 1)


        p <- recordPlot()

        return(list(res = out,
                    plot = p))

      }) %>% set_names(paste0("cause_", sort(unique(dat[[event_c]]))))

    } else {

      out.list[["diagnostics"]] <- map(model$models, ~ {

        res <- cox.zph(.x)

        plot <- survminer::ggcoxzph(cox.zph(.x))

        return(lst(res,
                   plot))

      }) %>% set_names(paste0("cause_", seq_len(length(names(model$models)))))

    }
    if(verbose) cli::cli_alert_success("Diagnostics: Complete {tockR(\'time\', cli=F)}, Runtime = {tockR(cli=F)}")
  }

  ##############################################  Time-to-event  ##############################################

  if(survtime) {
    if(verbose) cli::cli_progress_message("Time-to-event:")
    tickR()

    if(method == "aalen" || !plot) {



      #quantile = Risk(t=horizon) / 2
      qs <- c(tab$est[tab$time == horizon]) / 2

      msurv <- as.data.table(list2DF(quantile(prod, qs))[seq(1, length(group_levels)^2, length(group_levels)+1),])

      if(!surv) msurv[, cause := NULL]

      setnames(msurv,
               "quantile",
               "median")

    } else {

      tte <- copy(plot_data)

      ref <- tte[time == 120, .(risk_ref = est, lower_ref = lower, upper_ref = upper), by = group_c]

      msurv <- tte[ref, on = group_c][, .(
        quantile = risk_ref[1] / 2,
        median   = time[which.min(abs(est   - risk_ref[1]  / 2))],
        lower    = time[which.min(abs(upper - lower_ref[1] / 2))],
        upper    = time[which.min(abs(lower - upper_ref[1] / 2))]
      ), by = group_c]



    }


    out.list[["time_to_event"]] <- msurv

    if(verbose) cli::cli_alert_success("Time-to-event: Complete {tockR(\'time\', cli=F)}, Runtime = {tockR(cli=F)}")
  }



  ##############################################  Counts / RD / RR  ##############################################

   counts <-
     dat[, .(n.events = sum(get(event_c) == cause),
             total = .N), by = group_c]

  out.list[["counts"]] <- counts

  #Risks
  if(length(group_levels) > 1) {
    if(verbose) cli::cli_progress_message("Contrasts:")
    tickR()

    if(method == "cox") {

      contrast_list <- ate_obj[c("diffRisk", "ratioRisk")]

    }

    if(method == "aalen") {

        est_horizon <- est[time == horizon]
        combos <- combn(seq_along(group_levels), 2)

        group_contrasts <- map(c(1:ncol(combos)), ~ {

          r <-
            est_horizon[as.vector(combos[, .x]),]

          #Differences
          diff <- r$est[2] - r$est[1]
          se_d <- sqrt(r$se[2]^2 + r$se[1]^2)
          lower_d <- diff - 1.96 * se_d
          upper_d <- diff + 1.96 * se_d
          pval_d <- 2*pnorm(abs(diff/se_d), lower=FALSE)

          #Ratios
          ratio <- r$est[2] / r$est[1]
          log_r <- log(ratio)
          se_r <- sqrt((r$se[2]/r$est[2])^2 + (r$se[1]/r$est[1])^2)
          lower_r <- exp(log_r - 1.96 * se_r)
          upper_r <- exp(log_r + 1.96 * se_r)
          pval_r <- 2*pnorm(abs(log_r / se_r), lower=FALSE)

          frame <- data.frame(time = horizon,
                              A = r[[group_c]][1],
                              B = r[[group_c]][2])

          list("diff" = cbind(frame,
                              data.table(estimate = diff,
                                         se = se_d,
                                         lower = lower_d,
                                         upper = upper_d,
                                         p.value = pval_d)),
               "ratio" = cbind(frame,
                               data.table(estimate = ratio,
                                          se = se_r * ratio,
                                          lower = lower_r,
                                          upper = upper_r,
                                          p.value = pval_r)))

        })

        contrast_list <- list(rbindlist(map(group_contrasts, ~ .x$diff)),
                              rbindlist(map(group_contrasts, ~ .x$ratio)))

    }

    contrasts <- map(seq_along(contrast_list), ~ {

      out <- contrast_list[[.x]][time == horizon][, c("time", "A", "B", "estimate", "se", "lower", "upper", "p.value")] %>%
        .[, c("estimate", "se", "upper", "lower") := map(.SD, function(x) round(x, digits) * ifelse(.x == 1, 100, 1)), .SDcols = c("estimate", "se", "upper", "lower")] %>%
        .[, `:=` (p.exact = pmin(0.99, p.value * 0.05/alpha),
                  p.value = map_vec(p.value * 0.05/alpha, pvertR))]

      setnames(out,
               "estimate",
               ifelse(.x == 1, "diff", "ratio"))

    })

    out.list[["difference"]] <- contrasts[[1]]
    out.list[["ratio"]] <- contrasts[[2]]

    if(verbose) cli::cli_alert_success("Contrasts: Complete {tockR(\'time\', cli=F)}, Runtime = {tockR(cli=F)}")
  }



  ##############################################  Proportions  ##############################################

  if(proportions) {
    if(verbose) cli::cli_progress_message("Event proportions:")
    tickR()

    logit     <- function(p) log(p / (1 - p))
    inv_logit <- function(x) 1 / (1 + exp(-x))

    logit_ci <- function(est, se, upper = TRUE) {
      eps <- 1e-6
      fcase(
        is.na(se),  NA_real_,
        est <= 0,   0,
        est >= 1,   1,
        default     = inv_logit(logit(est) + (if (upper) 1 else -1) * 1.96 * se / (pmax(eps, est) * pmax(eps, 1 - est)))
      )
    }

    props <- c("before", "after", "window", "residual")

    props_list <- map(props, \(p) {

      out <- copy(est)

      if (p == "before") {
        out[, ref_est := est[.N], by = group_c]
        out[, ref_se  := se[.N],  by = group_c]
        out[, `:=` (
          est = fifelse(ref_est > 0, est / ref_est, NA_real_),
          se  = fifelse(est > 0 & ref_est > 0,
                        (est / ref_est) * sqrt((se / est)^2 + (ref_se / ref_est)^2),
                        NA_real_)
        )]

      } else if (p == "after") {
        out[, ref_est := est[.N], by = group_c]
        out[, ref_se  := se[.N],  by = group_c]
        out[, `:=` (
          est = fifelse(ref_est > 0, 1 - est / ref_est, NA_real_),
          se  = fifelse(est > 0 & ref_est > 0,
                        (est / ref_est) * sqrt((se / est)^2 + (ref_se / ref_est)^2),
                        NA_real_)
        )]

      } else if (p == "window") {
        out[, ref_est := est[.N], by = group_c]
        out[, ref_se  := se[.N],  by = group_c]
        out[, before_est := est / ref_est]
        out[, before_se  := (est / ref_est) * sqrt((se / est)^2 + (ref_se / ref_est)^2)]
        out[, `:=` (
          est = before_est - shift(before_est),
          se  = sqrt(before_se^2 + shift(before_se)^2)
        ), by = group_c]

      } else if (p == "residual") {
        out[, ref_est := est[.N], by = group_c]
        out[, ref_se  := se[.N],  by = group_c]
        out[, `:=` (
          est = ref_est - est,
          se  = fifelse(est > 0 & ref_est > 0,
                        sqrt(se^2 + ref_se^2),
                        NA_real_)
        )]
      }

      # CIs
      if (p == "window") {
        out[, `:=` (
          lower = pmax(0, est - 1.96 * se),
          upper = pmin(1, est + 1.96 * se)
        )]
      } else {
        out[, `:=` (
          lower = logit_ci(est, se, upper = FALSE),
          upper = logit_ci(est, se, upper = TRUE)
        )]
      }

      out[, c("est", "se", "lower", "upper") := map(.SD, ~ round(.x, digits) * 100),
          .SDcols = c("est", "se", "lower", "upper")]

      out[, c(group_c, "time", "est", "se", "lower", "upper"), with = FALSE]

      if(dt) return(out) else return(as.data.frame(out))

    }) %>% set_names(props)



    out.list[["proportions"]] <- props_list
    if(verbose) cli::cli_alert_success("Event proportions: Complete {tockR(\'time\', cli=F)}, Runtime = {tockR(cli=F)}")
  }



  ##############################################  Conditional  ##############################################

  if(conditional) {
    if(verbose) cli::cli_progress_message("Conditional risk:")
    tickR()

    cond_est <- copy(est)

  cond_res <- cond_est %>%
    .[, est_surv := 1 - est] %>%
    .[, se_log   := se / est_surv] %>%
    .[, log_ratio    := log(est_surv[.N]) - log(est_surv),
      by = group_c] %>%
    .[, se_log_ratio := sqrt(se_log[.N]^2 + se_log^2),
      by = group_c] %>%
    .[, ratio_lo := exp(log_ratio - 1.96 * se_log_ratio)] %>%
    .[, ratio_hi := exp(log_ratio + 1.96 * se_log_ratio)] %>%
    .[, cond     := 1 - exp(log_ratio)] %>%
    .[, cond_lo  := pmax(0, 1 - ratio_hi)] %>%
    .[, cond_hi  := pmin(1, 1 - ratio_lo)] %>%
  .[, c("cond", "cond_lo", "cond_hi") :=
      .(fifelse(time == 0, est[.N], cond),
        fifelse(time == 0, lower[.N], cond_lo),
        fifelse(time == 0, upper[.N], cond_hi)),
    by = group_c] %>%
  .[, c("time", group_c, "cond", "cond_lo", "cond_hi"), with = FALSE]

  setnames(cond_res,
           c("cond_lo", "cond_hi"),
           c("lower", "upper"))

    out.list[["conditional"]] <- cond_res
    if(verbose) cli::cli_alert_success("Conditional risk: Complete {tockR(\'time\', cli=F)}, Runtime = {tockR(cli=F)}")
  }

  est[, c("est", "lower", "upper") := map(.SD, ~ round(.x, digits)), .SDcols = c("est", "lower", "upper")]

  if(survscale == "OS") switchR(est)

  out.list[["risks"]] <- est

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
                                                       "table",
                                                       "risks",
                                                       "time_to_event",
                                                       "counts",
                                                       "plot_data"
    )]


    for(i in elements) {
      out.list[[i]] <- as.data.table(out.list[[i]])[, c("grp") := NULL]

    }
  }

  if(!dt) {
    non_dt <- c("diagnostics", "info", "proportions", "models")

    out.list[names(out.list) %nin% non_dt] <-
      map(out.list[names(out.list) %nin% non_dt], ~ as.data.frame(.x))

  }

  class(out.list) <- "estimatR"

  return(out.list)

}
