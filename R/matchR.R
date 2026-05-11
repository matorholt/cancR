#' Perform exposure-density matching for multilevel data
#'
#' @description
#' Function for performing time-dependent exact matching (exposure density sampling). For each case, n controls will be matched at index with exact covariate values.
#'
#'
#'
#' @param data Dataset containing case/control indicator with index date for cases and fixed matching parameters
#' @param td.frame Dataset with time dependent matching covariates. Should be multiple rows for each patient with updated values for each parameter.
#' @param index Index date for cases, NA for controls
#' @param case 1/0 indicator for case/control status
#' @param follow End of follow up as date
#' @param td.date Name of the date column in the time dependent matching covariate dataset
#' @param fixed.vars Vector of fixed matching parameters
#' @param td.vars Vector of time-dependent matching parameters
#' @param exclude Vector of parameters that are not allowed to occur before index
#' @param look.back look back in days period where exposure and outcomes are not allowed to happen (default is 100 years - 365.25*100)
#' @param index.shift number of days before index where exposure and outcomes are allowed, default = 0
#' @param n.controls Number of desired controls per case
#' @param replace whether controls should be sampled with replacement (default = T)
#' @param seed Seed
#' @param cores Cores, defaults to 4
#' @param pnr Name of PNR column
#' @param interval Interval to split birthyear into intervals (e.g. 1950-1955). Assigns a new variable named "age_group".
#' @param dt whether the dataframe should be returned as a data.table
#'
#' @return Returns the same dataframe as the original but with n_control matches and pairs indicated with a "set" column.
#' @export
#'
#'
#'

# n=4000
# c=10
#
# set.seed(1)
# pop <- simulatR("match",
#                n=n,
#                match.cases = c) %>%
#   mutate(byear = round(runif(n, 1955,1965),0),
#          ethnic = sample(c("euro", "africa", "asia"), n, replace=TRUE))
# #
# set.seed(1)
# covariates <- simulatR("covariates",
#                        n=n+c)
# set.seed(1)
# covariates_long <- simulatR("covariates",
#                             format = "long",
#                             n=n+c)

# tdf <-
# matchR(data=pop,
#        follow=follow,
#        fixed.vars = c(byear, sex, ethnic),
#        td.vars = c(education, cancer),
#        exclude = c(skinc, imm_sup),
#        exclude.length = 365.25*5,
#        td.frame = covariates_df,
#        n.controls=2,
#        seed=1,
#        cores = NULL,
#        dt = T)


matchR <- function(data,
                    td.frame = NULL,
                    index = index,
                    case = case,
                    follow = fu,
                    fixed.vars = c(byear, sex),
                    td.vars,
                    exclude,
                    look.back = 365.25*100,
                    index.shift = 0,
                    n.controls=4,
                    replace = T,
                    seed=1,
                    cores=4,
                    pnr=pnr,
                    birth = birth,
                    interval = NULL,
                    dt=F) {

  if("date" %in% names(data)) {
    return(cat("Error: The pre_match dataframe cannot contain a variable named date"))
  }

  tickR()

  start <- tickR.start

  on.exit({
    cli::cli_h3("Matching complete!")
    cli::cli_text("Total runtime:")
    cli::cli_text(tockR("diff", start))
  })

  cli::cli_h2("Initializing matchR algorithm: {tockR(\'time\')}")

  case    <- data %>% select({{case}})    %>% names
  fu      <- data %>% select({{follow}})  %>% names
  birth   <- data %>% select({{birth}})   %>% names
  pnr     <- data %>% select({{pnr}})     %>% names
  index   <- data %>% select({{index}})   %>% names
  exclude <- data %>% select({{exclude}}) %>% names

  if(!is.null(interval)) {
    data[["age_group"]] <- data[["tvar"]] <- cutR(data, byear, seq(1800,3000,interval))[["byear"]]
  } else {
    data[["tvar"]] <- data[["byear"]]
  }

  fixed.vars <- data %>% select({{fixed.vars}}) %>% names %>%
    str_replace_all(., "byear", "tvar")

  setDT(data)

  cli::cli_h3("Data reduction")

  namelist <- list(pnr   = pnr,
                   case  = case,
                   index = index,
                   fu    = fu)

  setnames(data, unlist(namelist), names(namelist))

  cli::cli_alert_success("Completed - {tockR(\'time\')}")
  cli::cli_h3("Merging time-dependent data frame")

  if(!is.null(td.frame)) {
    vars <- c(fixed.vars, td.frame %>% select({{td.vars}}) %>% names)

    setDT(td.frame)

    # Merge upfront as in original
    data <- merge(data, td.frame, by = pnr)[order(-case, pnr, from)]

  } else {
    data[, from := get(birth)][, to := fu]
    vars <- fixed.vars
  }

  # match() gives a globally unique integer per unique pnr
  data[, set := match(pnr, unique(pnr))]

  cli::cli_alert_success("Completed - {tockR(\'time\')}")
  cli::cli_h3("Partitioning of cohorts")

  total_cases <- data[case == 1L][
    , cohorts := do.call(paste, c(.SD, sep = ".")), .SDcols = fixed.vars]

  setkeyv(data, c(fixed.vars, "pnr"))

  split_df <- split(data, by = fixed.vars)[sort(unique(total_cases$cohorts))]

  cli::cli_alert_success("Completed - {tockR(\'time\')}")

  if(!inherits(plan(), "multisession") & !is.null(cores)) {
    multitaskR(cores = cores)
  }

  progressr::handlers(global = TRUE)
  progressr::handlers("cli")
  options(cli.progress_bar_style = "fillsquares")

  p <- progressr::progressor(along = seq_along(split_df))

  cli::cli_h2("MATCHING")
  cli::cli_text("{length(split_df)} cohorts ({names(split_df)[1]} to {names(split_df)[length(split_df)]})")

  cohort_list <- future_map(seq_along(split_df), function(j) {

    tickR()

    df    <- split_df[[j]]
    cases <- df[case == 1L]

    #Case status at index
    cases_td <- cases[index >= from & index < to]

    #Matching parameters + case_set and itime for matching
    case_times <- cases_td[, c(list(case_set = set, itime = index), .SD),
                           .SDcols = vars]



    #Controls
    controls_df <- df[case == 0L]
    setkeyv(controls_df, vars)

    #Matching all control candidates to all cases (case_set = matched case)
    cand <- controls_df[case_times,
                        on              = vars,
                        allow.cartesian = TRUE,
                        nomatch         = 0
    ][itime >= from & itime < to & fu > itime]

    #Exclude criteria before index
    if(length(exclude) > 0L) {
      excl_mask <- Reduce(`&`, lapply(exclude, function(col) {
        x <- cand[[col]]
        is.na(x) | !between(as.numeric(cand$itime - x), index.shift, look.back)
      }))
      cand <- cand[excl_mask]
    }

    #Not yet cases
    future_cases_df <- df[case == 1L]
    setkeyv(future_cases_df, vars)

    future_cands <- future_cases_df[case_times,
                                    on              = vars,
                                    allow.cartesian = TRUE,
                                    nomatch         = 0
    ][set != case_set & index > itime][itime >= from & itime < to]



    if(nrow(future_cands) > 0L) {

      if(length(exclude) > 0L) {
        excl_mask_fc <- Reduce(`&`, lapply(exclude, function(col) {
          x <- future_cands[[col]]
          is.na(x) | !between(as.numeric(future_cands$itime - x), index.shift, look.back)
        }))
        future_cands <- future_cands[excl_mask_fc]
      }

      future_cands[, fu    := index]
      future_cands[, index := as.Date(NA)]
      future_cands[, case  := 0L]
    }

    #Combine controls + not yet cases
    all_cands <- rbindlist(
      list(cand, future_cands),
      use.names = TRUE,
      fill      = TRUE
    )

    set.seed(seed)

    #With replacement
    if(replace) {

      matched_controls <- all_cands[
        , .SD[sample(.N, pmin(.N, n.controls))],
        by = case_set
      ]

    } else {

      #Sort by number of candidates
      case_order <- all_cands[
        , .(n_cands = .N), by = case_set
      ][order(n_cands), case_set]

      used_ids <- character(0L)

      matched_controls <-
        rbindlist(map(case_order, function(cs) {

          eligible <- all_cands[case_set == cs & !(pnr %chin% used_ids)]


          m <- eligible[sample(.N, pmin(.N, n.controls))]

          used_ids <<- c(used_ids, m$pnr)

          m
        }), use.names = TRUE, fill = TRUE)

    }

    #Assign case-set to set
    matched_controls[, set := case_set][, case_set := NULL]

    #Collect all case/control pairs
    out <- rbindlist(
      list(cases_td, matched_controls),
      use.names = TRUE,
      fill      = TRUE
    )[, itime := NULL]

    p(message = paste0(
      "Cohort: ", names(split_df)[j], " (",j, "/", length(split_df),
      ") - cases/total: ", nrow(cases_td), "/", nrow(total_cases),
      " complete: ", tockR("time"),
      " - Runtime: ", tockR()
    ))

    out

  }, .options = furrr_options(seed = seed))

  #Drop cols
  drop <- c(exclude[exclude != "sc_date"], "from", "to", "tvar")

  #Controls inherit indices
  match.df <- rbindlist(cohort_list, use.names = TRUE, fill = TRUE)[
    order(set, -case)][
      , index := nafill(index, "locf")][
        , (drop) := NULL]

  setcolorder(match.df, c("pnr", "case", "set", "index"))

  # Restore original column names
  setnames(match.df, names(namelist), unlist(namelist))

  cli::cli_h3("Matching complete!")
  cli::cli_text("Total runtime:")
  cli::cli_text(tockR("diff", start))

  if(dt) return(match.df) else return(as.data.frame(match.df))

}
