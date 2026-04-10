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
#' @param exclude.length Number of days before index where exclusions must not have occured. Default: 50 years (=never)
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

# n=1000
# c=5
#
#
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
                   exclude.length = 365.25*100,
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

  cli::cli_h2("Initializing matchR algorithm: {tockR('time')}")

  case <- data %>% select({{case}}) %>% names
  fu <- data %>% select({{follow}}) %>% names
  birth <- data %>% select({{birth}}) %>% names
  pnr <- data %>% select({{pnr}}) %>% names
  index <- data %>% select({{index}}) %>% names
  exclude <- data %>% select({{exclude}}) %>% names

  if(!is.null(interval)) {
    data[["age_group"]] <- data[["tvar"]] <- cutR(data, byear, seq(1800,3000,interval))[["byear"]]
  } else {
    data[["tvar"]] <- data[["byear"]]
  }

  fixed.vars <- data %>% select({{fixed.vars}}) %>% names %>% str_replace_all(., "byear", "tvar")

  setDT(data)

  cli::cli_h3("Data reduction")

  #Change names
  namelist <- list(pnr = pnr,
                   case = case,
                   index = index,
                   fu = fu)

  setnames(data,
           unlist(namelist),
           names(namelist))

  data <- data[case == 1 | (case == 0 & fu > min(index, na.rm=T))] %>%
    .[.[, Reduce(`&`, lapply(.SD, function(x) is.na(x) | x > min(index, na.rm=T))),.SDcols = c(exclude)], .SDcols= c(exclude)]

  cli::cli_text("Completed - {tockR('time')}")
  cli::cli_h3("Merging time-dependent data frame")

  all_indices <- data[case==1, substitute(index)]

  #If TD covariates are assigned, merge with main data
  if(!is.null(td.frame)) {
    vars <- c(fixed.vars, td.frame %>% select({{td.vars}}) %>% names)

    setDT(td.frame)

    #Merge
    data <- merge(data, td.frame, by = pnr)[order(-case, pnr, from)]

  } else {
    data[, from := substitute(birth)][
      , to := substitute(fu)]

    vars <- fixed.vars
  }

  data[, set := match(pnr, unique(pnr))]

  cli::cli_text("Completed - {tockR('time')}")
  cli::cli_h3("Partitioning of cohorts")

  #Isolate cases and get case-specific cohorts for splitting
  total_cases <- data[case == 1][, cohorts := do.call(paste, c(.SD, sep = ".")), .SDcols = c(fixed.vars)]

   #Partition into cohorts filtered on presence in cases
  split_df <-
    split(data, by = fixed.vars)[sort(unique(total_cases$cohorts))]

  cli::cli_text("Completed - {tockR('time')}")

  if(!is.null(cores)) {
    multitaskR(cores = cores)
  }

  progressr::handlers(global = TRUE)
  progressr::handlers("cli")
  options(cli.progress_bar_style = "fillsquares")

  p <- progressr::progressor(along = seq_along(split_df))

  cli::cli_h2("MATCHING")
  cli::cli_text("{length(split_df)} cohorts ({names(split_df)[1]} to {names(split_df)[length(split_df)]})")
  ##MATCHING##
  cohort_list <- future_map(seq_along(split_df), function(j) {

    tickR()

                           df <- split_df[[j]]

                           cases <- df[case == 1]

                          # control_list <- list()

                           control_list <- map(cases$set, function(i) {

                             #Index for current case
                             itime <- df[set == i, index]

                             #Keep cases with:
                             # - index > current index (Cases cannot be selected as controls if already cases)
                             #Keep controls (with:
                             # - fu > current index (alive at index) & where the current index is contained between from and to

                             df_c <- df[set == i |
                                          (index > itime |
                                             (case == 0 & (itime >= from & itime < to))),] %>%
                               #Exclude cases who were exposed within x-days before index (arg: exclude.length)
                               .[.[, Reduce(`&`, lapply(.SD, function(x) is.na(x) | !between((as.numeric(itime - x)), 0, exclude.length))) | set == i,.SDcols = c(exclude)], .SDcols= c(exclude)]

                             #Fast binary-search on matching parameters
                             filters <- as.list(df_c[set==i, .SD, .SDcols=vars])

                             setkeyv(df_c, vars)

                             df_c[filters] %>%

                               #Not-yet cases converted to controls. Censoring at time of switch to case and removing index-date
                               .[, fu := if_else(case == 1 & set != i, index, fu)] %>%
                               .[, index := if_else(case == 1 & set != i, as.Date(NA), index)] %>%
                               .[, case := ifelse(case == 1 & set != i, 0, case)]



                             #Reroute output
                             #control_list[[c]] <- df_c

                           })

                           control_list <- control_list[order(sapply(control_list, nrow))]

                           idlist <- c()

                           matches <- list()

                           #For each case, n controls are sampled
                           for(i in seq(1,length(control_list))) {

                             set.seed(seed)

                             if(replace) {

                               m <-
                                 control_list[[i]][order(-case)][
                                   ,set:=first(set)][case == 0][
                                     sample(.N, pmin(.N, n.controls))]

                              #Same as above, but a cumulative ID list excludes previous matches
                             } else {

                               m <-
                                 control_list[[i]][
                                   order(-case)][
                                     ,set:=first(set)][
                                       case == 0 & !(pnr %in% idlist)][
                                         sample(.N, pmin(.N, n.controls))]

                               idlist <- c(idlist, m[, pnr])


                             }

                             matches[[i]] <- m

                           }

                           p(paste0("Cohort: ", names(split_df)[j], " - cases/total: ", nrow(cases), "/", nrow(total_cases), " complete: ", tockR("time"), " - Runtime: ", tockR()))

                           as.data.frame(bind_rows(cases, rbindlist(matches)))

                           #   #Reroute output
                           #control_list
                           #   #matches

                           #
                           #
                         }, .options = furrr_options(
                           seed = seed
                         ))


  #Sorting + controls inherit index date and exclusion variables are omitted.
  match.df <- rbindlist(cohort_list)[order(set, -case)][
    , index := nafill(index, "locf")][
      , c(exclude[exclude != "sc_date"], "from", "to", "tvar") := NULL]

  setcolorder(match.df, c("pnr", "case", "set", "index"))

  #Returning original names
  setnames(match.df,
           names(namelist[namelist != "from"]),
           unlist(namelist[namelist != "from"]))

  cli::cli_h3("Matching complete!")
  cli::cli_text("Total runtime:")
  cli::cli_text(tockR("diff", start))

  if(dt) return(match.df) else return(as.data.frame(match.df))

  #Reroute output
  #cohort_list
  #split_df

}
