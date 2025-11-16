#' Perform exposure-density matching for multilevel data
#'
#'
#' @param data Dataset containing case/control indicator with index date for cases and fixed matching parameters
#' @param td.frame Dataset with time dependent matching covariates. Should be multiple rows for each patient with updated values for each parameter.
#' @param index Index date for cases, NA for controls
#' @param case 1/0 indicator for case/control status
#' @param fu End of follow up as date
#' @param td.date Name of the date column in the time dependent matching covariate dataset
#' @param fixed.vars Vector of fixed matching parameters
#' @param td.vars Vector of time-dependent matching parameters
#' @param exclude Vector of parameters that are not allowed to occur before index
#' @param n.controls Number of desired controls per case
#' @param replace whether controls should be sampled with replacement (default = T)
#' @param seed Seed
#' @param cores Cores, defaults to 4
#' @param pnr Name of PNR column
#' @param interval Interval to split birthyear into intervals (e.g. 1950-1955). Assigns a new variable named "age_group".
#'
#' @return Returns the same dataframe as the original but with n_control matches and pairs indicated with a "set" column.
#' @export
#'
#'
#'

# library(foreach)
# library(doParallel)
# library(doSNOW)

# n=100000
# c=100
#
#
# pop <- simulatR("match",
#                 n=n,
#                 match.cases = c)
#
# covariates <- simulatR("covariates",
#                 n=n)
#
# post_match <-
#   matchR3(data=pop,
#         case=case,
#         pnr = pnr,
#         fu=follow,
#         index=index,
#         td.date=date,
#         fixed.vars=c(byear, sex),
#         td.vars = c(education, cci, cancer, hema),
#         exclude = c(skinc, imm_sup),
#         td.frame = covariates,
#         n.controls=4,
#         seed=1,
#         cores = 4)

matchR <- function(data,
                   td.frame,
                   index,
                   case,
                   fu,
                   td.date,
                   fixed.vars,
                   td.vars,
                   exclude,
                   n.controls=4,
                   replace = T,
                   seed=1,
                   cores=NULL,
                   pnr=pnr,
                   interval = NULL) {

  if("date" %in% names(data)) {
    return(cat("Error: The pre_match dataframe cannot contain a variable named date"))
  }

  tickR()

  start <- tickR.start

  cat(paste0("\nInitializing matchR algorithm: ", tockR("time"), "\n"))

  if(!is.null(cores)) {
    cluster <- parallel::makeCluster(cores)
    registerDoSNOW(cluster)
  }

  pnr_c <- data %>% select({{pnr}}) %>% names()
  index_c <- data %>% select({{index}}) %>% names()
  td.date_c <- td.frame %>% select({{td.date}}) %>% names()
  vars_c <- c(data %>% select({{fixed.vars}}) %>% names(), td.frame %>% select({{td.vars}}) %>% names())
  exclude_c <- data %>% select({{exclude}}) %>% names()

  if(!is.null(interval)) {
    data[["age_group"]] <- data[["tvar"]] <- cutR(data, byear, seq(1800,3000,interval))[["byear"]]
  } else {
    data[["tvar"]] <- data[["byear"]]
  }

  vars_cc <- str_replace_all(vars_c, "byear", "tvar")

  case_s <- substitute(case)
  pnr_s <- substitute(pnr)
  fu_s <- substitute(fu)
  index_s <- substitute(index)
  td.date_s <- substitute(td.date)

  setDT(data)
  setDT(td.frame)

  cat(paste0("\nData reduction: "))

  data <- data[case == 1 | (case == 0 & fu > min(index, na.rm=T)), env=list(index = substitute(index),
                                                                            case = substitute(case),
                                                                            fu = substitute(fu))] %>%
    .[.[, Reduce(`&`, lapply(.SD, function(x) is.na(x) | x > min(index_s, na.rm=T))),.SDcols = c(exclude_c)], .SDcols= c(exclude_c), env=list(index_s = index_s)]

  cat(paste0("completed - ", tockR("time"), "\n\n"))

  cat(paste0("Merging time-dependent data frame: "))



  full_df <-
    merge(data, td.frame[pnr %in% data[, pnr],
                         env=list(pnr = substitute(pnr))], by = pnr_c)[order(-case, pnr, td.date),
                                                                       env=list(pnr = substitute(pnr),
                                                                                case = substitute(case),
                                                                                td.date = substitute(td.date))][
                                                                                  td.date <= index | (case==0 & td.date <= max(index, na.rm=T)),
                                                                                  env=list(index = substitute(index),
                                                                                           case = substitute(case),
                                                                                           td.date = substitute(td.date))][
                                                                                             , .SD[row.names(.SD) == .N | case==0], by = pnr_c,
                                                                                             env=list(pnr=substitute(pnr),
                                                                                                      case=substitute(case))][
                                                                                                        , set := match(pnr, unique(pnr)),
                                                                                                        env=list(pnr = substitute(pnr))]



  cat(paste0("\nMerging time-dependent data frame: completed - ",  tockR("time"), "\n\n"))

  cat(paste0("Partitioning of age-sex cohorts: "))

  total_cases <- data[case == 1,]

  #unique age-sex cohorts found in cases
  int <- sort(unique(interaction(total_cases$byear, total_cases$sex)))

  #Partition into age-sex cohorts filtered on presence of cases
  split_df <- split(full_df, by = c("tvar", "sex"))[as.character(int)]



  cat(paste0("\nPartitioning of age-sex cohorts: completed - ",  tockR("time"), "\n\n"))


  #Progress bar
  pb_out <- c()
  t_start <- 0
  ncase <- 0
  tcase <- nrow(full_df[full_df$case == 1,])

  for(i in seq_along(split_df)) {

    coh <- names(split_df)[i]

    cases <- split_df[[i]][case == 1]

    ncase <- ncase + nrow(cases)

    pb_out <- c(pb_out, paste0(" - Cohort: ", coh," - Current: ", nrow(cases), " Cases / ", nrow(split_df[[i]]), " - Total Status: ", ncase, " / ", tcase, " Cases       "))


  }


  if(str_detect(names(split_df)[1], "-")) {
    cat(paste0("Partitioning ", length(split_df), " cohorts: \n"))
    cat(paste0(names(split_df), collapse="\n"))
    cat("\n\n")
  } else {
    cat(paste0("Partitioning ", length(split_df), " cohorts (", names(split_df)[1], " to ", names(split_df)[length(split_df)], "): \n\n"))
  }

  cohort_list <- list()

  pb <- txtProgressBar(max = length(split_df), style = 3)
  progress <- function(n) {
    setTxtProgressBar(pb, n)
    cat(paste0("     Time: ", tockR("time"), ", runtime: ", tockR("diff"), pb_out[n]))
  }
  opts <- list(progress = progress)

  cohort_list <- foreach(j = seq_along(split_df),
                         .packages = c("tidyverse", "data.table", "foreach"),
                         .options.snow = opts) %dopar% {

                           df <- split_df[[j]]

                           cases <- df[case_s == 1, env=list(case_s = case_s)]

                           ncase <- ncase + nrow(cases)

                           control_list <- list()

                           for(c in seq_along(cases$set)) {

                             i <- cases$set[[c]]

                             itime <- df[set == i, index_s, env = list(index_s = index_s)]

                             #Cases cannot be selected as controls if already cases
                             df_c <- df[set == i | ((index_s > itime | is.na(index_s)) & fu_s > itime & td.date_s < itime),
                                        env=list(case_s = case_s,
                                                 fu_s = fu_s,
                                                 index_s = index_s,
                                                 td.date_s = td.date_s)] %>%
                               .[.[, Reduce(`&`, lapply(.SD, function(x) is.na(x) | x > itime)) | set == i,.SDcols = c(exclude_c)], .SDcols= c(exclude_c)] %>%
                               #Slice last row for each patient
                               .[unlist(.[, .I[.N], by = pnr_c][,2]),]

                             filters <- as.list(df_c[set==i, .SD, .SDcols=vars_cc])

                             setkeyv(df_c, vars_cc)

                             control_list[[c]] <- df_c[filters] %>%

                               #Not-yet cases converted to controls. Censoring at time of switch to case
                               .[, fu_s := if_else(case_s == 1 & set != i, index_s, fu_s),
                                 env = list(
                                   fu_s = fu_s,
                                   index_s = index_s,
                                   case_s = case_s)] %>%
                               .[, index_s := if_else(case_s == 1 & set != i, as.Date(NA), index_s),
                                 env = list(index_s = index_s,
                                            case_s = case_s)] %>%
                               .[, case_s := ifelse(case_s == 1 & set != i, 0, case_s),
                                 env = list(case_s = case_s)]



                             #Reroute output
                             #control_list[[c]] <- filters

                           }

                           control_list <- control_list[order(sapply(control_list, nrow))]

                           idlist <- c()

                           matches <- list()

                           for(i in seq(1,length(control_list))) {

                             set.seed(seed)

                             if(replace) {

                               m <-
                                 control_list[[i]][order(-case_s),env=list(case_s = case_s)][
                                   ,set:=first(set)][case_s == 0, env=list(case_s = case_s)][
                                     sample(.N, pmin(.N, n.controls))]

                             } else {

                               m <-
                                 control_list[[i]][
                                   order(-case_s),env=list(case_s = case_s)][
                                     ,set:=first(set)][
                                       case_s == 0 & !(pnr_s %in% idlist),
                                       env=list(case_s = case_s,
                                                pnr_s=pnr_s)][
                                                  sample(.N, pmin(.N, n.controls))]

                               idlist <- c(idlist, m[, c(pnr_s), env=list(pnr_s = pnr_s)])

                             }

                             matches[[i]] <- m

                           }

                           as.data.frame(bind_rows(cases, rbindlist(matches)))

                           #Reroute output
                           #control_list
                           #matches


                         }

  close(pb)

  cat("\nMatching complete!\n")
  cat("Total runtime: \n")
  cat(tockR("diff", start), "\n\n")

  if(!is.null(cores)) {
    parallel::stopCluster(cluster)
  }

  rbindlist(cohort_list)[order(set)][
    , "index" := nafill(index, "locf"),
    env=list(index=substitute(index))][
      , c(exclude_c[exclude_c != "sc_date"], td.date_c, "tvar") := NULL] %>%
    select({{pnr}}, case, set, index, everything()) %>%
    as.data.frame()

  #Reroute output
  #cohort_list
  #split_df



}

