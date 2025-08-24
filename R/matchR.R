#' matchR
#'
#' @description
#' Function for time dependent exact matching
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
#
# no = 120000
# cno= 0.02*no
#
#
# set.seed(2)
# (c <- data.frame(id = paste("pnr", rnorm(cno, 40000,1000)),
#                  case = 1,
#                  index = sample(c(sample(seq(as.Date('1990/01/01'), as.Date('2010/01/01'), by="day"))), size = cno, replace=TRUE),
#                  follow = c(sample(seq(as.Date('2015/01/01'), as.Date('2020/01/01'), by="day"), cno, replace=T)),
#                  birth = sample(c(sample(seq(as.Date('1930/01/01'), as.Date('1961/01/01'), by="day"))), size = cno, replace=TRUE),
#                  byear = sample(c(seq(1930,1961)), cno, replace=T),
#                  sex = sample(c("f","m"), cno, replace=T),
#                  skinc = sample(c(sample(seq(as.Date('2011/01/01'), as.Date('2020/01/01'), by="day")),rep(as.Date(NA),8000)), size = cno, replace=TRUE),
#                  imm_sup = sample(c(sample(seq(as.Date('2011/01/01'), as.Date('2020/01/01'), by="day")),rep(as.Date(NA),8000)), size = cno, replace=TRUE),
#                  random1 = 1,
#                  random2 = 2) %>%
#     mutate(across(c(skinc, imm_sup), ~ if_else(. > follow | .< index, follow-100, .))))
#
# (cnt <- data.frame(id = paste("pnr", rnorm(no, 40000,1000)),
#                    case = 0,
#                    follow = c(sample(seq(as.Date('1980/01/01'), as.Date('2020/01/01'), by="day"), no, replace=T)),
#                    birth = sample(c(sample(seq(as.Date('1930/01/01'), as.Date('1961/01/01'), by="day"))), size = cno, replace=TRUE),
#                    byear = sample(c(seq(1930,1961)), no, replace=T),
#                    sex = sample(c("f","m"), no, replace=T),
#                    skinc = sample(c(sample(seq(as.Date('1985/01/01'), as.Date('2000/01/01'), by="day")),rep(as.Date(NA),8000)), size = no, replace=TRUE),
#                    imm_sup = sample(c(sample(seq(as.Date('1985/01/01'), as.Date('2000/01/01'), by="day")),rep(as.Date(NA),8000)), size = no, replace=TRUE),
#                    random1 = 1,
#                    random2 = 2) %>%
#     mutate(across(c(skinc, imm_sup), ~ if_else(. > follow, follow-2000, .))))
#
# pop <- bind_rows(c, cnt)
#
# set.seed(1)
# mp <- 40
# (ses <- data.frame(var = sample(c("marital",
#                                   "education",
#                                   "income",
#                                   "cci",
#                                   "region"),
#                                 size = no*mp,
#                                 replace=TRUE),
#                    date = sample(c(sample(seq(as.Date('1980/01/01'), as.Date('2000/01/01'), by="day"))), size = no*mp, replace=TRUE),
#                    id = sample(pop$id, size = no*mp, replace=TRUE)) %>%
#     arrange(id) %>%
#     mutate(val = case_when(var %in% "income" ~ sample(paste("q", seq(1,4), sep=""), no*mp, prob = rep(0.25,4), replace=TRUE),
#                            var %in% "education" ~ sample(c("low", "medium", "high"), no*mp, prob = rep(1/3,3), replace=TRUE),
#                            var %in% "cci" ~ sample(c("cci_0",
#                                                      "cci_1",
#                                                      "cci_2-3",
#                                                      "cci_4-5",
#                                                      "cci_6+"), no*mp, prob = rep(0.20,5), replace=TRUE),
#                            var %in% "region" ~ sample(c("the_capital_region_of_denmark",
#                                                         "region_zealand",
#                                                         "the_north_denmark_region",
#                                                         "central_denmark_region",
#                                                         "the_region_of_southern_denmark"), no*mp, prob = rep(0.20,5), replace=TRUE),
#                            var %in% "marital" ~ sample(c("married",
#                                                          "unmarried",
#                                                          "divorced"), no*mp, prob = rep(0.20,3), replace=TRUE))) %>%
#     select(id, date, var, val))
#
# ses_wide <- ses %>% arrange(id, date) %>%
#   distinct(id, date, .keep_all = T) %>%
#   pivot_wider(names_from=var, values_from = val) %>%
#   fill(income, marital, region, education, cci, .direction = "down")

# t1 <- matchR(data=pop,
#               case=case,
#               pnr = id,
#               fu=follow,
#               index=index,
#               td.date=date,
#               fixed.vars=c(byear, sex),
#               td.vars = c(education, income, cci, region, marital),
#               exclude = c(skinc, imm_sup),
#               td.frame = ses_wide,
#               n.controls=4,
#               seed=1,
#               cores = 4)

# reportR(t1)


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

  #Partition into birth cohorts
  cohorts <- sort(unique(full_df[full_df[["case"]]==1,][["tvar"]]))
  cohorts_length <- length(cohorts)
  ncase <- 0
  tcase <- nrow(full_df[full_df$case == 1,])

  #Progress overview
  pb_out <- c()
  t_start <- 0

  for(i in seq_along(cohorts)) {

    coh <- cohorts[i]

    dat <- full_df[tvar == coh]

    cases <- dat[case == 1]

    ncase <- ncase + nrow(cases)

    pb_out <- c(pb_out, paste0(" - Cohort: ", coh," - Current: ", nrow(cases), " Cases / ", nrow(dat), " - Total Status: ", ncase, " / ", tcase, " Cases       "))


  }


  if(str_detect(cohorts[1], "-")) {
    cat(paste0("Partitioning ", cohorts_length, " cohorts: \n"))
    cat(paste0(cohorts, collapse="\n"))
    cat("\n\n")
  } else {
    cat(paste0("Partitioning ", cohorts_length, " cohorts (", cohorts[1], " to ", cohorts[cohorts_length], "): \n\n"))
  }

  cohort_list <- list()

  pb <- txtProgressBar(max = cohorts_length, style = 3)
  progress <- function(n) {
    setTxtProgressBar(pb, n)
    cat(paste0("     Time: ", tockR("time"), ", runtime: ", tockR("diff"), pb_out[n]))
  }
  opts <- list(progress = progress)

  cohort_list <- foreach(j = 1:cohorts_length,
                         .packages = c("tidyverse", "data.table", "foreach"),
                         .options.snow = opts) %dopar% {

                           b <- cohorts[[j]]

                           df <- full_df[tvar == b]

                           cases <- df[case_s == 1, env=list(case_s = case_s)]

                           ncase <- ncase + nrow(cases)

                           control_list <- list()

                           for(c in seq_along(cases$set)) {

                             i <- cases$set[[c]]

                             itime <- df[set == i, index_s, env = list(index_s = index_s)]

                             covars <- unlist(type.convert(df[set == i, .SD, .SDcols = vars_cc], as.is=TRUE))

                             control_list[[c]] <-
                               #Cases cannot be selected as controls if already cases
                               df[set == i | ((index_s > itime | is.na(index_s)) & fu_s > itime & td.date_s < itime),
                                  env=list(case_s = case_s,
                                           fu_s = fu_s,
                                           index_s = index_s,
                                           td.date_s = td.date_s)] %>%
                               .[.[, Reduce(`&`, lapply(.SD, function(x) is.na(x) | x > itime)) | set == i,.SDcols = c(exclude_c)], .SDcols= c(exclude_c)] %>%
                               .[, .SD[.N], by = pnr_c] %>%
                               .[.[, Reduce(`&`, lapply(.SD, function(x) x %in% covars)) | set == i,.SDcols = c(vars_cc)], .SDcols= c(vars_cc)] %>%
                               #Not-yet cases converted to controls
                               .[, index_s := if_else(case_s == 1 & set != i, as.Date(NA), index_s),
                                 env = list(index_s = index_s,
                                            case_s = case_s)] %>%
                               .[, case_s := ifelse(case_s == 1 & set != i, 0, case_s),
                                 env = list(case_s = case_s)]



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


}

