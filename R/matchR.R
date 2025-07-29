#' matchR
#'
#' @description
#' Function for time dependent exact matching
#'
#'
#' @param data Dataset containing case/control indicator with index date for cases and fixed matching parameters
#' @param td_frame Dataset with time dependent matching covariates. Should be multiple rows for each patient with updated values for each parameter.
#' @param index Index date for cases, NA for controls
#' @param case 1/0 indicator for case/control status
#' @param fu End of follow up as date
#' @param td_date Name of the date column in the time dependent matching covariate dataset
#' @param fixed_vars Vector of fixed matching parameters
#' @param td_vars Vector of time dependent matching parameters
#' @param exclude Vector of parameters that are not allowed to occur before index
#' @param n_controls Number of desired controls per case
#' @param seed Seed
#' @param cores Cores, defaults to 4
#' @param pnr Name of PNR column
#'
#' @return Returns the same dataframe as the original but with n_control matches and pairs indicated with a "set" column.
#' @export
#'
#'
#'

# library(foreach)
# library(doParallel)
#
# no = 40000
# cno= 0.0025*no
#
#
# set.seed(2)
# (c <- data.frame(id = paste("pnr", rnorm(cno, 40000,1000)),
#                  case = 1,
#                  index = sample(c(sample(seq(as.Date('1990/01/01'), as.Date('2010/01/01'), by="day"))), size = cno, replace=TRUE),
#                  follow = c(sample(seq(as.Date('2015/01/01'), as.Date('2020/01/01'), by="day"), cno, replace=T)),
#                  byear = sample(c(seq(1959,1960)), cno, replace=T),
#                  sex = sample(c("F","M"), cno, replace=T),
#                  skinc = sample(c(sample(seq(as.Date('2011/01/01'), as.Date('2020/01/01'), by="day")),rep(as.Date(NA),8000)), size = cno, replace=TRUE),
#                  imm_sup = sample(c(sample(seq(as.Date('2011/01/01'), as.Date('2020/01/01'), by="day")),rep(as.Date(NA),8000)), size = cno, replace=TRUE),
#                  random1 = 1,
#                  random2 = 2) %>%
#     mutate(across(c(skinc, imm_sup), ~ if_else(. > follow | .< index, follow-100, .))))
#
# (cnt <- data.frame(id = paste("pnr", rnorm(no, 40000,1000)),
#                    case = 0,
#                    follow = c(sample(seq(as.Date('1980/01/01'), as.Date('2020/01/01'), by="day"), no, replace=T)),
#                    byear = sample(c(seq(1959,1960)), no, replace=T),
#                    sex = sample(c("F","M"), no, replace=T),
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
# (ses <- data.frame(var = sample(c("married",
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
#                            var %in% "education" ~ sample(c("education_low", "education_medium", "education_high"), no*mp, prob = rep(1/3,3), replace=TRUE),
#                            var %in% "cci" ~ sample(as.character(seq(1,6)), no*mp, prob = rep(1/6,6), replace=TRUE),
#                            var %in% "region" ~ sample(c("the_capital_region_of_denmark",
#                                                         "region_zealand",
#                                                         "the_north_denmark_region",
#                                                         "central_denmark_region",
#                                                         "the_region_of_southern_denmark"), no*mp, prob = rep(0.20,5), replace=TRUE),
#                            var %in% "married" ~ "married")) %>%
#     select(id, date, var, val))
#
# ses_wide <- ses %>% arrange(id, date) %>%
#   distinct(id, date, .keep_all = T) %>%
#   pivot_wider(names_from=var, values_from = val) %>%
#   fill(income, married, region, education, cci, .direction = "down")
#
# t1 <- matchR(data=pop,
#              case=case,
#              pnr = id,
#              fu=follow,
#              index=index,
#              td_date=date,
#              fixed_vars=c(byear, sex),
#              td_vars=c(education, income, cci, region, married),
#              exclude = c(skinc, imm_sup),
#              td_frame = ses_wide,
#              n_controls=4,
#              seed=1)


matchR <- function(data, td_frame, index, case, fu, td_date, fixed_vars, td_vars, exclude, n_controls=4, seed=1, cores=4, pnr=pnr) {

  tickR()

  cat(paste0("\nInitializing matchR algorithm: ", tockR("time"), "\n"))

  cluster <- parallel::makeCluster(cores)
  registerDoParallel(cluster)

  pnr_c <- deparse(substitute(pnr))
  index_c <- deparse(substitute(index))
  td_date_c <- deparse(substitute(td_date))
  cat_c <- unlist(str_extract_all(deparse(substitute(fixed_vars)), "\\w{2,}"))
  dat_c <- unlist(str_extract_all(deparse(substitute(td_vars)), "\\w{2,}"))
  exclude_c <- unlist(str_extract_all(deparse(substitute(exclude)), "\\w{2,}"))

  case_s <- substitute(case)
  fu_s <- substitute(fu)
  index_s <- substitute(index)
  td_date_s <- substitute(td_date)

  setDT(data)
  setDT(td_frame)

  cat(paste0("\nData reduction: "))

  data <- data[case == 1 | (case == 0 & fu > min(index, na.rm=T)), env=list(index = substitute(index),
                                                                            case = substitute(case),
                                                                            fu = substitute(fu))] %>%
    .[.[, Reduce(`&`, lapply(.SD, function(x) is.na(x) | x > min(index_s, na.rm=T))),.SDcols = c(exclude_c)], .SDcols= c(exclude_c), env=list(index_s = index_s)]


  cat(paste0("completed - ", tockR("time"), "\n\n"))

  cat(paste0("Merging time-dependent data frame: "))


  full_df <-
    merge(data, td_frame, by = pnr_c)[order(-case, pnr, td_date), env=list(pnr = substitute(pnr),
                                                                           case = substitute(case),
                                                                           td_date = substitute(td_date))][
                                                                             td_date <= index | (case==0 & td_date <= max(index, na.rm=T)), env=list(index = substitute(index),
                                                                                                                                                     case = substitute(case),
                                                                                                                                                     td_date = substitute(td_date))][
                                                                                                                                                       , .SD[row.names(.SD) == .N | case==0], by = pnr_c, env=list(pnr=substitute(pnr),
                                                                                                                                                                                                                   case=substitute(case))][, set := match(pnr, unique(pnr)), env=list(pnr = substitute(pnr))]

  cat(paste0("\nMerging time-dependent data frame: completed - ",  tockR("time"), "\n\n"))

  #Partition into birth cohorts
  cohorts <- sort(unique(full_df[full_df[["case"]]==1,][["byear"]]))
  cohorts_length <- length(cohorts)

  cat(paste0("Partitioning ", cohorts_length, " cohorts (", cohorts[1], " to ", cohorts[cohorts_length], "): \n\n"))

  byear_list <- list()

  pb <- txtProgressBar(max = cohorts_length, style = 3)

  for(j in 1:cohorts_length) {

    b <- cohorts[[j]]

    df <- full_df[byear == b]

    cases <- df[case == 1, env=list(case=substitute(case))]

    control_list <- foreach(i = cases$set,
                            .packages = c("tidyverse", "data.table")) %dopar% {

                              itime <- df[set == i, index_s, env = list(index_s = index_s)]

                              controls <-
                                df[set == i | case_s == 0 & fu_s > itime & td_date_s < itime, env=list(case_s = case_s,
                                                                                                       fu_s = fu_s,
                                                                                                       td_date_s = td_date_s)] %>%
                                .[.[, Reduce(`&`, lapply(.SD, function(x) is.na(x) | x > itime)),.SDcols = c(exclude_c)], .SDcols= c(exclude_c)] %>%
                                .[, .SD[.N], by = pnr_c] %>%
                                .[.[, Reduce(`&`, lapply(.SD, function(x) is.na(x) & is.na(first(x)) | x == first(x))),.SDcols = c(cat_c, dat_c)], .SDcols= c(cat_c, dat_c)]

                            }


    control_list <- control_list[order(sapply(control_list, nrow))]

    idlist <- c()

    matches <- list()

    for(i in seq(1,length(control_list))) {

      set.seed(seed)



      m <-
        control_list[[i]][,set:=first(set)][case==0 & pnr %nin% idlist, env=list(case=substitute(case),
                                                                                 pnr=substitute(pnr))][sample(.N, pmin(.N, n_controls))]

      idlist <- c(idlist, m[, c(pnr), env=list(pnr = substitute(pnr))])

      matches[[i]] <- m

    }

    setTxtProgressBar(pb, j)

    byear_list[[as.character(b)]] <- as.data.frame(bind_rows(cases, rbindlist(matches)))

  }

  close(pb)

  cat("\nMatching complete!\n")
  cat("Total runtime: \n")
  cat(tockR("diff"), "\n\n")

  stopCluster(cluster)

  rbindlist(byear_list)[order(set)][, "index" := nafill(index, "locf"), env=list(index=substitute(index))][, c(exclude_c, td_date_c) := NULL] %>%
    select({{pnr}}, case, set, index, everything())



}
