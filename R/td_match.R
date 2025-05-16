#' td_match
#'
#' @description
#' Function for time dependent exact matching
#'
#'
#' @param data Dataset containing case/control indicator with index date for cases and fixed matching parameters
#' @param tdframe Dataset with time dependent matching covariates. Should be multiple rows for each patient with updated values for each parameter.
#' @param index Index date for cases, NA for controls
#' @param case 1/0 indicator for case/control status
#' @param fu End of follow up as date
#' @param tddate Name of the date column in the time dependent matching covariate dataset
#' @param cat Vector of fixed matching parameters
#' @param dat Vector of time dependent matching parameters
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

# library(cancR)
# library(foreach)
# library(doParallel)
#
#
# no = 40000
# cno= 0.00025*no
#
#
# set.seed(2)
# (c <- data.frame(id = paste("pnr", rnorm(cno, 40000,1000)),
#                  case = 1,
#                  index_cll = sample(c(sample(seq(as.Date('1990/01/01'), as.Date('2010/01/01'), by="day"))), size = cno, replace=TRUE),
#                  follow = c(sample(seq(as.Date('2015/01/01'), as.Date('2020/01/01'), by="day"), cno, replace=T)),
#                  byear = sample(c(seq(1950,1960)), cno, replace=T),
#                  sex = sample(c("F","M"), cno, replace=T),
#                  skinc = sample(c(sample(seq(as.Date('2011/01/01'), as.Date('2020/01/01'), by="day")),rep(as.Date(NA),8000)), size = cno, replace=TRUE),
#                  imm_sup = sample(c(sample(seq(as.Date('2011/01/01'), as.Date('2020/01/01'), by="day")),rep(as.Date(NA),8000)), size = cno, replace=TRUE),
#                  random1 = 1,
#                  random2 = 2) %>%
#     mutate(across(c(skinc, imm_sup), ~ if_else(. > follow | .< index_cll, follow-100, .))))
#
# (cnt <- data.frame(id = paste("pnr", rnorm(no, 40000,1000)),
#                    case = 0,
#                    follow = c(sample(seq(as.Date('1980/01/01'), as.Date('2020/01/01'), by="day"), no, replace=T)),
#                    byear = sample(c(seq(1945,1965)), no, replace=T),
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
#
# start <- Sys.time()
# (t1 <- td_match(data=pop,
#                         case=case,
#                         pnr = id,
#                         fu=follow,
#                         index=index_cll,
#                         tddate=date,
#                         cat=c(byear, sex),
#                         dat=c(education, income, cci, region, married),
#                         exclude = c(skinc, imm_sup),
#                         tdframe = ses_wide,
#                         n_controls=4,
#                         seed=1))
# Sys.time() - start

td_match <- function(data, tdframe, index, case, fu, tddate, cat, dat, exclude, n_controls=4, seed=1, cores=4, pnr=pnr) {

  deparse(substitute(cat))

  cluster <- makeCluster(cores)
  registerDoParallel(cluster)

  pnr_c <- deparse(substitute(pnr))
  cat_c <- unlist(str_extract_all(deparse(substitute(cat)), "\\w{2,}"))
  dat_c <- unlist(str_extract_all(deparse(substitute(dat)), "\\w{2,}"))
  exclude_c <- unlist(str_extract_all(deparse(substitute(exclude)), "\\w{2,}"))

  case_s <- substitute(case)
  fu_s <- substitute(fu)
  index_s <- substitute(index)
  tddate_s <- substitute(tddate)

  setDT(data)
  setDT(tdframe)


  data <- data[case == 1 | (case == 0 & fu > min(index, na.rm=T)), env=list(index = substitute(index),
                                                                            case = substitute(case),
                                                                            fu = substitute(fu))] %>%
    .[.[, Reduce(`&`, lapply(.SD, function(x) is.na(x) | x > min(index_s, na.rm=T))),.SDcols = c(exclude_c)], .SDcols= c(exclude_c), env=list(index_s = index_s)]



  df <-
    merge(data, tdframe, by = pnr_c)[order(-case, pnr, tddate), env=list(pnr = substitute(pnr),
                                                                         case = substitute(case),
                                                                         tddate = substitute(tddate))][
                                                                           tddate <= index | (case==0 & tddate <= max(index, na.rm=T)), env=list(index = substitute(index),
                                                                                                                                                 case = substitute(case),
                                                                                                                                                 tddate = substitute(tddate))][
                                                                                                                                                   , .SD[row.names(.SD) == .N | case==0], by = pnr_c, env=list(pnr=substitute(pnr),
                                                                                                                                                                                                               case=substitute(case))][, set := match(pnr, unique(pnr)), env=list(pnr = substitute(pnr))]

  cases <- df[case == 1, env=list(case=substitute(case))]



  control_list <- foreach(i = seq(1,nrow(cases)), .packages = c("tidyverse", "data.table")) %dopar% {

    controls <-
      df[set == i | case_s == 0 & fu_s > first(index_s) & tddate_s < first(index_s), env=list(case_s = case_s,
                                                                                              fu_s = fu_s,
                                                                                              index_s = index_s,
                                                                                              tddate_s = tddate_s)] %>%
      .[.[, Reduce(`&`, lapply(.SD, function(x) is.na(x) | x > first(index_s))),.SDcols = c(exclude_c)], .SDcols= c(exclude_c), env=list(index_s = index_s)] %>%
      .[, .SD[.N], by = pnr_c] %>%
      .[.[, Reduce(`&`, lapply(.SD, function(x) is.na(x) & is.na(first(x)) | x == first(x))),.SDcols = c(cat_c, dat_c)], .SDcols= c(cat_c, dat_c)]

  }

  control_list <- control_list[order(sapply(control_list, nrow))]

  idlist <- c()

  matches <- list()

  for(i in seq(1,length(control_list))) {

    set.seed(seed)

    m <-
      control_list[[i]][,set:=first(set)][case==0 & !(pnr %in% idlist), env=list(case=substitute(case),
                                                                       pnr=substitute(pnr))][sample(.N, pmin(.N, n_controls))]

    idlist <- c(idlist, m[, pnr, env=list(pnr = substitute(pnr))])

    matches[[i]] <- m

  }



  stopCluster(cluster)
  return(as.data.frame(bind_rows(cases, rbindlist(matches))[order(set)][, c("index") := nafill(index, "locf"), env=list(index=substitute(index))]))

}

