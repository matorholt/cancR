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
#' @param fu End of follow up as date
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
# n=1000
# c=5
#
#
# pop <- simulatR("match",
#                n=n,
#                match.cases = c)
#
# set.seed(1)
# covariates <- simulatR("covariates",
#                        n=n+c)
# set.seed(1)
# covariates_long <- simulatR("covariates",
#                             format = "long",
#                             n=n+c)
#
#
# post_match <-
#   matchR(data=pop,
#          fu=follow,
#          td.vars = c(education, cci, cancer),
#          exclude = c(skinc, imm_sup),
#          exclude.length = 365.25*1,
#          td.frame = covariates_df,
#          n.controls=4,
#          seed=1)



matchR <- function(data,
                   td.frame,
                   index = index,
                   case = case,
                   fu = fu,
                   td.date = from,
                   fixed.vars = c(byear, sex),
                   td.vars,
                   exclude,
                   exclude.length = 365.25*100,
                   n.controls=4,
                   replace = T,
                   seed=1,
                   cores=4,
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

  case_c <- data %>% select({{case}}) %>% names()
  fu_c <- data %>% select({{fu}}) %>% names()
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

  #Change names
  namelist <- list(pnr = pnr_c,
                   case = case_c,
                   index = index_c,
                   fu = fu_c,
                   from = td.date_c)

  setnames(data,
           unlist(namelist[namelist != "from"]),
           names(namelist[namelist != "from"]))

  setnames(td.frame,
           unlist(namelist[c("pnr", "from")]),
           names(namelist[c("pnr", "from")]))


  data <- data[case == 1 | (case == 0 & fu > min(index, na.rm=T))] %>%
    .[.[, Reduce(`&`, lapply(.SD, function(x) is.na(x) | x > min(index, na.rm=T))),.SDcols = c(exclude_c)], .SDcols= c(exclude_c)]

  cat(paste0("completed - ", tockR("time"), "\n\n"))

  cat(paste0("Merging time-dependent data frame: "))

  all_indices <- data[case==1, substitute(index)]

  #TD covariates are merged with data. Only time windows that include an index from any of the cases are included to reduce data.
  full_df <- merge(data, td.frame[pnr %in% data[, pnr]], by = pnr_c)[order(-case, pnr, from)][
    from <= index | (case==0 & from <= max(index, na.rm=T))][
      index >= from & index < to | case == 0][
        sapply(seq_len(.N), function(i)
          any(all_indices >= from[i] & all_indices < to[i])
        )][
          , set := match(pnr, unique(pnr))]

  cat(paste0("\nMerging time-dependent data frame: completed - ",  tockR("time"), "\n\n"))

  cat(paste0("Partitioning of age-sex cohorts: "))

  total_cases <- data[case == 1,]

  #Partition into age-sex cohorts filtered on presence of cases
  split_df <- split(full_df, by = c("tvar", "sex"))[as.character(sort(unique(interaction(total_cases$byear, total_cases$sex))))]

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


  pb <- txtProgressBar(max = length(split_df), style = 3)
  progress <- function(n) {
    setTxtProgressBar(pb, n)
    cat(paste0("     Time: ", tockR("time"), ", runtime: ", tockR("diff"), pb_out[n]))
  }
  opts <- list(progress = progress)

  ##MATCHING##
  cohort_list <- foreach(j = seq_along(split_df),
                         .packages = c("tidyverse", "data.table", "foreach"),
                         .options.snow = opts) %dopar% {

                           df <- split_df[[j]]

                           cases <- df[get("case") == 1]

                           ncase <- ncase + nrow(cases)

                           control_list <- list()

                           for(c in seq_along(cases$set)) {

                             i <- cases$set[[c]]

                             #Index for current case
                             itime <- df[set == i, get("index")]

                             #Keep cases with:
                             # - index > current index (Cases cannot be selected as controls if already cases)
                             #Keep controls (with:
                             # - fu > current index (alive at index) & where the current index is contained between from and to

                             df_c <- df[set == i |
                                          (get("index") > itime |
                                             (get("case") == 0 & get("fu") > itime & (itime >= get("from") & itime < get("to")))),] %>%
                               #Exclude cases who were exposed within x-days before index (arg: exclude.length)
                               .[.[, Reduce(`&`, lapply(.SD, function(x) is.na(x) | !between((as.numeric(itime - x)), 0, exclude.length))) | set == i,.SDcols = c(exclude_c)], .SDcols= c(exclude_c)]

                             #Fast binary-search on matching parameters
                             filters <- as.list(df_c[set==i, .SD, .SDcols=vars_cc])

                             setkeyv(df_c, vars_cc)

                             control_list[[c]] <- df_c[filters] %>%

                               #Not-yet cases converted to controls. Censoring at time of switch to case and removing index-date
                               .[, c("fu") := if_else(get("case") == 1 & set != i, get("index"), get("fu"))] %>%
                               .[, c("index") := if_else(get("case") == 1 & set != i, as.Date(NA), get("index"))] %>%
                               .[, c("case") := ifelse(get("case") == 1 & set != i, 0, get("case"))]



                             #Reroute output
                             #control_list[[c]] <- df_c

                           }

                           control_list <- control_list[order(sapply(control_list, nrow))]

                           idlist <- c()

                           matches <- list()

                           #For each case, n controls are sampled
                           for(i in seq(1,length(control_list))) {

                             set.seed(seed)

                             if(replace) {

                               m <-
                                 control_list[[i]][order(-get("case"))][
                                   ,set:=first(set)][get("case") == 0][
                                     sample(.N, pmin(.N, n.controls))]

                              #Same as above, but a cumulative ID list excludes previous matches
                             } else {

                               m <-
                                 control_list[[i]][
                                   order(-get("case"))][
                                     ,set:=first(set)][
                                       get("case") == 0 & !(get("pnr") %in% idlist)][
                                         sample(.N, pmin(.N, n.controls))]

                               idlist <- c(idlist, m[, c("pnr")])


                             }

                             matches[[i]] <- m

                           }

                           as.data.frame(bind_rows(cases, rbindlist(matches)))

                           #   #Reroute output
                           #control_list
                           #   #matches

                           #
                           #
                         }

  close(pb)

  cat("\nMatching complete!\n")
  cat("Total runtime: \n")
  cat(tockR("diff", start), "\n\n")

  if(!is.null(cores)) {
    parallel::stopCluster(cluster)
  }

  #Sorting + controls inherit index date and exclusion variables are omitted.
  match.df <- rbindlist(cohort_list)[order(set, -case)][
    , index := nafill(index, "locf")][
      , c(exclude_c[exclude_c != "sc_date"], "from", "to", "tvar") := NULL]

  setcolorder(match.df, c("pnr", "case", "set", "index"))

  #Returning original names
  setnames(match.df,
           names(namelist[namelist != "from"]),
           unlist(namelist[namelist != "from"]))

  as.data.frame(match.df)

  #Reroute output
  #cohort_list
  #split_df

}
