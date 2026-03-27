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
#' @param diminish Whether unused variables in td.frame should be removed (default = T)
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
#                match.cases = c)
#
# set.seed(1)
# covariates <- simulatR("covariates",
#                        n=n+c)
# set.seed(1)
# covariates_long <- simulatR("covariates",
#                             format = "long",
#                             n=n+c)

# post_match <-
#   matchR(data=pop,
#          fu=follow,
#          td.vars = c(income, education, cci, cancer),
#          exclude = c(skinc, imm_sup),
#          exclude.length = 365.25*1,
#          td.frame = covariates_df,
#          n.controls=4,
#          seed=1,
#          cores = 4)

#Fast
# post_match <-
#   matchR(data=match_df,
#          fu=follow,
#          td.vars = c(income, education, cci, cancer),
#          exclude = c(skinc, imm_sup),
#          exclude.length = 365.25*1,
#          td.frame = covariates_df,
#          n.controls=4,
#          seed=1,
#          cores = 4)

matchR <- function(data,
                   td.frame,
                   index = index,
                   case = case,
                   follow = fu,
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
                   interval = NULL,
                   diminish = T) {

  if("date" %in% names(data)) {
    return(cat("Error: The pre_match dataframe cannot contain a variable named date"))
  }

  tickR()

  start <- tickR.start

  cat(paste0("\nInitializing matchR algorithm: ", tockR("time"), "\n"))

  case <- data %>% select({{case}}) %>% names()
  fu <- data %>% select({{follow}}) %>% names()
  pnr <- data %>% select({{pnr}}) %>% names()
  index <- data %>% select({{index}}) %>% names()
  td.date <- td.frame %>% select({{td.date}}) %>% names()
  vars <- c(data %>% select({{fixed.vars}}) %>% names(), td.frame %>% select({{td.vars}}) %>% names())
  exclude <- data %>% select({{exclude}}) %>% names()

  if(!is.null(interval)) {
    data[["age_group"]] <- data[["tvar"]] <- cutR(data, byear, seq(1800,3000,interval))[["byear"]]
  } else {
    data[["tvar"]] <- data[["byear"]]
  }

  vars_c <- str_replace_all(vars, "byear", "tvar")

  setDT(data)
  setDT(td.frame)

  cat(paste0("\nData reduction: "))

  #Remove unused columns
  td.frame[, c(names(td.frame)[names(td.frame) %nin% c(vars, "pnr", "from", "to")]) := NULL]

  #Change names
  namelist <- list(pnr = pnr,
                   case = case,
                   index = index,
                   fu = fu,
                   from = td.date)

  setnames(data,
           unlist(namelist[namelist != "from"]),
           names(namelist[namelist != "from"]))

  setnames(td.frame,
           unlist(namelist[c("pnr", "from")]),
           names(namelist[c("pnr", "from")]))

  data <- data[case == 1 | (case == 0 & fu > min(index, na.rm=T))] %>%
    .[.[, Reduce(`&`, lapply(.SD, function(x) is.na(x) | x > min(index, na.rm=T))),.SDcols = c(exclude)], .SDcols= c(exclude)]

  cat(paste0("completed - ", tockR("time"), "\n\n"))

  cat(paste0("Merging time-dependent data frame: "))

  all_indices <- data[case==1, substitute(index)]

  #TD covariates are merged with data. Only time windows that include an index from any of the cases are included to reduce data.
  full_df <- merge(data, td.frame[pnr %in% data[, pnr]], by = pnr)[order(-case, pnr, from)][
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
  split_df <-
    split(full_df, by = c("tvar", "sex"))[as.character(sort(unique(interaction(total_cases$tvar, total_cases$ses))))]

  cat(paste0("\nPartitioning of age-sex cohorts: completed - ",  tockR("time"), "\n\n"))

  if(str_detect(names(split_df)[1], "-")) {
    cat(paste0("Partitioning ", length(split_df), " cohorts: \n"))
    cat(paste0(names(split_df), collapse="\n"))
    cat("\n\n")
  } else {
    cat(paste0("Partitioning ", length(split_df), " cohorts (", names(split_df)[1], " to ", names(split_df)[length(split_df)], "): \n\n"))
  }

  if(!is.null(cores)) {

    multitaskR(cores = cores)

  }

  progressr::handlers(global = TRUE)
  handlers(list(
    handler_progress(
      format   = "|:bar| :current/:total (:percent) - :message",
      width    = getOption("width"),
      complete = "=",
      clear = F
    )))

  p <- progressr::progressor(along = seq_along(split_df))

  ##MATCHING##
  cohort_list <- future_map(seq_along(split_df), function(j) {

    tickR()

                           df <- split_df[[j]]

                           cases <- df[get("case") == 1]

                           control_list <- list()

                           for(c in seq_along(cases$set)) {

                             i <- cases$set[[c]]

                             if(j > 1) {
                              #Case status
                             message(paste0("Cohort: ", names(split_df)[j], " - case: ", c, " of ", nrow(cases), " total (", round(c/nrow(cases)*100,1), ")%"))

                             }

                             #Index for current case
                             itime <- df[set == i, get("index")]

                             #Keep cases with:
                             # - index > current index (Cases cannot be selected as controls if already cases)
                             #Keep controls (with:
                             # - fu > current index (alive at index) & where the current index is contained between from and to

                             df_c <- df[set == i |
                                          (get("index") > itime |
                                             (case == 0 & fu > itime & (itime >= from & itime < to))),] %>%
                               #Exclude cases who were exposed within x-days before index (arg: exclude.length)
                               .[.[, Reduce(`&`, lapply(.SD, function(x) is.na(x) | !between((as.numeric(itime - x)), 0, exclude.length))) | set == i,.SDcols = c(exclude)], .SDcols= c(exclude)]

                             #Fast binary-search on matching parameters
                             filters <- as.list(df_c[set==i, .SD, .SDcols=vars_c])

                             setkeyv(df_c, vars_c)

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

  #close(pb)

  cat("\nMatching complete!\n")
  cat("Total runtime: \n")
  cat(tockR("diff", start), "\n\n")

  #Sorting + controls inherit index date and exclusion variables are omitted.
  match.df <- rbindlist(cohort_list)[order(set, -case)][
    , index := nafill(index, "locf")][
      , c(exclude[exclude != "sc_date"], "from", "to", "tvar") := NULL]

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
