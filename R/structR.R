#' Convert dates to status indicator and time-to-event
#'
#' @description
#' Converts event dates to status indicator and a time-to-event variable.
#' The status indicator will be assigned to:
#' 0 if alive and event-free,
#' 1 in the presence of the event of interest
#' 2 in the presence of the competing event, often death
#'
#' @param data Dataframe
#' @param index Index time point 'T=0' provided in date format
#' @param fu End of follow-up or time of death in date format
#' @param outcomes vector of single or multiple column names with the event of interest in date format
#' @param competing vector of single or multiple column names that should be considered competing risks in the specified order.
#' @param composite named list of composite outcomes with one list per outcome specifying "outcomes" and "competing" (optional).
#' @param pattern Indicates the name pattern of the outcomes such as index_event or event_date
#' @param unit Whether time-to-event should be reported in months or years.
#' @param keep_dates Whether the original event dates should be kept.
#' @param digits number of digits on event times
#' @param id name of the id column. If missing, autodetection is attempted
#'
#' @return For each outcome, a status indicator of 0/1/2 and a time-to-event column are returned. If competing is missing, the levels are 0/1
#' For the competing event, a status indicator of 0/1 and a time-to-event is returned.
#' For patients without an event the function returns the time to last follow-up if status = 0 and time to death if status = 2
#' @export
#'

# n=10
# df <-
#   data.frame(
#     id = seq(1,n),
#     opdate = c(rep("2000-01-01", n-1), "2030-01-01"),
#            follow = rep("2025-01-01", n),
#            recurrence_date = c(NA, "2005-01-01", NA, NA, NA, "2005-01-01", "2005-01-01", NA, "1995-01-01", "2005-01-01"),
#            metastasis_date = c(NA, NA, "2007-01-01", NA, NA, "2006-01-01", "2005-01-01", NA, NA, NA),
#            dsd_date = c(NA,NA, "2008-01-01", "2009-01-01", NA, NA, NA, NA, NA, NA),
#            death_date = c(NA, NA, "2008-01-01", "2009-01-01", NA, "2010-01-01", "2010-01-01", "2024-01-01", "2019-01-01", "1999-01-01"),
#            second_date = c(NA, NA, NA, NA, "2008-01-01", NA, "2001-01-01", NA, NA, NA)) %>%
#   datR(c(opdate:second_date))
#
# df <- df[c(1:(n-1)), ]
#
# df <- df[c(1:(n-2)), ]
#
# structR(df,
#         index = opdate,
#         fu = follow,
#         outcomes=c(recurrence_date, metastasis_date),
#         competing = c(death_date, second_date),
#         composite = list("pfs" = list("outcomes" = c("recurrence_date", "metastasis_date", "death_date")),
#                          "relapse" = list("outcomes" = c("recurrence_date", "metastasis_date", "dsd_date"),
#                                           "competing" = c("death_date")),
#                          "test" = list("outcomes" = c("metastasis_date"),
#                                        "competing" = c("recurrence_date", "death_date"))),
#         keep.dates = T)



structR <- function(data,
                    index,
                    fu,
                    outcomes,
                    competing,
                    composite = list(),
                    pattern = "_date",
                    unit = "months",
                    keep.dates=F,
                    digits = 2,
                    id){

  if("data.table" %in% class(data)) {

    return(cat("Error: dataframe is in the format data.table - convert to data frame using as.data.frame(data)"))

  }

  if(unit %nin% c("months", "years", "days")) {
    cat("Error: Invalid choice of unit. Choose between days, months or years")
  }

  if(missing(id)) {

    id_syn <- paste0("\\b", c("id", "ID", "pnr", "pt_id", "study_id", "record_id"), "\\b")

    if(sum(id_syn %in% colnames(data)) > 1) {
      return(cat("Multiple ID columns detected - pick only one"))
    }
    id_c <- data %>% select(matches(id_syn)) %>% names()
  } else {
    id_c <- data %>% select({{id}}) %>% names()
  }

  out_c <- data %>% select({{outcomes}}) %>% names()
  com_c <- data %>% select({{competing}}) %>% names()
  fu_c <- data %>% select({{fu}}) %>% names()
  index_c <- data %>% select({{index}}) %>% names()


  if(any(str_detect(c(out_c, com_c), pattern, negate=T))) {
    return(cat("Wrong naming pattern for outcome columns (e.g. index_ or _date)"))
  }

  if(any(str_detect(c(out_c, com_c), "index_")) &
     any(str_detect(c(out_c, com_c), "_date"))) {
    return(cat("Outcome variables are both named index_ and _date - they should be uniform"))
  }

  errors <- c()

  #Date check
  for(i in c(out_c, com_c, fu_c, index_c)) {

    if(sum(data[[i]] > Sys.Date(), na.rm=T) > 0) {

      errors <- c(errors, data[which(replace_na(data[["opdate"]], as.Date(0)) > Sys.Date()), id_c])
    }

  }

  if(length(errors) > 0) {

    cat(paste0("Error: Dates in the future found for ", length(errors), " individual(s). Vector of IDs returned"))

  return(errors)

  }


  if(unit == "months"){
    t = 30.4375
  }else if(unit == "years"){
    t = 365.25
  } else if(unit == "days"){
    t <- 1
  }

  out_names <-
    str_remove(out_c, pattern)
  out_time <-
    paste0("t_", out_names)

  #event + t_event
  for(v in seq_along(out_c)) {

    event_cols <- c(out_c[[v]], com_c, fu_c)

    #Event
    data[[out_names[[v]]]] <- ifelse((vec <- apply(data[, event_cols], 1, function(x) which(x == min(x, na.rm=T))[1])) == length(event_cols), 0, vec)

    #Time
    data[[out_time[[v]]]] <-round(as.numeric(as.Date(apply(data[, event_cols], 1, function(x) min(x, na.rm=T))) - data[, index_c])/t, digits)



  }

  #Death + t_death

  if(any(str_detect(event_cols, "death"))) {

    #Death
    data[["death"]] <- ifelse(!is.na(data[["death_date"]]), 1, 0)
    #Time
    data[["t_death"]] <- round(as.numeric(as.Date(apply(data[, c("death_date", fu_c)], 1, function(x) min(x, na.rm=T))) - data[, index_c])/t, digits)

  }

  #Custom composite outcomes

  for(c in seq_along(composite)) {

    outs <- data %>% select(composite[[c]][["outcomes"]]) %>% names()

    #Collapse competing risks to first date
    if("competing" %in% names(composite[[c]])) {

      comps <- data %>% select(composite[[c]][["competing"]]) %>% names()

      if(length(comps) == 1) {
        c_collapse <- data[,comps]

      } else {
        c_collapse <- suppressWarnings(apply(data[, comps], 1, function(x) min(x, na.rm=T)[1]))
      }
    } else {
      c_collapse <- NULL
    }

    #Collapse outcomes to first date
    if(length(outs) == 1) {
      o_collapse <- data[,outs]
    } else{
    o_collapse <- suppressWarnings(apply(data[, outs], 1, function(x) min(x, na.rm=T)[1]))

    }

    #New frame with collapsed outcomes, competing risks and follow_up
    frame <- bind_cols("outs" = o_collapse, "comps" = c_collapse, "follow" = data[, fu_c])

    #Event
    data[[names(composite)[c]]] <- ifelse((vec <- apply(frame, 1, function(x) which(x == min(x, na.rm=T))[1])) == length(names(frame)), 0, vec)
    #Time
    data[[paste0("t_", names(composite)[c])]] <- round(as.numeric(as.Date(apply(frame, 1, function(x) min(x, na.rm=T))) - data[, index_c])/t, digits)

  }

  check_names <- data %>% select(matches("\\bt_")) %>%
    names()

  #If any event times are negative, zero or NA
  if(any(min(data %>% select(matches(check_names)) %>% unlist, na.rm=T) <= 0 | sum(is.na(data %>% select(matches(check_names)) %>% unlist))>0)) {


    checklist <- list()

    for(i in check_names) {

      checklist[[i]] <- data[which(data[[i]] <= 0 | is.na(data[[i]])), c(i, "id")]

    }

    checklist

    frame <- plyr::join_all(checklist, by = "id", type = "full")

    cat(paste0("Error: ", nrow(frame), " ID(s) with blank, negative or zero times to event. Dataframe with errors returned:\n\n"))

    return(frame %>% select(!!sym(id_c), everything()))

  }



  if(!keep.dates) {

    data <- data[, colnames(data) %nin% c(out_c, com_c, unlist(composite))]
  }

  return(data)
}

