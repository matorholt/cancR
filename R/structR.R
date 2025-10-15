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
#' @param outcomes Vector of single or multiple column names with the event of interest in date format
#' @param competing Single column indicating either status or dates of the competing event. If passed as a status (0/1) the dates are assumed to stem from follow-up (e.g. death)
#' @param pattern Indicates the name pattern of the outcomes such as index_event or event_date
#' @param unit Whether time-to-event should be reported in months or years.
#' @param keep_dates Whether the original event dates should be kept.
#'
#' @return For each outcome, a status indicator of 0/1/2 and a time-to-event column are returned. If competing is missing, the levels are 0/1
#' For the competing event, a status indicator of 0/1 and a time-to-event is returned.
#' For patients without an event the function returns the time to last follow-up if status = 0 and time to death if status = 2
#' @export
#'
# df <- data.frame(index = c(as.Date("1995-01-01"), as.Date("1995-05-02"), as.Date("1995-05-02"))) %>%
#   mutate(fu = c(as.Date("2010-05-02"), as.Date("2010-05-02"), as.Date("2010-05-02")),
#          death = c(0,1,1),
#          explant = c(0,1,1),
#          exp_date = c(as.Date("2005-05-02"), as.Date("2006-05-02"), as.Date("2006-05-02")),
#          hudc_date = index+500,
#          mm_date = c(as.Date("1996-01-01"), as.Date(NA), as.Date("2023-05-02")),
#          other_date = c(1,2,3))
#
# structR(df,
#         index,
#         fu,
#         outcomes=c(hudc_date, mm_date),
#         keep_dates = T)
# structR(df,
#         index,
#         fu,
#         outcomes=c(hudc_date, mm_date),
#         competing = exp_date,
#         unit = "days",
#         keep_dates = T)
#
# structR(df,
#         index,
#         fu,
#         outcomes=c(hudc_date, mm_date),
#         competing = explant,
#         unit = "days", keep_dates = T)

structR <- function(data,
                         index,
                         fu,
                         outcomes,
                         competing,
                         pattern = "_date",
                         unit = "months",
                         keep_dates=F){

  unit <- match.arg(unit, c("months", "years", "days"))

  if(any(str_detect(data %>% select({{outcomes}}) %>% colnames(), pattern, negate=T))) {
    return(cat("Wrong naming pattern for outcome columns (e.g. index_ or _date)"))
  }

  if(any(str_detect(data %>% select({{competing}}, {{outcomes}}) %>% colnames(), "index_")) &
     any(str_detect(data %>% select({{competing}}, {{outcomes}}) %>% colnames(), "_date"))) {
    return(cat("Outcome variables are both named index_ and _date - they should be uniform"))
  }

  if(unit == "months"){
    t = 30.4375
  }else if(unit == "years"){
    t = 365.25
  } else if(unit == "days"){
    t <- 1
  }




  if(missing(competing)) {

    d <- data %>%
      mutate(t_fu = as.numeric({{fu}} - {{index}})/t,
             across(c({{outcomes}}), ~ ifelse(!is.na(.) & . < {{fu}}, as.numeric(. - {{index}})/t, t_fu), .names=paste0(c("{str_remove({.col},'", pattern, "') %>% paste0('t_', .)}"), collapse="")),
             across(c({{outcomes}}), ~ ifelse(!is.na(.) & . < {{fu}}, 1, 0), .names=paste0("{str_remove_all({.col},'", pattern, "')}", collapse="")))

  } else{

    comp_name <- str_remove(data %>% select({{competing}}) %>% names(), pattern)

  if(data %>% pull({{competing}}) %>% class() %in% c("numeric", "character", "integer")) {

    d <- data %>%
      mutate(t_fu = as.numeric({{fu}} - {{index}})/t,
             !!(sym(paste0("t_", comp_name))) := t_fu,
             across(c({{outcomes}}), ~ifelse(!is.na(.) & . < {{fu}}, as.numeric(. - {{index}})/t, t_fu), .names=paste0(c("{str_remove({.col},'", pattern, "') %>% paste0('t_', .)}"), collapse="")),
             across(c({{outcomes}}), ~ifelse(!is.na(.) & . < {{fu}}, 1, ifelse(!!(sym(comp_name)) == 1, 2, 0)), .names=paste0("{str_remove_all({.col},'", pattern, "')}", collapse="")))

  } else {

    d <- data %>%
    mutate(t_fu = as.numeric({{fu}} - {{index}})/t,
           !!(sym(paste0("t_", comp_name))) := ifelse(!is.na({{competing}}), as.numeric({{competing}} - {{index}})/t, t_fu),
           across(c({{outcomes}}), ~ifelse(!is.na(.) & . < {{fu}}, as.numeric(. - {{index}})/t, t_fu), .names=paste0(c("{str_remove({.col},'", pattern, "') %>% paste0('t_', .)}"), collapse="")),
           !!(sym(comp_name)) := ifelse(!is.na({{competing}}), 1, 0),
           across(c({{outcomes}}), ~ifelse(!is.na(.) & . < {{fu}}, 1, ifelse(!!(sym(comp_name)) == 1, 2, 0)), .names=paste0("{str_remove_all({.col},'", pattern, "')}", collapse="")))

  }
  }

  if(!keep_dates & !missing(competing)){
    d <- d %>% select(-{{outcomes}}, -{{competing}})
  } else if(!keep_dates) {
    d <- d %>% select(-{{outcomes}})
  }



  return(d)
}





