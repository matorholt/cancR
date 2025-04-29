#' Event structuring
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
#' @param death Status indicator for death, 0=Alive, 1=Dead
#' @param outcomes Vector of single or multiple column names with the event of interest in date format
#' @param pattern Indicates the name pattern of the outcomes such as index_event or event_date
#' @param unit Whether time-to-event should be reported in months or years.
#' @param keep_dates Whether the original event dates should be kept.
#'
#' @return To columns for each event of interest. A status indicator of 0/1/2 and a time-to-event column.
#' For patients without an event the function returns the time to last follow-up if status = 0 and time to death if status = 2
#' @export
#'
# df <- data.frame(index = c(as.Date("1995-01-01"), as.Date("1995-05-02"))) %>%
#   mutate(fu = index + 10000,
#          death_date = c(as.Date(NA), as.Date("2010-05-02")),
#          hudc_date = index+500,
#          other_date = c(1,2),
#          mm_date = c(as.Date("1996-01-01"), as.Date(NA)))
#
#
# event_struct(df, index, fu, outcomes=c(hudc_date, mm_date), competing = death_date)

event_struct <- function(data,
                         index,
                         fu,
                         outcomes,
                         competing,
                         pattern = "_date",
                         unit = "months",
                         keep_dates=F){

  unit <- match.arg(unit, c("months", "years"))

  if(any(str_detect(data %>% select({{outcomes}}) %>% colnames(), pattern, negate=T))) {
    stop("Wrong naming pattern for outcome columns (e.g. index_ or _date)")
  }

  if(unit == "months"){
    t = 30.4375
  }else if(unit == "years"){
    t = 365.25
  }

  t_pat <- ifelse(str_detect(pattern, "(?<=([:alpha:]))_"), "t_", "_t")

  if(missing(competing)) {

    d <- data %>%
      mutate(t_fu = as.numeric({{fu}} - {{index}})/t,
             across(c({{outcomes}}), ~ ifelse(!is.na(.), as.numeric(. - {{index}})/t, t_fu), .names=paste0(c("{str_replace_all({.col},'", pattern, "', '", t_pat, "')}"), collapse="")),
             across(c({{outcomes}}), ~ ifelse(!is.na(.), 1, 0), .names=paste0("{str_remove_all({.col},'", pattern, "')}", collapse="")))

  } else {
    comp_name <- str_remove(data %>% select({{competing}}) %>% names(), pattern)
    d <- data %>%
    mutate(t_fu = as.numeric({{fu}} - {{index}})/t,
           !!(sym(comp_name)) := ifelse(!is.na({{competing}}), 1, 0),
           across(c({{outcomes}}), ~ifelse(!is.na(.), as.numeric(. - {{index}})/t, t_fu), .names=paste0(c("{str_replace_all({.col},'", pattern, "', '", t_pat, "')}"), collapse="")),
           across(c({{outcomes}}), ~ifelse(!is.na(.), 1, ifelse(!!(sym(comp_name)) == 1, 2, 0)), .names=paste0("{str_remove_all({.col},'", pattern, "')}", collapse="")))

  }

  if(!keep_dates & !missing(competing)){
    d <- d %>% select(-{{outcomes}}, -{{competing}})
  } else if(!keep_dates) {
    d <- d %>% select(-{{outcomes}})
  }



  return(d)
}





