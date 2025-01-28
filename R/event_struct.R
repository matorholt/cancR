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
#' @param namefix Vector of length 2. Indicates the name pattern of the event of interest.
#' @param unit Whether time-to-event should be reported in months or years.
#' @param keep_dates Whether the original event dates should be kept.
#'
#' @return To columns for each event of interest. A status indicator of 0/1/2 and a time-to-event column.
#' For patients without an event the function returns the time to last follow-up if status = 0 and time to death if status = 2
#' @export
#'
#'

event_struct <- function(data,
                         index,
                         fu,
                         death,
                         outcomes,
                         namefix=c("{str_remove_all({.col}, 'index_')}", "{str_replace_all({.col}, 'index_', 't_')}"),
                         unit = "months",
                         keep_dates=F){

  if(str_detect(unit, "m|M")){
    t = 30.4375
  }else if(str_detect(unit, "y|Y")){
    t = 365.25
  }

  d <- data %>%
    mutate(t_fu = as.numeric(fu - index)/t,
           across(c({{outcomes}}), ~ifelse(!is.na(.), 1, ifelse(death == 1, 2, 0)), .names=namefix[1]),
           across(c({{outcomes}}), ~ifelse(!is.na(.), as.numeric(. - index)/t, t_fu), .names=namefix[2]))

  if(keep_dates){
    d <- d
  }else {
    d <- d %>%select(-{{outcomes}})
  }

  return(d)
}
