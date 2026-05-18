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
#' @param check whether data check should be performed (default = T). Checks if dates are in the future and event times are zero or negative.
#' @param remove whether dates before index should be removed (e.g. beyond look back period), default = F
#'
#' @return For each outcome, a status indicator of 0/1/2 and a time-to-event column are returned. If competing is missing, the levels are 0/1
#' For the competing event, a status indicator of 0/1 and a time-to-event is returned.
#' For patients without an event the function returns the time to last follow-up if status = 0 and time to death if status = 2
#' @export
#'
#' @examples
#' n=10
#' df <-
#'  data.frame(
#'    id = seq(1,n),
#'    opdate = c(rep("2000-01-01", n-1), "1990-01-01"),
#'    follow = rep("2025-01-01", n),
#'    recurrence_date = c(NA, "2005-01-01", NA, NA, NA, "2005-01-01", "2005-01-01", NA, "2005-01-01", "2005-01-01"),
#'    metastasis_date = c(NA, NA, "2007-01-01", NA, NA, "2006-01-01", "2005-01-01", NA, NA, NA),
#'    dsd_date = c(NA,NA, "2008-01-01", "2009-01-01", NA, NA, NA, NA, NA, NA),
#'    death_date = c(NA, NA, "2008-01-01", "2009-01-01", NA, "2010-01-01", "2010-01-01", "2024-01-01", "2019-01-01", "1999-01-01"),
#'    second_date = c(NA, NA, NA, NA, "2008-01-01", NA, "2001-01-01", NA, NA, NA)) %>%
#'  datR(c(opdate:second_date))
#'
#
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
# t <- structR(df,
#              index = opdate,
#              fu = follow,
#              outcomes=c(recurrence_date, metastasis_date),
#              competing = c(death_date, second_date),
#              composite = list("pfs" = list("outcomes" = c("recurrence_date", "metastasis_date", "death_date")),
#                               "relapse" = list("outcomes" = c("recurrence_date", "metastasis_date", "dsd_date"),
#                                                "competing" = c("death_date")),
#                               "test" = list("outcomes" = c("metastasis_date"),
#                                             "competing" = c("recurrence_date", "death_date"))),
#              keep.dates = T,
#              check = T,
#              remove = T)




structR <- function(data,
                    index = "index",
                    fu = "fu",
                    outcomes,
                    competing,
                    composite = list(),
                    pattern = "_date",
                    unit = "months",
                    keep.dates=F,
                    digits = 2,
                    id,
                    check = F,
                    remove = F,
                    dt = F) {

  #Convert to data.frame
  if(any(c("data.table", "tbl") %in% class(data))) {
    data <- as.data.frame(data)
  }

  if(unit %nin% c("months", "years", "days")) {
    cli::cli_alert_danger("Error: Invalid choice of unit. Choose between days, months or years")
  }

  if(missing(id)) {

    id_syn <- paste0("\\b", c("id", "ID", "pnr", "pt_id", "study_id", "record_id"), "\\b")

    if(sum(id_syn %in% colnames(data)) > 1) {
      return(cli::cli_alert_danger("Multiple ID columns detected - pick only one"))
    }
    id_c <- data %>% select(matches(id_syn)) %>% names()
  } else {
    id_c <- data %>% select({{id}}) %>% names()
  }


  out_c <- data %>% select({{outcomes}}) %>% names()
  com_c <- data %>% select({{competing}}) %>% names()
  fu_c <- data %>% select({{fu}}) %>% names()
  index_c <- data %>% select({{index}}) %>% names()
  all_out <- c(out_c, com_c, names(composite))

  if(!all(str_detect(c(out_c, com_c), pattern))) {
    return(cli::cli_alert_danger("Wrong name pattern. All outcomes should include the pattern: {pattern}"))
  }

  setDT(data)

  if(check) {

    errors <- c()

    #Date check
    for(i in c(out_c, com_c, fu_c, index_c)) {

      if(sum(data[[i]] > Sys.Date(), na.rm=T) > 0) {

        errors <- c(errors, data[which(replace_na(data[["opdate"]], as.Date(0)) > Sys.Date()), get(id_c)])
      }

    }

    if(length(errors) > 0) {

      cli::cli_alert_danger("Error: Dates in the future found for {length(errors)} individual(s). Vector of IDs returned")

      return(errors)

    }

  }

  if(remove) {
    for(i in out_c) {

      data <- data[,substitute(i) := if_else(get(i) <= get(index_c), as.Date(NA), as.Date(get(i)))]


    }
  }

  if(unit == "months"){
    t = 30.4375
  }else if(unit == "years"){
    t = 365.25
  } else if(unit == "days"){
    t <- 1
  }

  event_map <- map(all_out, function(v) {

    map_list <- list()

    if(v %in% c(out_c, com_c)) {

      map_list[["name"]] <- str_remove(v, pattern)
      map_list[["time"]] <- paste0("t_", map_list[["name"]])

    } else {
      map_list[["name"]] <- v
      map_list[["time"]] <- paste0("t_", v)
    }

    if(v %nin% com_c) {
      map_list[["event_cols"]] <- c(v, com_c)
    } else {
      map_list[["event_cols"]] <- c(v)
    }

    if(v %in% names(composite)) {

      map_list[["event_cols"]] <- c(v, composite[[v]]$competing)

    }

    map_list

  }) %>% set_names(all_out)

  if(length(composite) > 0) {

    #Find minima in composite columns
    for(v in names(composite)) {

      data <- rowR(data,
                   composite[[v]]$outcomes,
                   type = "pmin",
                   label = v,
                   dt=T)
      }
  }

  #EVENT + TIME STRUCTURING
  for(v in seq_along(event_map)) {

    evs <- event_map[[v]][["event_cols"]]

    #Time
    data[, (event_map[[v]][["time"]]) := {
      round(as.numeric(pmin(do.call(pmin, c(.SD, list(na.rm=T))), get(fu_c), na.rm=T) - get(index_c))/t, digits)

    }, .SDcols= evs]

    #Event
    data[, (event_map[[v]][["name"]]) := {
      apply(.SD, 1, function(x) {
        suppressWarnings(pmax(0, which(x == min(x, na.rm=T))[1], na.rm=T))
      })
    }, .SDcols = evs]
  }

  if(check) {

    check_names <- map_chr(event_map, ~ .x$time)

    #If any event times are negative, zero or NA
    if(any(min(data %>% select(matches(check_names)) %>% unlist, na.rm=T) <= 0 | sum(is.na(data %>% select(matches(check_names)) %>% unlist))>0)) {


      checklist <- list()

      for(i in check_names) {

        checklist[[i]] <- data[which(data[[i]] <= 0 | is.na(data[[i]])), c(i, "id")]

      }



      frame <- joinR(checklist, by = "id", type = "full")

      cli::cli_alert_danger("Error: {nrow(frame)} ID(s) with blank, negative or zero times to event. Dataframe with errors returned:")

      return(frame %>% select(!!sym(id_c), everything()))

    }

  }

  if(!keep.dates) {

    data <- data[, unique(c(out_c, com_c, unlist(composite, use.names=F))) := NULL]
  }

  if(dt) return(data) else return(as.data.frame(data))
}
