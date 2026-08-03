#' @title Convert dates from character to date format
#' @description
#' Convert dates easily without specifying format. The format is identified automatically and converted to standard Year-month-day.
#'
#' @param data data frame or vector of dates
#' @param vars character vector for specifying variables to convert to date format. Default is all columns containing "date"
#' @param dt whether a data.table should be returned
#'
#' @returns the input data frame with correctly formatted date variables
#' @export
#'
#' @examples
#'
#' redcap_df %>%
#' datR(c(birth, date_of_surgery, followup, death_date)) %>%
#'  str
#'
#' datR(c("2001-02-01", "03-02-2002", 12345))
#'
#' datR(1234)

# redcap_df %>%
#   datR(c(birth, date_of_surgery, followup, death_date)) %>%
#   str
#
# datR(c("2001-02-01", "03-02-2002", 12345))
#
# datR(1234)

datR <- function(data, vars = contains("date"), dt=F) {

  #Return DT if input is DT and dt is not specified
  if(is.data.table(data) & missing(dt)) dt <- T

  #Function for detection and conversion
  get_date <- function(input) {

    map_vec(input, ~ {

      #If NA
      if(is.na(.x)) return(as.Date(NA))

      #If numeric
      if(is.numeric(.x)) return(as.Date(.x))

      #If pseudo-numeric (character)
      if(str_detect(.x, "\\-", negate=T)) return(as.Date(as.numeric(.x)))

      #If specific pattern
      form <- case_when(str_detect(.x, "\\b\\d{2}-\\d{2}-\\d{4}\\b") ~ "%d-%m-%Y",
                        T ~ "%Y-%m-%d")

      as.Date(.x, format = form)

    })

  }

  if(is.character(data) || is.numeric(data)) return(get_date(data))

  vars_c <- defusR(vars)

  setDT(data)

  data[, (vars_c) := map(.SD, ~ get_date(.x)), .SDcols = vars_c]

  if(dt) return(data) else return(as.data.frame(data))
}
