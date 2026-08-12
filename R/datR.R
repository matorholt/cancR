#' @title Convert dates from character to date format
#' @description
#' Convert dates easily without specifying format. The format is identified automatically and converted to standard Year-month-day.
#'
#' @param data data frame or vector of dates
#' @param vars character vector for specifying variables to convert to date format. Default is all columns containing "date|dato"
#' @param HMS whether hours, minutes and seconds should be kept (default = F)
#' @param dt whether a data.table should be returned
#'
#' @returns the input data frame with correctly formatted date variables
#' @export
#'
#' @examples
#'
# redcap_df %>%
# datR(c(birth, date_of_surgery, followup, death_date)) %>%
#  str
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

datR <- function(data, vars = contains(c("date", "dato")), HMS = F, dt = FALSE) {

  # Return DT if input is DT and dt is not specified
  if (is.data.table(data) && missing(dt)) dt <- TRUE

  get_date <- function(x) {

    if(is.numeric(x)) return(as.Date(x, origin = "1970-01-01"))

    # Sample format from first non-NA value
    sample <- x[!is.na(x)][1L]

    form <- case_when(str_detect(sample,
                                 "\\b\\d{2}-\\d{2}-\\d{4}\\b") ~ "%d-%m-%Y",
                      T ~ "%Y-%m-%d")

    if(HMS) as.POSIXct(x, format = paste0(form, " %H:%M")) else as.Date(x, format = form)
  }

  if (is.character(data) || is.numeric(data) || is.Date(data)) return(get_date(data))

  vars_c <- defusR(vars)

  setDT(data)
  data[, (vars_c) := lapply(.SD, get_date), .SDcols = vars_c]

  if (dt) data else as.data.frame(data)
}
