#' Convert dates from character to date format
#'
#' @description
#' Convert dates easily without specifying format. The format is identified automatically and converted to standard Year-month-day.
#'
#'
#' @param data data frame
#' @param vars vector of character vars to convert to date format
#'
#' @returns the input data frame with correctly formatted date variables
#' @export
#'
#

# redcap_df %>%
#   datR(c(birth, date_of_surgery, followup, death_date)) %>%
#   str

datR <- function(data, vars) {

  vars_c <- data %>% select({{vars}}) %>% names()

  formats <- list()

  for(v in vars_c) {

    #Automatic formatting
    formats[[v]] <- case_when(str_detect(first(data %>% pull(!!sym(v)), na_rm=T), "\\b\\d{2}-\\d{2}-\\d{4}\\b") ~ "%d-%m-%Y",
                              T ~ "%Y-%m-%d")

    if(any(class(data[[v]]) %nin% "Date")) {
      data[[v]] <- as.Date(data[[v]], format = formats[[v]])

    }


   }

  data

}



