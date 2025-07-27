#' tockR
#'
#' @description
#' tockR stops the clock and prints either a date/time or a time difference
#'
#'
#' @param format Whether date and time or a time difference should be returned
#' @param digits Number of digits on time difference
#'
#' @return Date/time or time difference since tickR()
#' @export
#'
#'

tockR <- function(format = "diff", digits = 1) {

    if(format == "time") {

    out <- lubridate::round_date(Sys.time(), "second")

    } else if(format == "diff") {

    t <- Sys.time() - tickR.start

    out <- paste0(round(as.numeric(t), digits), " ", attr(t, "units"))


    }


  out

}
