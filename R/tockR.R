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

tockR <- function(format = "diff", start, digits = 2) {

    if(format == "time") {

    out <- paste0(lubridate::round_date(Sys.time(), "second"))

    } else if(format == "diff") {

      if(!missing(start)) {

        t <- Sys.time() - start
      } else {

        t <- Sys.time() - tickR.start
      }



    out <- paste0(round(as.numeric(t), digits), " ", attr(t, "units"))


    }


  out

}
