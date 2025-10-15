#' First timestamp for taking time
#'
#' @description
#' tickR starts the clock by adding the timestamp "start" to global environment
#'
#'
#' @return A timestamp
#' @export
#'
#'

tickR <- function() {

  tickR.start <<- Sys.time()

  paste0(lubridate::round_date(Sys.time(), "second"), "\n")

}

