#' @title Perform rolling operations
#' @description
#' Useful for conditional grouping
#'
#'
#' @param data dataset
#' @param by vector of grouping column labels
#' @param order vector of variables to order the dataset
#' @param sort vector of the values c(1, -1) of length equal to the order argument
#' @param label label for new unique id column
#' @param type type of grouping operation, see details. Default = "roll"
#' @param dt whether a data.table should be returned
#' @param vars the variable used for computing lagged differences if type = "interval
#' @param interval vector of length 2 for evaluation of whether the lagged differences are within the two bounds
#' @param lag length of lag for lagged differences (default = 1)
#'
#' @details
#' Types of roll include:
#' - roll: Scans the by argument of unique values and assigns these ids
#' - count: simple row counter by group
#' - interval: groups based on whether the lagged difference is within the bounds of an interval (arguments vars, interval and)
#'
#'
#' @return adds a new column to the dataset with unique ids based on original id conditional on a sorting
#' @export
#'
#'
# df <- data.frame(id=c(1,1,1,1,2,2,2,3,3,3,4,4,4,4),
#            x=c(5,5,6,6,7,8,9,1,2,1,4,1,2,4))
#
# df %>%
#   arrange(x) %>%
#   rollR()
#
# df %>%
#   rollR(by = id, type = "count")
#
# df %>%
#   rollR(by=id, order = c(id, x), sort = c(1,1), type = "interval", interval = c(1,2), vars = x, lag = 1)


rollR <- function (data, by = NULL, order, sort = 1L, label = grp, type = "roll",
                   dt = F, vars, interval, lag = 1)
{
  if (is.data.table(data) & missing(dt))
    dt <- T
  by_c <- defusR(by)
  if (any(type %in% c("interval"))) {
    if (missing(interval))
      return({
        cli::cli_alert_danger("Error: interval not specified")
        invisible(NULL)
      })
    if (missing(vars))
      return({
        cli::cli_alert_danger("Error: interval vars not specified")
        invisible(NULL)
      })
    vars_c <- defusR(vars)
  }
  label_name <- defusR(label)
  dat <- as.data.table(data)
  if (!missing(order)) {
    order_c <- defusR(order)
    setorderv(dat, order_c, order = sort)
  }
  if (type == "roll") {
    dat[, `:=`((label_name), .GRP), by = by_c]
  }
  if (type == "count") {
    dat[, `:=`((label_name), seq_len(.N)), by = by_c]
  }
  if (type == "interval") {
    dat[, `:=`((label_name), {
      val <- get(vars_c)
      diff_val <- as.numeric(diff(val, lag = lag))
      diff_val <- c(rep(NA, length(val) - length(diff_val)),
                    diff_val)
      cumsum(ifelse(is.na(diff_val) | !(diff_val >= interval[1] &
                                          diff_val <= interval[2]), 0, 1))
    }), by = by_c]
  }
  if (dt)
    return(dat)
  else return(as.data.frame(dat))
}
