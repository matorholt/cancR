#' Extract first n groups in data frame
#'
#'
#' @param data dataframe
#' @param grps grouping variable
#' @param n number of groups to extract
#'
#' @return filtered dataset including only n first groups
#' @export
#'
#'
groupR <- function(data, grps, n) {
  data %>% group_by({{grps}}) %>%
    filter(cur_group_id() <= n)
}
