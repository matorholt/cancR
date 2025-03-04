#' CPRfix
#'
#' @description
#' Fixes CPR numbers with removed leading zeros. Works with piping
#'
#'
#' @param data dataset
#' @param cpr cpr-column
#'
#' @return Returns same dataset with correct CPR numbers
#' @export
#'
#'
cprfix <- function(data, cpr) {
  data %>%
    rowwise() %>%
    mutate({{cpr}} := ifelse(str_count({{cpr}}) == 9, paste0("0",{{cpr}}, collapse=""), {{cpr}}))
}
