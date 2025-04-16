#' Positivity
#'
#' @description
#' Detection of positivity violations (empty levels)
#'
#'
#' @param data data frame to detect positivity violations
#' @param treatment the main stratum which all covariates should include all levels of
#' @param vars the covariates to examine for positivity violations. State multiple variables as c(var1, var2, var3) without quotes.
#'
#' @return prints the variables with positivity violations if present. Otherwise none detected. Also returns a TRUE or FALSE for downstream use.
#' @export
#'
#'
# n=20
# set.seed(1)
# df <- data.frame(pnr=seq(1:n),
#                  group=sample(c("pre", "sub"), n, replace=T),
#                  sex=sample(c("M","F"), n, replace=T),
#                  age_group=sample(c("<50",">50"),n,replace=T),
#                  chemo = sample(c("yes","no"), n, replace=T),
#                  hospital = sample(c("rh","herlev","roskilde"), n, replace=T)) %>%
#   mutate(hospital = ifelse(group %in% "sub", "roskilde", hospital),
#          chemo = ifelse(group %in% "pre", "yes", chemo),
#          age_group = ifelse(group %in% "sub", "<50", age_group))
#
# positivity(df, group, c(hospital, chemo, age_group))
# positivity(df, group, c(sex, hospital))

positivity <- function(data, treatment, vars) {

  gr_char <- data %>% select({{treatment}}) %>% names()
  var_char <- data %>% select({{vars}}) %>% names()

  data <- data %>% mutate(across(c({{vars}}), ~ as.factor(.)))

 zero <- unlist(lapply(var_char, function(x) {
   data %>%
     group_by(!!sym(gr_char), !!sym(x), .drop = FALSE) %>%
     count() %>%
     filter(n == 0) %>%
     mutate(check = str_c(str_c(gr_char, ": ", !!sym(gr_char), sep=""), str_c(x, ": ", !!sym(x), sep =""), sep="  -  ")) %>%
     pull(check)
 }))

 if(length(zero) == 0) {
   cat("No positivity violations detected")
   return(FALSE)
 } else {
   cat(paste0(c("Positivity violations:", zero), sep="\n"))
   return(TRUE)
 }
}
