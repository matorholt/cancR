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
#' @return prints the variables with positivity violations if present. Otherwise none detected. Also returns as either NULL or character for downstream use.
#' @export
#'
#'
# n=20
# set.seed(1)
# df <- data.frame(id=seq(1:n),
#                  group=sample(c("pre", "sub"), n, replace=T),
#                  sex=sample(c("M","F"), n, replace=T),
#                  age_group=sample(c("<50",">50"),n,replace=T),
#                  chemo = sample(c("yes","no"), n, replace=T),
#                  age = rnorm(n,60,10),
#                  hospital = sample(c("rh","herlev","roskilde"), n, replace=T)) %>%
#   mutate(hospital = ifelse(group %in% "sub", "roskilde", hospital),
#          chemo = ifelse(group %in% "pre", "yes", chemo),
#          age_group = ifelse(group %in% "sub", "<50", age_group))
#
#
#
# t <- positivity(df, group, sex)
# t2 <- positivity(df, group, c(sex, hospital))
#
# is.character(t)
# is.null(t2)

positivity <- function(data, treatment, vars, id = id) {

  na_check <- data %>%
    select({{id}}, {{treatment}}, {{vars}}) %>%
    filter(if_any(c({{treatment}}, {{vars}}), is.na))

  if(nrow(na_check) > 0) {
    id_list <- na_check %>% select({{id}}, all_of((names(na_check)[sapply(na_check, anyNA)]))) %>%
             pivot_longer(cols = all_of((names(na_check)[sapply(na_check, anyNA)])), names_to = "na_vars", values_to = "nas") %>%
             filter(is.na(nas))


    return(cat(paste(c("NAs detected in the following variables:",
                       (names(na_check)[sapply(na_check, anyNA)]),
                       "In the following IDs:\nID  variable",
                       paste(t %>% pull(id) %>% str_pad(width = 3), t %>% pull(na_vars), sep = "   ")), collapse="\n")))
  }

  gr_char <- data %>% select({{treatment}}) %>% names()
  var_char <- data %>% select({{vars}}) %>% names()



  data <- data %>%
    mutate(across(where(is.numeric), ~ cut(.,breaks = quantile(., seq(0,1,0.1)))),
           across(c({{vars}}), ~ as.factor(.)))

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
 }
 if(length(zero) > 0) {
   cat(paste0(c("Positivity violations:", zero), sep="\n"))
   return(cat(" "))
 }
}

