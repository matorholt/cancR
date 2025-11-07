#' Detection of positivity violations (empty levels)
#'
#'
#' @param data data frame to detect positivity violations
#' @param treatment the main stratum which all covariates should include all levels of (optional)
#' @param outcome the outcome variable which all covariates should include all levels of (optional)
#' @param vars the covariates to examine for positivity violations. State multiple variables as c(var1, var2, var3) without quotes.
#' @param levels the number of combinations of covariates. level=1 (default) corresponds to a 2x2 table, whereas level=2 corresponds to 2x2 tables stratified on e.g. treatment
#' @param quantiles quantiles for binning of continuous covariates
#' @param id column indicating unique patient identifier for returning specific NAs
#'
#' @return prints the variables with positivity violations if present. Otherwise none detected. Also returns as either NULL or character for downstream use.
#' @export
#'
#'
# n=200
# set.seed(1)
# df <- data.frame(id=seq(1:n),
#                  group=sample(c("pre", "sub"), n, replace=T),
#                  sex=factor(sample(c("M","F"), n, replace=T)),
#                  age_group=sample(c("<50",">50"),n,replace=T),
#                  chemo = sample(c("yes","no"), n, replace=T),
#                  age = sample(c(seq(50,60), 50), n, replace=TRUE),
#                  hospital = sample(c("rh","herlev","roskilde"), n, replace=T)) %>%
#   mutate(hospital = ifelse(group %in% "sub", "roskilde", hospital),
#          chemo = ifelse(group %in% "pre", "yes", chemo),
#          age_group = ifelse(group %in% "sub", "<50", age_group),
#          hospital = as.factor(hospital))
#
# #add random NA
# df <- apply (df, 2, function(x) {x[sample( c(1:n), floor(n/10))] <- NA; x} ) %>%
#   as_tibble()
#
# t <- checkR(df,
#             treatment=group,
#             vars=sex,
#             levels=1)
# t2 <- checkR(df, group, vars=c(sex, hospital), levels=2)
# t3 <- checkR(df, treatment=group, outcome = sex, vars=c(age_group, chemo, hospital), levels = 3)

checkR <- function(data, treatment, outcome, vars, levels=1, quantiles=0.1, id=id) {

  id_c <- data %>% select({{id}}) %>% names()
  vars_c <- data %>% select({{vars}}) %>% names()
  treat_c <- data %>% select({{treatment}}) %>% names()
  out_c <- data %>% select({{outcome}}) %>% names()

  if(levels > length(c(vars_c))) {
    return(cat(paste0("ERROR: Levels exceeding number of variables. Levels can maximally be: ", length(c(vars_c)))))
  }

  na <- missR(data, vars = c(vars_c, treat_c, out_c), id =id, print=F)

  if(!is.null(na)) {
    cat("\nThe checkR algorithm only works on complete data. Remove NAs in the following variables:\n\n")
    print(na$counts)


    return(na)
  }


  n = levels

  data <- data %>%
    select({{treatment}}, {{vars}}, {{outcome}}) %>%
    mutate(across(where(is.numeric), ~ cut(.,breaks = unique(quantile(., seq(0,1,quantiles))))),
           across(c({{treatment}}, {{vars}}), ~ as.factor(.)))

  grid <- expand.grid(lapply(1:n, function(x) {
    vars_c
  })) %>%
    mutate(index = row_number()) %>%
    pivot_longer(cols=c(1:n), names_to = "position", values_to="var") %>%
    group_by(index) %>%
    distinct(var) %>%
    filter(n() == n) %>%
    arrange(index, var) %>%
    mutate(position = letters[row_number()+10]) %>%
    ungroup() %>%
    pivot_wider(names_from = position, values_from = var) %>%
    select(-index) %>%
    distinct() %>%
    mutate(across(everything(), ~ as.character(.)))

  if(!missing(treatment)) {
    grid$a_treat <- paste(substitute(treatment))
  }

  if(!missing(outcome)) {
    grid$b_outcome <- paste(substitute(outcome))
  }

  grid <- grid[, order(colnames(grid))]

  l <-
    lapply(c(1:nrow(grid)), function(i) {
      paste0(grid[i,])
    })

  zero <- rbindlist(lapply(l, function(x) {
    data %>% select(!!!syms(x)) %>%
      group_by(!!!syms(x), .drop = FALSE) %>%
      count() %>%
      ungroup() %>%
      filter(n == 0) %>%
      select(-n) %>%
      ungroup()
  }), fill=T)

  if(nrow(zero) == 0) {
    cat("No positivity violations detected")
  }
  if(nrow(zero) > 0) {
    cat("Positivity violations detected in the following combinations:\n")
    print(as_tibble(zero), n=300)
  }
}

