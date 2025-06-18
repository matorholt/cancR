#' factR
#'
#' @description
#' Convenience function for levelling, labelling and referencing multiple factors in one step. Works with piping.
#' The order of effects is:
#' 1) Set as factor (fct_infreq setting levels based on frequency, most common is reference)
#' 2) Set reference levels
#' 3) Set levels manually
#' 4) Set labels
#'
#' The list can be non-complete if no modifications should be passed to the remaining variables
#'
#'
#' @param data dataframe
#' @param vars Vector of variables that should be factorized
#' @param reference List of variables with the reference level (e.g. list("v1" = "a"))
#' @param levels List of variables with the corresponding levels (e.g. list("v1" = "c("a","b","c","d","e")))
#' @param labels List of variables with the corresponding labels (e.g. list("v3" = c("e" = "epsilon", "d" = "delta")))
#' @param lab_to_lev Whether changing labels should change levels if these are not specified (defaults to TRUE)
#'
#' @return Returns the inputted dataframe with modified factor variables
#' @export
#'
#'
#
# set.seed(1)
#
# n=20
#
# (df <-
#     data.frame(v1 = sample(letters[1:5], size = 20, replace=TRUE),
#                v2 = sample(letters[1:5], size = 20, replace=TRUE),
#                v3 = sample(letters[1:5], size = 20, replace=TRUE),
#                vnum = sample(c(0,1), size = n, replace=TRUE)))
#
# # #Lazy_coding
# (tdf1 <-
#     df %>%
#     factR(vars = c(v1, v2,v3)))
#
# str(tdf1)
#
# #Setting reference level(s)
# (tdf2 <-
#     df %>%
#     factR(vars = c(v1, v2,v3),
#           reference = list("v1" = "c")))
#
# str(tdf2)
#
# #Changing levels manually
# (tdf3 <-
#     df %>%
#     factR(vars = c(v1, v2,v3),
#           levels = list("v1" = c("e","d","c","b","a"),
#                         "v2" = c("a", "b","c","d","e"))))
#
# str(tdf3)
#
# #Change labels without changing levels
# (tdf4 <-
#   df %>%
#   factR(c(v1, v2, v3), labels = list("v3" = c("a" = "alpha")), lab_to_lev = F))
#
# str(tdf4)
#
# #Changing everything
# (tdf5 <-
#     df %>%
#     factR(vars = c(v1, v2,v3),
#           reference = list("v1" = "b",
#                            "v3" = "d"),
#           levels = list("v2" = c("a","b","c","d","e")),
#           labels = list("v3" = c("e" = "epsilon", "d" = "delta"))))
#
#
# str(tdf5)
#
# #Setting levels by labels
# (tdf6 <-
#     df %>%
#     factR(vars = c(v1, v2,v3),
#           labels = list("v3" = c("e" = "epsilon", "d" = "delta", "a" = "alfa", "b" = "beta", "c" = "charlie")), lab_to_lev=T))
#
# str(tdf6)

# #Single variable
# (tdf7 <-
#     df %>%
#     factR(vars = v1, levels = c("a", "b")))
#
# str(tdf7)


factR <- function(data, vars, reference = list(), levels = list(), labels = list(), lab_to_lev = FALSE) {

  vars_c <-
    data %>% select({{vars}}) %>% names()

  if(class(levels) == "character") {
    levels <- list(levels)
    names(levels) <- vars_c
  }

  if(class(labels) == "character") {
    labels <- list(labels)
    names(labels) <- vars_c
  }

  if(class(reference) == "character") {
    reference <- list(reference)
    names(reference) <- vars_c
  }

  for(v in vars_c) {

    if(length(levels) == 0 & lab_to_lev) {

      levels[[v]] <- names(labels[[v]])
    }

    data <- data %>%
      mutate(!!sym(v) := forcats::fct_infreq(as.character(!!sym(v))),
            !!sym(v) := forcats::fct_relevel(!!sym(v), reference[[v]]),
            !!sym(v) := forcats::fct_relevel(!!sym(v), levels[[v]]),
            !!sym(v) := forcats::fct_recode(!!sym(v), !!!setNames(as.character(names(labels[[v]])), labels[[v]])))

  }

data

}


