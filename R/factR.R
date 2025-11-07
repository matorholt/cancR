#' Factorize variables
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
#' @param num_vars vector of variables with pseudonumeric ordering
#' @param reference List of variables with the reference level (e.g. list("v1" = "a"))
#' @param levels List of variables with the corresponding levels (e.g. list("v1" = "c("a","b","c","d","e")))
#' @param labels List of variables with the corresponding labels (e.g. list("v3" = c("e" = "epsilon", "d" = "delta")))
#' @param lab_to_lev Whether changing labels should change levels if these are not specified (defaults to TRUE)
#' @param reverse Whether the levels should be reversed (default is FALSE)
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
#                vnum = sample(c("<40", "50-60", "10-20", "100-110", ">110", "cci_0"), size = n, replace=TRUE)))
#
#
#
# # #Lazy_coding
# df %>%
#     factR(vars = c(v1, v2,v3)) %>%
#   str
#
# #Setting reference level(s)
# df %>%
#     factR(vars = c(v1, v2,v3),
#           reference = list("v1" = "c")) %>%
#   str
#
# #Changing levels manually
# df %>%
#     factR(vars = c(v1, v2,v3),
#           levels = list("v1" = c("e","d","c","b","a"),
#                         "v2" = c("a", "b","c","d","e"))) %>%
#   str
#
#
# #Change labels without changing levels
# df %>%
#   factR(c(v1, v2, v3), labels = list("v3" = c("alpha" = "a")), lab_to_lev = F) %>%
#   str
#
# #Changing everything
# df %>%
#     factR(vars = c(v1, v2,v3),
#           reference = list("v1" = "b",
#                            "v3" = "d"),
#           levels = list("v2" = c("a","b","c","d","e")),
#           labels = list("v3" = c("epsilon" = "e", "delta" = "d"))) %>%
#   str
#
# #Setting levels by labels
# df %>%
#     factR(vars = c("v1", "v2", "v3"),
#           labels = list(
#             "v1" = c("beta" = "b", "alfa" = "a"),
#             "v2" = c("charlie" = "c"),
#             "v3" = c("epsilon" = "e", "delta" = "d", "alfa" = "a", "beta" = "b", "charlie" = "c")), lab_to_lev=T) %>%
#   str
#
# #Single variable
#
#     df %>%
#     factR(vars = v1, levels = c("b", "e")) %>%
#       str
#
# # Sort pseudo numeric character variable
# df %>%
#     factR(num_vars=vnum,
#           labels = list("vnum" = c("test" = "cci_0"))) %>%
#   str

factR <- function(data, vars, num_vars, reference = list(), levels = list(), labels = list(), lab_to_lev = FALSE, reverse = F) {

  vars_c <-
    data %>% select({{vars}}) %>% names()
  num_c <-
    data %>% select({{num_vars}}) %>% names()


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

    if(lab_to_lev) {

      levels[[v]] <- labels[[v]]
    }

      data <- data %>%
        mutate(!!sym(v) := forcats::fct_infreq(as.character(!!sym(v))),
               !!sym(v) := forcats::fct_relevel(!!sym(v), reference[[v]]),
               !!sym(v) := forcats::fct_relevel(!!sym(v), levels[[v]]))

      if(v %in% names(labels)) {
        data <- data %>%
          mutate(!!sym(v) := forcats::fct_recode(!!sym(v), !!!setNames(labels[[v]], as.character(names(labels[[v]])))))

      }



    if(reverse) {
        data <- data %>% mutate(!!sym(v) := fct_rev(!!sym(v)))
    }


  }

    for(v in num_c) {

      n <- as.character(data[,v])

      names(n) <-
        lapply(n, function(x) {
          x2 <- as.numeric(paste0(unlist(str_extract_all(x, "\\d")), collapse=""))

          if(str_detect(x, "<")) {
            x2 <- -x2
          } else if(str_detect(x, ">|\\+")) {x2 <- x2+100000000}
          else {x2}

        })

      n <- unique(n[as.character(sort(as.numeric(names(n))))])

      data <- data %>%
      mutate(!!sym(v) := forcats::fct_relevel(!!sym(v), n),
             !!sym(v) := forcats::fct_recode(!!sym(v), !!!setNames(labels[[v]], as.character(names(labels[[v]])))))


    }



data

}
