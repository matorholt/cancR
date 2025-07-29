#' cutR
#'
#' @description
#' Wrapper for cut() with simplified labels and automatic conversion to factor based on most frequent levels
#'
#' @param data dataframe. Works with piping.
#' @param vars Vector of vars which should be cut
#' @param seqlist List of the split patterns for each var. E.g. list("v1" = seq(0,10), "v2" = round(quantile(df %>% pull(age), seq(0,1,0.1)),0))
#' @param names Whether the split should overwrite or as a new variable
#'
#' @return The inputted dataframe with the cut variables
#' @export
#'
# Examples
# set.seed(1)
#
# n=50
#
# df <-
#   data.frame(birth = sample(c(sample(seq(as.Date('1990/01/01'), as.Date('2010/01/01'), by="day"))), size = n, replace=TRUE),
#              age = sample(seq(50,80), size = n, replace=TRUE))
#
#Simple
# df %>%
#   cutR(age, round(quantile(df %>% pull(age), seq(0,1,0.1)),0), "age2")
#
#
# #Multiple
# (tdf <-
#     df %>%
#     cutR(vars = c(birth, age),
#          names = list("birth" = "period",
#                       "age" = "age_group"),
#          seqlist = list("birth" = seq(1990,2020, 10),
#                         "age" = c(0,seq(60,100,10)))))


cutR <- function(data, vars, seqlist, names = list()) {

  vars_c <-
    data %>% select({{vars}}) %>% names()

  if(class(seqlist) == "numeric") {
    seqlist <- list(seqlist)
    names(seqlist) <- vars_c
  }

  if(class(names) != "list") {
    names <- list(names)
    names(names) <- vars_c
  }

  #Default newvars
    default_names <- as.list(vars_c)
    names(default_names) <- vars_c

  newvars <- modifyList(default_names, names)

  for(v in vars_c) {

    #Extract year from dates
    if(class(data %>% pull(v)) == "Date") {
      data <- data %>%
        mutate(!!sym(newvars[[v]]) := as.numeric(str_extract(!!sym(v), "\\d{4}")))
    } else if(class(data %>% pull(v)) != "numeric") {
      data <- data %>%
        mutate(!!sym(newvars[[v]]) := as.numeric(!!sym(v)))
    }
    #Copy frame
    else {
      data <- data %>%
        mutate(!!sym(newvars[[v]]) := !!sym(v))
    }

    data <- data %>%
      mutate(!!sym(newvars[[v]]) := str_replace(cut(!!sym(newvars[[v]]),
                                                    breaks = seqlist[[v]],
                                                    include.lowest = TRUE,
                                                    right = F,
                                                    dig.lab=4),
                                                ".(\\d+).(\\d+).",
                                                "\\1-\\2")) %>%
      factR(!!sym(newvars[[v]]))



  }

  data
}
