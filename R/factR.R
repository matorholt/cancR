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
#' @param vars Vector of variables that should be factorized. The names from the lists "reference", "labels" and "levels" are automatically registered.
#' @param num.vars vector of variables with pseudonumeric ordering
#' @param reference List of variables with the reference level (e.g. list("v1" = "a"))
#' @param levels List of variables with the corresponding levels (e.g. list("v1" = "c("a","b","c","d","e")))
#' @param labels List of variables with the corresponding labels (e.g. list("v3" = c("e" = "epsilon", "d" = "delta")))
#' @param lab_to_lev Whether changing labels should change levels if these are not specified (defaults to TRUE)
#' @param na.level whether NAs should be added as a separate level named "missing" (default = F)
#' @param reverse Whether the levels should be reversed (default is FALSE)
#' @param auto.format whether no/yes and 0/1 should be autoformatted with no as reference
#' @param dt whether the data frame should be returned as data.table (default = F)
#'
#' @return Returns the inputted dataframe with modified factor variables
#' @export
#'

#
# set.seed(1)
#
# n=50
#
# (df <-
#     data.frame(v1 = sample(letters[1:5], size = n, replace=TRUE),
#                v2 = sample(letters[1:5], size = n, replace=TRUE),
#                v3 = sample(letters[1:5], size = n, replace=TRUE),
#                vnum = sample(c("<40", "50-60", "10-20", "100-110", ">110", "cci_0"), size = n, replace=TRUE),
#                comorb = rbinom(n, 1, 0.5)))

#Already a factor
# df %>%
#   factR(comorb) %>%
#   factR(num.vars = comorb)
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
#   factR(c(v1, v2, v3), labels = list("v3" = list("alpha" = "a")), lab_to_lev = F) %>%
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
    # df %>%
    # factR(vars = v1, levels = c("b", "e")) %>%
    #   str


#
# # Sort pseudo numeric character variable
# df %>%
#   factR(
#     num.vars=vnum,
#     labels = list("vnum" = c("test" = "cci_0")))

factR <- function(data,
                  vars,
                  num.vars = NULL,
                  reference = list(),
                  levels = list(),
                  labels = list(),
                  lab_to_lev = FALSE,
                  na.level = FALSE,
                  reverse = F,
                  auto.format = F,
                  dt = F) {

  #Return DT if input is DT and dt is not specified
  if(is.data.table(data) & missing(dt)) dt <- T

  num_c <- defusR(num.vars)

  vars_c <- defusR(vars)


  #Allow unnamed lists
  if(length(vars_c) == 1) {


    if(length(levels) > 0) {
      if(!is.null(names(levels))) {
        levels <- unlist(levels %>% set_names(NULL))

      }
      levels <- list(as.list(levels))
      names(levels) <- vars_c

    }

    if(length(labels) > 0) {

      if(all(names(labels) %nin% vars_c)) {

        labels <- list(as.list(labels)) %>% set_names(vars_c)

      }

    }
  }


  if(length(vars_c) == 1 & length(reference) == 1) {

    if(class(reference) == "character") {
      reference <- list(reference)

    }
    names(reference) <- vars_c
  }

  vars_c <- unique(c(vars_c, names(labels), names(reference), names(levels)))

  data <- copy(data)
  setDT(data)

  if(length(vars_c) > 0) {

    map(vars_c, function(v) {

      if(lab_to_lev && v %in% names(labels) && v %nin% num_c) {

        levels[[v]] <- labels[[v]]
      }

      if(auto.format & v %nin% names(labels) & length(na.omit(unique(data[[v]]))) == 2) {

        #No/Yes
        if(all(str_detect(na.omit(unique(data[[v]])), regex("no|yes", ignore_case=T)))) {

          levels[[v]] <- c("no", "No", "yes", "Yes")

        }

        if(all(str_detect(na.omit(unique(data[[v]])), regex("0|1", ignore_case=T))))

          labels[[v]] <- list("No" = "0",
                              "Yes" = "1")

      }

      if(is.factor(v)) {
        data <- data[, c(v) := fct_drop(get(v))]
      }

      if(v %in% num_c) {

        nlevs <- levels(data[[v]])

        mods <- as.numeric(str_extract(nlevs, "\\d+\\.?(\\d+)?"))+case_when(str_detect(nlevs, "\\<") ~ -1000000,
                                                                            str_detect(nlevs, "\\>") ~ 1000000,
                                                                            T ~ 0)

        val_list <- as.list(mods) %>% set_names(nlevs)
        val_list <- val_list[order(as.numeric(val_list))]

        levels[[v]] <- names(val_list)
        labs <- as.list(levels[[v]]) %>% set_names(as.list(levels[[v]]))

        labels[[v]] <- list_modify(listR(labs, type = "reverse"),
                                   !!!listR(labels[[v]], type = "reverse")) %>% listR(., type = "reverse")


      }

      suppressWarnings(data[, c(v) := fct_infreq(as.character(get(v)))][
        , c(v) := fct_relevel(get(v), reference[[v]])][
          , c(v) := fct_relevel(get(v), levels[[v]])])

      if(v %in% names(labels) & any(labels[[v]] %in% levels(data[[v]]))) {

        data[, c(v) := fct_recode(get(v), !!!setNames(labels[[v]], as.character(names(labels[[v]]))))]

      }



      if(reverse) {

        data[, c(v) := fct_rev(get(v))]

      }

      if(na.level && sum(is.na(data[[v]]) > 0)) {
        data[, c(v) := fct_na_value_to_level(get(v), level = "Missing")]
      }


    })

  }


  if(dt) return(data) else return(as.data.frame(data))

}
