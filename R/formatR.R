#' formatR
#'
#' @param data dataframe. Works with piping
#' @param cat_vars Vector of categorical covariates which should be formatted to factors with labels
#' @param num_vars Vector of numerical covariates that should be cut
#' @param case_labs Labels for the case/control column
#'
#' @return Returns the same dataframe with print-friendly labels for the typical full matching setup.
#' @export
#'
#'

# (df <- data.frame(case = c(0,0,0,1,1),
#                   birth = c(as.Date("1940-01-01"),
#                             as.Date("1930-01-01"),
#                             as.Date("1920-01-01"),
#                             as.Date("1910-01-01"),
#                             as.Date("1900-01-01")),
#                   index = c(as.Date("2007-01-01"),
#                             as.Date("2005-01-01"),
#                             as.Date("2020-01-01"),
#                             as.Date("2010-01-01"),
#                             as.Date("2015-01-01")),
#                   sex = c("f","f","f","m","m"),
#                   cci = c("cci_0",
#                           "cci_1",
#                           "cci_2-3",
#                           "cci_4-5",
#                           "cci_6+"),
#                   region = c("the_capital_region_of_denmark",
#                              "the_region_of_southern_denmark",
#                              "region_zealand",
#                              "central_denmark_region",
#                              "the_north_denmark_region"),
#                   education = c("low",
#                                 "low",
#                                 "medium",
#                                 "medium",
#                                 "high"),
#                   income = c("q1",
#                              "q2",
#                              "q3",
#                              "q4",
#                              "q4"),
#                   marital = c("married",
#                               "married",
#                               "unmarried",
#                               "divorced",
#                               "divorced")
# ))
#
#
# (tdf <-
#     df %>%
#     formatR(case_labs = c("No CLL", "CLL"),
#             seqlist = list("age" = c(0,70, seq(80,150, 10)),
#                            "index" = seq(1980,2030,10)),
#             names = list("index" = "Period")))



formatR <- function(data,
                    cat_vars = c(case, sex, cci, region, education, income, marital),
                    num_vars = c(age, index),
                    seqlist = list(),
                    names = list(),
                    case_labs) {



  seqlist_default <- list("age" = seq(0,150,10),
                          "index" = seq(1980,2030,5))
  names_default <- list("age" = "age_group",
                        "index" = "period")

  seqlist <- modifyList(seqlist_default, seqlist)
  names <- modifyList(names_default, names)


    data %>%
    factR(vars = {{cat_vars}}, labels = list("case" = c("0" = case_labs[1], "1" = case_labs[2]),
                                         "sex" = c("m" = "Male",
                                                   "f" = "Female"),
                                         "cci" = c("cci_0" = "0 Points",
                                                   "cci_1" = "1 Point",
                                                   "cci_2-3" = "2-3 Points",
                                                   "cci_4-5" = "4-5 Points",
                                                   "cci_6+" = "6+ Points"),
                                         "region" = c("the_capital_region_of_denmark" = "The Capital Region of Denmark",
                                                      "central_denmark_region" = "Central Denmark Region",
                                                      "the_region_of_southern_denmark" = "The Region of Southern Denmark",
                                                      "region_zealand" = "Region Zealand",
                                                      "the_north_denmark_region" = "The North Denmark Region"),
                                         "education" = c("low" = "Low",
                                                         "medium" = "Medium",
                                                         "high" = "High"),
                                         "income" = c("q1" = "1st Quartile",
                                                      "q2" = "2nd Quartile",
                                                      "q3" = "3rd Quartile",
                                                      "q4" =  "4th Quartile"),
                                         "marital" = c("married" = "Married",
                                                       "unmarried" = "Unmarried",
                                                       "divorced" = "Divorced"))) %>%
    mutate(age = round(as.numeric(index - birth)/365.25,1)) %>%
    cutR({{num_vars}},
       seqlist = seqlist,
       names = names) %>%
    mutate(across(c(unlist(names), {{cat_vars}}), ~ fct_drop(.)))

  }
