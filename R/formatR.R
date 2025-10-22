#' Auto-formatting of a data frame with layout option for typical levels and labels
#'
#'
#' @param data dataframe. Works with piping
#' @param cat_vars Vector of categorical covariates which should be formatted to factors
#' @param cut_vars vector of numerical covariates that should be cut
#' @param num_vars vector of pseudonumerical variables that should be ordered (e.g. 1990-2000, 2000-2010).
#' @param labels list of labels (e.g. list("var" = c("a" = "apple", "b" = "banana")))
#' @param seqlist list of sequences for cutting (e.g. list("age" = seq(0,100,10)))
#' @param names names for the cut variable (e.g. list("age" = "age_group")
#' @param layout layout preset for common procedures. "Matching" for typical matching studies
#'
#' @return Returns the same dataframe with print-friendly labels for the typical full matching setup.
#' @export
#'
#'

# library(foreach)
# library(doParallel)
#
# no = 40000
# cno= 0.0025*no
#
#
# set.seed(2)
# (c <- data.frame(id = paste("pnr", rnorm(cno, 40000,1000)),
#                  case = 1,
#                  index = sample(c(sample(seq(as.Date('1990/01/01'), as.Date('2010/01/01'), by="day"))), size = cno, replace=TRUE),
#                  follow = c(sample(seq(as.Date('2015/01/01'), as.Date('2020/01/01'), by="day"), cno, replace=T)),
#                  birth = sample(c(sample(seq(as.Date('1958/01/01'), as.Date('1961/01/01'), by="day"))), size = cno, replace=TRUE),
#                  byear = sample(c(seq(1958,1961)), cno, replace=T),
#                  sex = sample(c("f","m"), cno, replace=T),
#                  skinc = sample(c(sample(seq(as.Date('2011/01/01'), as.Date('2020/01/01'), by="day")),rep(as.Date(NA),8000)), size = cno, replace=TRUE),
#                  imm_sup = sample(c(sample(seq(as.Date('2011/01/01'), as.Date('2020/01/01'), by="day")),rep(as.Date(NA),8000)), size = cno, replace=TRUE),
#                  random1 = 1,
#                  random2 = 2) %>%
#     mutate(across(c(skinc, imm_sup), ~ if_else(. > follow | .< index, follow-100, .))))
#
# (cnt <- data.frame(id = paste("pnr", rnorm(no, 40000,1000)),
#                    case = 0,
#                    follow = c(sample(seq(as.Date('1980/01/01'), as.Date('2020/01/01'), by="day"), no, replace=T)),
#                    birth = sample(c(sample(seq(as.Date('1958/01/01'), as.Date('1961/01/01'), by="day"))), size = cno, replace=TRUE),
#                    byear = sample(c(seq(1958,1961)), no, replace=T),
#                    sex = sample(c("f","m"), no, replace=T),
#                    skinc = sample(c(sample(seq(as.Date('1985/01/01'), as.Date('2000/01/01'), by="day")),rep(as.Date(NA),8000)), size = no, replace=TRUE),
#                    imm_sup = sample(c(sample(seq(as.Date('1985/01/01'), as.Date('2000/01/01'), by="day")),rep(as.Date(NA),8000)), size = no, replace=TRUE),
#                    random1 = 1,
#                    random2 = 2) %>%
#     mutate(across(c(skinc, imm_sup), ~ if_else(. > follow, follow-2000, .))))
#
# pop <- bind_rows(c, cnt)
#
# set.seed(1)
# mp <- 40
# (ses <- data.frame(var = sample(c("marital",
#                                   "education",
#                                   "income",
#                                   "cci",
#                                   "region"),
#                                 size = no*mp,
#                                 replace=TRUE),
#                    date = sample(c(sample(seq(as.Date('1980/01/01'), as.Date('2000/01/01'), by="day"))), size = no*mp, replace=TRUE),
#                    id = sample(pop$id, size = no*mp, replace=TRUE)) %>%
#     arrange(id) %>%
#     mutate(val = case_when(var %in% "income" ~ sample(paste("q", seq(1,4), sep=""), no*mp, prob = rep(0.25,4), replace=TRUE),
#                            var %in% "education" ~ sample(c("low", "medium", "high"), no*mp, prob = rep(1/3,3), replace=TRUE),
#                            var %in% "cci" ~ sample(c("cci_0",
#                                                         "cci_1",
#                                                         "cci_2-3",
#                                                         "cci_4-5",
#                                                         "cci_6+"), no*mp, prob = rep(0.20,5), replace=TRUE),
#                            var %in% "region" ~ sample(c("the_capital_region_of_denmark",
#                                                         "region_zealand",
#                                                         "the_north_denmark_region",
#                                                         "central_denmark_region",
#                                                         "the_region_of_southern_denmark"), no*mp, prob = rep(0.20,5), replace=TRUE),
#                            var %in% "marital" ~ sample(c("married",
#                                                         "unmarried",
#                                                         "divorced"), no*mp, prob = rep(0.20,3), replace=TRUE))) %>%
#     select(id, date, var, val))
#
# ses_wide <- ses %>% arrange(id, date) %>%
#   distinct(id, date, .keep_all = T) %>%
#   pivot_wider(names_from=var, values_from = val) %>%
#   fill(income, marital, region, education, cci, .direction = "down")
#
# t1 <- matchR(data=pop,
#              case=case,
#              pnr = id,
#              fu=follow,
#              index=index,
#              td.date=date,
#              fixed.vars=c(byear, sex),
#              td.vars = c(education, income, cci, region, marital),
#              exclude = c(skinc, imm_sup),
#              td.frame = ses_wide,
#              n.controls=4,
#              seed=1)
#
#
# (tdf <-
#     t1 %>%
#     formatR(labels = list("case" = c("0" = "No CLL", "1"="CLL"))))
# #
# str(tdf)
# #
# (tdf2 <- tdf %>% formatR(layout = "matching"))
# #
# str(tdf2)


formatR <- function(data,
                    cat_vars = c(case, sex, cci, region, education, income, marital),
                    num_vars = c(period, age_group),
                    cut_vars = c(age, index),
                    labels = list(),
                    seqlist = list(),
                    names = list(),
                    layout = NULL) {



  seqlist_default <- list("age" = seq(0,150,10),
                          "index" = seq(1980,2030,5))
  names_default <- list("age" = "age_group",
                        "index" = "period")

  seqlist <- modifyList(seqlist_default, seqlist)
  names <- modifyList(names_default, names)


  if(!is.null(layout)) {

    if(layout == "matching") {

      labels_default <- list(
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
                      "divorced" = "Divorced"))

    }

    labels <- modifyList(labels_default, labels)

    data <- data %>%
    factR(num_vars = c({{num_vars}}), labels = labels) %>%
    factR(vars=c({{cat_vars}}), labels = labels, lab_to_lev=T)



  } else {


  data <- data %>%
    mutate(age = round(as.numeric(index - birth)/365.25,1)) %>%
    cutR({{cut_vars}},
         seqlist = seqlist,
         name.list = names) %>%
    factR(vars=c({{cat_vars}}, {{num_vars}}), labels = labels, lab_to_lev=F) %>%
    mutate(across(c(as.vector(unlist(names)), {{cat_vars}}), ~ fct_drop(.)))



  }

  data
}


