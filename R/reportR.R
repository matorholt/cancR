#' reportR
#'
#' @description
#' Automatic tablR function for overview of matched and unmatched cases
#'
#' @param data matched dataset
#' @param casename name or number of cases (e.g. 1 or "CLL")
#' @param n.controls n.controls from the matchR function
#' @param vars vars that should be in the table
#' @param labels List specifying labels of the specific labels for each variable
#' @param headings List specifying labels for variable names
#' @param filename
#'
#' @return Returns a table of the matching overview and exports a word-file (optional if filename is provided)
#' @export
#'
#'

# library(doSNOW)
#
# no = 40000
# cno= 0.0025*no
#
#
# set.seed(2)
# (c <- data.frame(id = paste("pnr", rnorm(cno, 40000,1000)),
#                  case = 1,
#                  index_cll = sample(c(sample(seq(as.Date('1990/01/01'), as.Date('2010/01/01'), by="day"))), size = cno, replace=TRUE),
#                  follow = c(sample(seq(as.Date('2015/01/01'), as.Date('2020/01/01'), by="day"), cno, replace=T)),
#                  byear = sample(c(seq(1950,1960)), cno, replace=T),
#                  sex = sample(c("f","m"), cno, replace=T),
#                  skinc = sample(c(sample(seq(as.Date('2011/01/01'), as.Date('2020/01/01'), by="day")),rep(as.Date(NA),8000)), size = cno, replace=TRUE),
#                  imm_sup = sample(c(sample(seq(as.Date('2011/01/01'), as.Date('2020/01/01'), by="day")),rep(as.Date(NA),8000)), size = cno, replace=TRUE),
#                  random1 = 1,
#                  random2 = 2) %>%
#     mutate(across(c(skinc, imm_sup), ~ if_else(. > follow | .< index_cll, follow-100, .)),
#            birth = as.Date(str_c(byear, "-01-01"))))
#
# (cnt <- data.frame(id = paste("pnr", rnorm(no, 40000,1000)),
#                    case = 0,
#                    follow = c(sample(seq(as.Date('1980/01/01'), as.Date('2020/01/01'), by="day"), no, replace=T)),
#                    byear = sample(c(seq(1945,1965)), no, replace=T),
#                    sex = sample(c("f","m"), no, replace=T),
#                    skinc = sample(c(sample(seq(as.Date('1985/01/01'), as.Date('2000/01/01'), by="day")),rep(as.Date(NA),8000)), size = no, replace=TRUE),
#                    imm_sup = sample(c(sample(seq(as.Date('1985/01/01'), as.Date('2000/01/01'), by="day")),rep(as.Date(NA),8000)), size = no, replace=TRUE),
#                    random1 = 1,
#                    random2 = 2) %>%
#     mutate(across(c(skinc, imm_sup), ~ if_else(. > follow, follow-2000, .)),
#            birth = as.Date(str_c(byear, "-01-01"))))
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
#                                                      "cci_1",
#                                                      "cci_2-3",
#                                                      "cci_4-5",
#                                                      "cci_6+"), no*mp, prob = rep(1/5,5), replace=TRUE),
#                            var %in% "region" ~ sample(c("the_capital_region_of_denmark",
#                                                         "region_zealand",
#                                                         "the_north_denmark_region",
#                                                         "central_denmark_region",
#                                                         "the_region_of_southern_denmark"), no*mp, prob = rep(0.20,5), replace=TRUE),
#                            var %in% "marital" ~ sample(c("married",
#                                                          "unmarried",
#                                                          "divorced"), no*mp, prob = rep(0.33,3), replace=TRUE))) %>%
#     select(id, date, var, val))
#
# ses_wide <- ses %>% arrange(id, date) %>%
#   distinct(id, date, .keep_all = T) %>%
#   pivot_wider(names_from=var, values_from = val) %>%
#   fill(income, marital, region, education, cci, .direction = "down")
#
# t1 <- matchR(data=pop,
#                case=case,
#                pnr = id,
#                fu=follow,
#                index=index_cll,
#                td_date=date,
#                fixed_vars=c(byear, sex),
#                td_vars=c(education, marital, cci),
#                exclude = c(skinc, imm_sup),
#                td_frame = ses_wide,
#                n_controls=4,
#                seed=1)
#
# d <- formatR(t1, case.labs=c("No CLL", "CLL"))
#
# reportR(d, casename="CLL", table=T, plots=T)


reportR <- function(data,
                    casename,
                    vars = c(period, age_group, sex, education, income, cci, region, marital),
                    table = F,
                    plots = F,
                        ...) {

  vars_c <- data %>% select({{vars}}) %>% names()

  h <- list("cci" = "Charlson Comorbidity Index")

  if(missing(casename)) {casename <- "1"}

  report <- data %>% group_by(set) %>%
    summarise(matches = n()) %>%
    group_by(matches) %>%
    summarise(count = n()) %>%
    print()

  n.controls <- max(report$matches)

  m <- paste0(seq(0,n.controls-1), "Matches")
  names(m) <- as.character(seq(0,n.controls-1))

  d <- data %>%
    group_by(set) %>%
    mutate(n_controls = as.character(n() - 1)) %>%
    ungroup() %>%
    factR(n_controls, labels=m, lab_to_lev=T) %>%
    filter(case == c(casename)) %>%
    as.data.frame()

    if(table) {
    d %>% tablR(group=n_controls,
          vars = vars_c,
          headings = h,
          ...)
  }

  if(plots) {

    cols <- c("#9B62B8", "#224B87", "#67A8DC", "#D66ACE", "orange")

    plist <-
      lapply(vars_c, function(v) {

          ggplot(d, aes(x=!!sym(v), fill=n_controls)) +
            geom_bar() +
            scale_fill_manual(values = cols) +
            theme_classic()

      })

  }

  ggarrange(plotlist=plist, common.legend=T)

}

