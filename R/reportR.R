#' reportR
#'
#' @description
#' Automatic tablR function for overview of matched and unmatched cases
#'
#' @param data matched dataset
#' @param casename name or number indicating cases (e.g. "1" or "CLL")
#' @param vars vars that should be in the table
#' @param table whether a table should be made (default = F)
#' @param plot whether a plot should be made (default = F)
#' @param type whether non-matched counts should be collapsed to "unmatched" ("simple") or remain stratified ("full")
#' @param cols color palette (default is cancR_palette)
#' @param headings List specifying labels for variable names
#' @param layout layout of the bar chart (horizontal or vertical (default))
#' @param vjust vertical adjustment of the counts (pct) labels
#' @param text.color label colors
#' @param ... passed to tablR
#'
#' @return Prints the matching report, table and plot. Returns af list of the table and plot.
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
# reportR(t1)
#
# (tdf <-
#     t1 %>%
#     formatR(labels = list("case" = c("0" = "No CLL", "1"="CLL"))))
#
# tt <- tdf %>%
#   formatR(layout = "matching") %>%
#   reportR(casename = "CLL",
#           table=T,
#           plot = T,
#           type = "simple",
#           headings = list("cci" = "Charlson Comorbidity Index"),
#           layout = "horizontal",
#           vjust = 1,
#           text.color = "Black")
#
# match_report$report
# match_report$table
# match_report$plot



reportR <- function(data,
                    casename,
                    vars = c(period, age_group, sex, education, income, cci, region, marital),
                    table = F,
                    plot = F,
                    type = "simple",
                    cols = cancR_palette,
                    headings = list(),
                    layout = "vertical",
                    vjust = -0.5,
                    text.color = "White",
                    ...) {

  type <- match.arg(type, c("simple", "full"))

  report <- data %>% group_by(set) %>%
    summarise(matches = n()-1) %>%
    group_by(matches) %>%
    summarise(count = n()) %>%
    mutate(pct = paste0(round(count / sum(count)*100,1), "%")) %>%
   print()

  case_reuse <- sum(unique(data$pnr[data$case == 1]) %in% unique(data$pnr[data$case == 0]))
  counts <- as.data.table(data)[case == 0, .N, by = pnr][N > 1]

  if(case_reuse > 0 | nrow(counts) > 0) cat("\nOBS\n")
  if(case_reuse > 0) cat(paste0(case_reuse, " cases were reused\n"))
  if(nrow(counts) > 0) cat(paste0(nrow(counts), " controls were reused, max reuse: ", max(counts$N)))


  returnlist <- list(report = report)

  if(table | plot) {
  vars_c <- data %>% select({{vars}}) %>% names()

  #Autoformatting (to_title and spacing)
    headings_default <- as.list(str_to_title(str_replace_all(vars_c, "_", " "))) %>% set_names(vars_c)
    headings <- modifyList(headings_default, headings)

    n.controls <- max(report$matches)


  if(type == "full") {

  m <- paste0(seq(0,n.controls-1)," Matches")
  names(m) <- as.character(seq(0,n.controls-1))

  d <- data %>%
    group_by(set) %>%
    mutate(n_controls = as.character(n() - 1)) %>%
    ungroup() %>%
    factR(n_controls, labels=m, lab_to_lev=T) %>%
    filter(case == casename) %>%
    as.data.frame()
  } else {

    d <- data %>%
      group_by(set) %>%
      mutate(n_controls = ifelse(n()-1 == n.controls, "Matched", "Unmatched")) %>%
      ungroup() %>%
      factR(n_controls, levels = c("Matched", "Unmatched")) %>%
      filter(case == casename) %>%
      as.data.frame()


  }


    if(table) {
    (t <- d %>% tablR(group=n_controls,
                      vars = vars_c,
                      headings = headings,
                      reverse=F,
                      ...))

      returnlist <- append(returnlist, list("table" = t))
  }

  if(plot) {

    if("region" %in% colnames(d)) {
    d <- d %>% mutate(region = str_remove_all(region, "Region|Denmark|The|of| "))
    }

   p <- summarisR(d, vars = vars_c, group = n_controls, headings = headings, layout = layout, vjust = vjust, text.color = text.color)

   print(p)

   returnlist <- append(returnlist, list("plot" = p))
  }








  }

  invisible(returnlist)

}
