#' Overview of matched and unmatched cases
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

# n=4000
# c=10
#
# set.seed(1)
# pop <- simulatR("match",
#                n=n,
#                match.cases = c) %>%
#   mutate(byear = round(runif(n, 1955,1965),0),
#          ethnic = sample(c("euro", "africa", "asia"), n, replace=TRUE))
# #
# set.seed(1)
# covariates <- simulatR("covariates",
#                        n=n+c)
# set.seed(1)
# covariates_long <- simulatR("covariates",
#                             format = "long",
#                             n=n+c)

# tdf <-
# matchR(data=pop,
#        follow=follow,
#        fixed.vars = c(byear, sex, ethnic),
#        td.vars = c(education, cancer),
#        exclude = c(skinc, imm_sup),
#        exclude.length = 365.25*5,
#        td.frame = covariates_df,
#        n.controls=2,
#        seed=1,
#        cores = NULL,
#        dt = T)
#

#
# (tdf1 <-
#     tdf %>%
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

  setDT(data)

  report <- data[, .(matches = .N-1), by = set][, .N, by = matches][, pct := paste0(round(N / sum(N)*100,1), "%")] %>% print

  case_reuse <- sum(unique(data$pnr[data$case == 1]) %in% unique(data$pnr[data$case == 0]))
  counts <- as.data.table(data)[case == 0, .N, by = pnr][N > 1]

  if(case_reuse > 0 | nrow(counts) > 0) cli::cli_alert_warning("OBS")
  if(case_reuse > 0) cli::cli_text(paste0(case_reuse, " cases were reused"))
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
