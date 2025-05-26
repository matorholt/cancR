#' basetab
#'
#' @description
#' Wrapper for the tableby function in the Arsenal package
#'
#'
#' @param data dataframe
#' @param group Grouping variable. If omitted a single column tabel is provided
#' @param vars Variables that the grouping variable should be aggregated by
#' @param test Whether statistical tests should be performed (default = FALSE)
#' @param total Whether a total column should be included (default = FALSE)
#' @param numeric Selection of the type of stats for numerical variables (e.g. median, q1q3, range, mean, sd)
#' @param direction Direction for percentages (colwise or rowwise)
#' @param labels Labels for the aggregate variables
#' @param test_stats Vector of length 2 containing statistical tests that should be performed
#' @param show_na Whether NAs should be presented
#' @param filename The filname of the exported word-table
#'
#' @return Returns a table and exports a word-file (optional if filename is provided)
#' @export
#'
#'
# library(cancR)
# library(arsenal)
#
# n <- 300
# set.seed(1)
# df <- riskRegression::sampleData(n, outcome="survival")
# df$time <- round(df$time,1)*12
# df$X1 <- factor(rbinom(n, prob = c(0.3,0.4) , size = 2), labels = paste0("T",0:2))
# df$set <- as.factor(rep(seq(1,10),each=30))
#

# basetab(df,
#         group=X2,
#         vars=c(X1,X3,X4,X6,X7),
#         labels = c("X1" = "Treatment",
#                    "X3" = "Sex"),
#         total = T,
#         numeric = c("median", "q1q3", "range"),
#         test = T,
#         show_na=T,
#         filename = "Table 1")


basetab <- function(data,
                    group,
                    vars,
                    test=FALSE,
                    total=FALSE,
                    numeric = c("median", "q1q3", "range"),
                    direction="colwise",
                    labels= NULL,
                    test_stats = c("kwt", "chisq"),
                    show_na = FALSE,
                    filename = NULL) {

  numeric <- match.arg(numeric, c("median", "q1q3", "iqr", "range", "mean", "sd", "min", "max"), several.ok = T)
  direction <- match.arg(direction, c("colwise", "rowwise"))
  test_stats <- match.arg(test_stats, c("kwt", "chisq", "anova"), several.ok = T)

  if(direction == "rowwise") {
    categorical <- "countrowpct"
  } else {
      categorical <- "countpct"
  }

  if(show_na) {
    numeric <- c("Nmiss", numeric)
    categorical <- c("Nmiss", categorical)
  }

  vars_c <- data %>% select({{vars}}) %>% names()

  c <- tableby.control(test=test, total=total,
                       numeric.test=test_stats[1], cat.test=test_stats[2],
                       numeric.stats=numeric,
                       cat.stats=categorical,
                       stats.labels=list(median='Median', q1q3='Q1, Q3', iqr = "IQR", mean = "Mean", sd="SD", range = "Range", Nmiss = "Missing")
                       )

  form <- paste0(substitute(group), " ~ ", paste0(vars_c, collapse="+"))

  table <- tableby(as.formula(form), data=data, control=c)

  if(!is.null(filename)) {
  write2word(table,
             paste0(getwd(), "/", filename, ".docx"),
             quiet = TRUE,
             labelTranslations = labels)
  }


  summary(table,
          text=T,
          labelTranslations = labels)

}


