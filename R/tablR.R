#' Create baseline table
#'
#' @description
#' Wrapper for the tableby function in the Arsenal package
#'
#' @param data dataframe
#' @param group Grouping variable. If omitted a single column tabel is provided
#' @param vars Variables that the grouping variable should be aggregated by
#' @param test Whether statistical tests should be performed (default = FALSE)
#' @param total Whether a total column should be included (default = FALSE)
#' @param numeric Selection of the type of stats for numerical variables (e.g. median, q1q3, range, mean, sd)
#' @param direction Direction for percentages (colwise or rowwise)
#' @param labels List specifying labels of the specific labels for each variable
#' @param reference List specifying reference group for each variable
#' @param headings List specifying labels for variable names
#' @param reverse whether the order of groups should start with the highest level (default = T)
#' @param test.stats Vector of length 2 containing statistical tests that should be performed
#' @param show.na Whether NAs should be presented
#' @param censur whether counts <= 3 should be censored
#' @param digits number of digits
#'
#' @return Returns a table and exports a word-file (optional if filename is provided)
#' @export
#'
#'

# n <- 300
# set.seed(1)
# df <- riskRegression::sampleData(n, outcome="survival")
# df$time <- round(df$time,1)*12
# df$X1 <- factor(rbinom(n, prob = c(0.3,0.4) , size = 2), labels = paste0("T",0:2))
# df$set <- as.factor(rep(seq(1,10),each=30))
# df$age_group <- sample(c("80-90", "10-20", "110-120", "0-40", ">120"), n, replace=TRUE)
#
# df <- as.data.frame(df)
#
# t1 <- tablR(df,
#       group=X2,
#       vars=c(X1,X3,X4,X6,X7, age_group),
#       labels = list("age_group" = c("0-40" = "<=40"),
#                     "X1" = c("T2" = "T2-T3")),
#       headings = list("age_group" = "Age2",
#                       "X6" = "New_var"),
#       total = T,
#       numeric = c("median", "q1q3", "range"),
#       test = T,
#       show.na=T,
#       censur = F)

tablR <- function(data,
                  group,
                  vars,
                  test=FALSE,
                  total=FALSE,
                  numeric = c("median", "q1q3", "range"),
                  direction="colwise",
                  labels= list(),
                  reference = list(),
                  headings = list(),
                  reverse = T,
                  test.stats = c("kwt", "chisq"),
                  show.na = FALSE,
                  censur=F,
                  digits = 1) {

  numeric <- match.arg(numeric, c("median", "q1q3", "iqr", "range", "mean", "sd", "min", "max"), several.ok = T)
  direction <- match.arg(direction, c("colwise", "rowwise"))
  test.stats <- match.arg(test.stats, c("kwt", "chisq", "anova"), several.ok = T)

  if(direction == "rowwise") {
    categorical <- "countrowpct"
  } else {
      categorical <- "countpct"
  }

  if(show.na) {
    numeric <- c("Nmiss", numeric)
    categorical <- c("Nmiss", categorical)
  }


  vars_c <- data %>% select({{vars}}) %>% names()
  group_c <- data %>% select({{group}}) %>% names()

  if(reverse) {
  data <- data %>% mutate(!!sym(group_c) := fct_rev(!!sym(group_c)))
  }

  for(v in names(labels)) {

    data <- data %>% mutate(!!sym(v) := recode(!!sym(v), !!!labels[[v]]))

  }

  c <- tableby.control(test=test, total=total,
                       numeric.test=test.stats[1], cat.test=test.stats[2],
                       numeric.stats=numeric,
                       cat.stats=categorical,
                       stats.labels=list(median="Median", q1q3="Q1, Q3", iqr = "IQR", mean = "Mean", sd="SD", range = "Range", Nmiss = "Missing")
                       )

  form <- paste0(group_c, " ~ ", paste0(vars_c, collapse="+"))

  table <- tableby(as.formula(form), data=data, control=c)

  #Autoformatting (to_title and spacing)
  headings_default <- as.list(str_to_title(str_replace_all(vars_c, "_", " "))) %>% set_names(vars_c)
  headings <- modifyList(headings_default, headings)

s <- summary(table,
        text=T,
        labelTranslations = headings,
        digits = digits)

if(censur) {
  for(v in c(as.character(unique(data[, group_c])), "Total")) {

    for(i in 1:length(s$object[[1]][v][[1]])) {

      if("tbstat_countpct" %in% class(s$object[[1]][v][[1]][[i]]) & s$object[[1]][v][[1]][[i]][1] != "" & s$object[[1]][v][[1]][[i]][1] <= 3) {

        s$object[[1]][v][[1]][[i]]<- "<=3"

      }

    }
  }
}

s
}


