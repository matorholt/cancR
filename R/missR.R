#' Overview of NAs in a dataframe
#'
#'
#' @param data data frame
#' @param vars vars where the na check should be beformed. If missing the whole data frame is analysed
#' @param drop.rows whether to remove rows containing all NA values, default = F
#' @param drop.cols  whether to remove columns contain all NA values, default = F
#' @param return.id whether rows with any NA should be returned, default = F
#' @param dt whether the data.frame should be returned as a data.table, default = F
#' @param print whether the NA check should be printed in the console, default = T
#' @param verbose whether cli messages should be printed, default = T
#'
#' @details
#' If drop.cols or drop.rows are TRUE, the data.frame is returned as modified. Otherwise the table of missing data is returned.
#'
#'
#' @return Prints whether any NAs are detected and returns a data frame with IDs and columns with NA
#' @export
#'
#'

# n=200
# set.seed(1)
# df <- data.frame(ID=seq(1:n),
#                  group=sample(c("pre", "sub"), n, replace=T),
#                  sex=factor(sample(c("M","F"), n, replace=T)),
#                  age_group=sample(c("<50",">50"),n,replace=T),
#                  chemo = sample(c("yes","no"), n, replace=T),
#                  age = sample(c(seq(50,60), 50), n, replace=TRUE),
#                  hospital = sample(c("rh","herlev","roskilde"), n, replace=T)) %>%
#   mutate(hospital = ifelse(group %in% "sub", "roskilde", hospital),
#          chemo = ifelse(group %in% "pre", "yes", chemo),
#          age_group = ifelse(group %in% "sub", "<50", age_group),
#          hospital = as.factor(hospital))
#
# #add random NA
# df <- apply (df, 2, function(x) {x[sample( c(1:n), floor(n/10))] <- NA; x} ) %>%
#   as_tibble()
#
# missR(df)


missR <- function(data,
                  vars,
                  drop.rows = F,
                  drop.cols = F,
                  return.id = F,
                  dt = F,
                  print = T,
                  verbose = T) {

  #Return DT if input is DT and dt is not specified
  if(is.data.table(data) && missing(dt)) dt <- T

  dat <- as.data.table(data)

  if(missing(vars)) {
    vars_c <- names(dat)
  } else {
    vars_c <- defusR(vars)
  }

  total <- nrow(dat)

  miss_df <- dat[, map(.SD, ~ sum(is.na(.x))), .SDcols = vars_c] %>%
    melt(measure.vars = vars_c, value.name = "count") %>%
    .[, pct := round((count/total) * 100,1)] %>%
    setorder(-pct)

  vars_z <- as.character(miss_df[pct == 100]$variable)

  if(return.id) {

    cli::cli_text("Returning IDS with missing values")
    return(rowR(dat,
                vars_c,
                type = "any.na",
                filter = "keep"))

  }

  if(drop.cols || drop.rows) {

    if(verbose) cli::cli_text("Missing variables")
    if(print) print(miss_df)

    drops <- c()

    if(drop.cols) {

      dat <- dat[, c(vars_z) := NULL]
      drops <- c("columns")
    }

    if(drop.rows) {

      dat <- rowR(dat, vars_c[vars_c %nin% vars_z], type = "all.na", filter = "remove")
      drops <- c(drops, "rows")
    }

    if(verbose) cli::cli_text("Returning dataset {if(length(drops) > 0) paste0(\'with \', paste0(drops, collapse = \' and \'), \' removed\')}")

    if(dt) return(dat) else return(as.data.frame(dat))
  } else {
    return(miss_df)
  }
}
