#' Overview of NAs in a dataframe
#'
#'
#' @param data data frame
#' @param vars vars where the na check should be beformed. If missing the whole data frame is analysed
#' @param drop.rows whether to remove rows containing all NA values, default = F
#' @param drop.cols  whether to remove columns contain all NA values, default = F
#' @param return.id whether rows with any NA should be returned, default = F
#' @param na.remove whether all ("all.na") or any ("any.na") NAs should be removed (default = "all.na")
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
# df <- apply(df, 2, function(x) {x[sample( c(1:n), floor(n/10))] <- NA; x}) %>%
#   as_tibble() %>%
#   mutate(na_test = NA)
#
# missR(df, drop.rows = T, drop.cols = T, dt =T)


missR <- function(data,
                  vars,
                  drop.rows = F,
                  drop.cols = F,
                  return.id = F,
                  na.remove = "all.na",
                  return.data = F,
                  dt = NULL,
                  print = T,
                  verbose = T) {

  #Return DT if input is DT and dt is not specified
  if(is.null(dt)) dt <- is.data.table(data)

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

  na_cols <- as.character(miss_df[pct == 100]$variable)
  na_rows <- nrow(miss_df[pct > 0]) - length(na_cols)

  if(return.id) {

    cli::cli_text("Returning IDS with missing values")
    return(rowR(dat,
                vars_c,
                type = "any.na",
                filter = "keep"))

  }

  if(print) {
    if(verbose) cli::cli_text("Missing variables")
    print(miss_df)
  }


  drops <- c()
  if((drop.cols || drop.rows) && sum(c(na_rows, length(na_cols))) > 0) {

    if(drop.cols & length(na_cols > 0)) {

      dat <- dat[, c(na_cols) := NULL]
      drops <- c("NA columns")
    }

    if(drop.rows && na_rows > 0) {

      dat <- rowR(dat, vars_c[vars_c %nin% na_cols], type = na.remove, filter = "remove")
      drops <- c(drops, paste0(str_extract(na.remove, "all|any"), " NA rows", collapse = " "))
    }

    if(verbose) cli::cli_text("Returning dataset {if(length(drops) > 0) paste0(\'with \', paste0(drops, collapse = \' and \'), \' removed\')}")

  }

  if(length(drops) > 0 || return.data) {
    if(dt) return(dat) else return(as.data.frame(dat))
  } else {
    return(miss_df)
  }


}
