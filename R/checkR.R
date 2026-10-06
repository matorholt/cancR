#' Detection of positivity violations (empty levels)
#'
#' @param data data frame to detect positivity violations
#' @param treatment treatment stratum that should be included to all covariate combinations (optional)
#' @param outcome outcome stratum that should be included to all covariate combinations (optional)
#' @param vars vector of covariates to examine for positivity violations
#' @param id column indicating unique patient identifier for returning specific NAs
#' @param levels the number of covariates for which each treatment and/or outcome level will be counted (default = all covariate combinations)
#' @param quantiles quantile argument for categorization of numeric variables. See `cutR()` for supported quantiles. Default = "decile"
#'
#' @return prints the variables with positivity violations if present, otherwise none detected.
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

# checkR(df,
#        treatment=group,
#        vars=c(sex, age_group),
#        return.counts = F,
#        threshold = 25)
# checkR(df, group, vars=c(sex), levels=1)
# checkR(df, treatment=group, vars=c(age_group, chemo, hospital, size), return.counts = T, levels = 1, threshold = 5, quantiles = "quartile")

checkR <- function(data,
                   treatment = NULL,
                   outcome = NULL,
                   vars = NULL,
                   id,
                   levels=NULL,
                   threshold = 0,
                   return.counts = F,
                   quantiles="decile") {

  if(missing(id)) {

    id_syn <- str_extract(names(data), paste0("\\b", c("id", "ID", "pnr", "pt_id", "study_id", "record_id"), "\\b", collapse = "|")) %>% na.omit

    if(length(id_syn) == 0) return(cli::cli_alert_danger("Error: No ID column identified - please provide"))

    if(sum(id_syn %in% colnames(data)) > 1) {
      return(cli::cli_alert_danger("Multiple ID columns detected - pick only one"))
    }

    id_c <- defusR(id_syn)
  } else {
    id_c <- defusR(id)
  }

  vars_c <- defusR(vars)
  treat_c <- defusR(treatment)
  out_c <- defusR(outcome)

  if(is.null(vars_c)) vars_c <- names(df)

  if(is.null(levels)) levels <- length(vars_c)
  if(levels > length(c(vars_c))) {
    return(cli::cli_alert_danger("ERROR: Levels exceeding number of variables. Levels can maximally be: {length(vars_c)}"))
  }

  dat <- missR(data, vars = c(vars_c, treat_c, out_c), drop.rows = T, drop.cols = T, na.remove = "any.na", print = T, verbose = T, dt = T, return.data = T)

  dat <- dat[, c(vars_c, treat_c, out_c), with = FALSE]

  num_vars <- names(dat)[map_lgl(names(dat), ~ is.numeric(dat[[.x]]))]

  if(length(num_vars) > 0) {

    dat <- cutR(dat,
                num_vars,
                seq.list = "decile")

  }

  dat <- factR(dat,
               c(vars_c, treat_c, out_c))

  grid <- combn(vars_c, levels, simplify = F)

  count_dt <- map(grid, ~ {

    by_cols <- c(.x, treat_c, out_c)

    counts <- dat[, .N, by = c(by_cols)]

    grid <- do.call(CJ, c(map(dat[, ..by_cols], unique), sorted = TRUE))

    out <- counts[grid, on = by_cols]

    if(!return.counts) out[is.na(N) | N < threshold] else out

  }) %>% rbindlist(fill=T)

  setcolorder(count_dt, c(treat_c, out_c, setdiff(names(count_dt), c(treat_c, out_c, "N")), "N"), skip_absent = T)

  if(nrow(count_dt) > 0) {

    cli::cli_alert_info("Positivity violations detected in {nrow(count_dt)} combinations")
    print(as_tibble(missR(count_dt, names(count_dt), drop.cols = T, print = F, verbose = F, return.data = T)))

  } else {
    cli::cli_alert_success("No positivity violations detected")

  }
}
