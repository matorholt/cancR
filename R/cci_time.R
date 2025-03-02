#' Time-dependent CCI
#'
#' @description
#' Generation of time-dependent CCI and comorbidities from the Charlson Comorbidity Index
#'
#'
#' @param data Full LPR dataset
#' @param interval Interval in which the CCIs should be calculated
#'
#' @return
#' cci: Date of first value of each CCI score for each patient
#' comorb: Date of first presence of each comorbidity
#'
#' @export
#'
#'
#'
cci_time <- function(data, interval = 365.25/2) {
  cci_total <- lapply(c(seq(as.Date("1977-01-01"), as.Date("2024-12-31"), by = interval)), function(x) {
    df <- copy(data)[inddto <= x][,charlson.date := x]
    ci <- charlsonIndex(df,ptid="pnr",vars="diag",data.date="inddto",
                        charlson.date="charlson.date")
    lst <- list(ci[[1]], ci[[2]])
    names(lst) <- c("cci", "comorb")
    return(lst)
  })

  cci <- rbindlist(lapply(cci_total, function(x) {
    x$cci
  }))[!is.na(pnr)] %>%
    group_by(pnr, charlson.index) %>%
    slice(1) %>%
    pivot_wider(names_from=charlson.index, values_from = charlson.date, names_prefix = "cci_")


  comorb <- rbindlist(lapply(cci_total, function(x) {
    x$comorb %>%
      pivot_longer(cols=c(-matches("pnr|charlson.date")), names_to = "disease", values_to = "score") %>%
      filter(score == 1)

  }), fill=T)[!is.na(pnr)] %>%
    group_by(pnr, disease) %>%
    slice(1) %>%
    pivot_wider(names_from=disease, values_from = charlson.date)

  return(lst(cci, comorb))

}
