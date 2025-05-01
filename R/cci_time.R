#' Time-dependent CCI
#'
#' @description
#' Generation of time-dependent CCI and comorbidities from the Charlson Comorbidity Index
#'
#'
#' @param data Full LPR dataset
#' @param interval Interval in which the CCIs should be calculated
#' @param comorb Whether the specific comorbidites in the CCI score should be extracted. Default is FALSE.
#'
#' @return
#' cci_all: Dates of all CCI scores. Repeted scores in adjacent rows are removed as the status is not changed.
#' cci_bin: Same as cci_all with CCI scores binned 1, 2-3, 4-5 and 6+
#' comorb: Dates of all comorbidites
#' cci_all_wide: Date of first value of each CCI score for each patient in wide format
#' cci_bin_wide: Date of first value of CCI binned into 1, 2-3, 4-5 and 6+ in wide format
#' comorb_wide: Date of first presence of each comorbidity in wide format
#'
#' @export
#'
#'
#
# set.seed(1)
# df <- simAdmissionData(200, m=30)
#
# (t <- cci_time(df, comorb = T))


cci_time <- function(data, interval = 365.25/2, comorb=F) {
  cci_total <- lapply(c(seq(as.Date("1977-01-01"), as.Date("2024-12-31"), by = interval)), function(x) {
    df <- copy(data)[inddto <= x][,charlson.date := x]
    ci <- charlsonIndex(df,ptid="pnr",vars="diag",data.date="inddto",
                        charlson.date="charlson.date")
    lst <- list(ci[[1]], ci[[2]])
    names(lst) <- c("cci", "comorb")
    return(lst)
  })

  cci_all <- rbindlist(lapply(cci_total, function(x) {
       x$cci
     }))[!is.na(pnr)] %>%
         arrange(pnr, charlson.date) %>%
         group_by(pnr) %>%
    filter(charlson.index != lag(charlson.index) | row_number() == 1) %>%
    ungroup() %>%
    mutate(cci = as.numeric(charlson.index)) %>%
    select(-charlson.index)

    cci_bin <-
      cci_all %>%
      mutate(cci_bin = case_match(cci,
                                  1 ~ "cci_1",
                                  c(2,3) ~ "cci_2-3",
                                  c(4,5) ~ "cci_4-5",
                                  seq(6,18) ~ "cci_6+"))

    cci_all_wide <-
    rbindlist(lapply(cci_total, function(x) {
    x$cci
  }))[!is.na(pnr)] %>%
      arrange(pnr, charlson.date) %>%
      group_by(pnr) %>%
      mutate(cum = cummax(charlson.index)) %>%
      filter(charlson.index == cum) %>%
      group_by(pnr, charlson.index) %>%
      slice(1) %>%
      select(-cum) %>%
      arrange(charlson.index) %>%
      pivot_wider(names_from=charlson.index, values_from = charlson.date, names_prefix = "cci_") %>%
      pivot_longer(contains("cci"), names_to = "cci", values_to = "date") %>%
      fill(date, .direction = "up") %>%
      ungroup() %>%
      pivot_wider(names_from=cci, values_from = date)

 cci_bin_wide <-
    cci_all %>%
    pivot_longer(contains("cci"), names_to = "cci", values_to = "date") %>%
    drop_na(date) %>%
    mutate(cci_bin = case_when(str_detect(cci, "_1$") ~ "0-1",
                               str_detect(cci, "_[23]$") ~ "2-3",
                               str_detect(cci, "_[45]$") ~ "4-5",
                               T ~ "6+")) %>%
    select(-cci) %>%
    group_by(pnr, cci_bin) %>%
    slice(1) %>%
    ungroup() %>%
    pivot_wider(names_from = cci_bin, values_from = date, names_prefix = "cci_")

if(comorb) {
 comorb <- rbindlist(lapply(cci_total, function(x) {
   x$comorb %>%
     pivot_longer(cols=c(-matches("pnr|charlson.date")), names_to = "disease", values_to = "score") %>%
     filter(score == 1)

 }), fill=T)[!is.na(pnr)] %>%
   group_by(pnr, disease) %>%
   filter(disease != lag(disease) | row_number() == 1) %>%
   ungroup() %>%
   select(pnr, charlson.date, disease)

 comorb_wide <- rbindlist(lapply(cci_total, function(x) {
    x$comorb %>%
      pivot_longer(cols=c(-matches("pnr|charlson.date")), names_to = "disease", values_to = "score") %>%
      filter(score == 1)

  }), fill=T)[!is.na(pnr)] %>%
    group_by(pnr, disease) %>%
    slice(1) %>%
    ungroup() %>%
    pivot_wider(names_from=disease, values_from = charlson.date)

 return(lst(cci_all, cci_bin, cci_all_wide, cci_bin_wide, comorb, comorb_wide))
}

 return(lst(cci_all, cci_bin, cci_all_wide, cci_bin_wide))

}





