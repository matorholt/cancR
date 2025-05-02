#' Time-dependent CCI
#'
#' @description
#' Generation of time-dependent CCI and comorbidities from the Charlson Comorbidity Index
#'
#'
#' @param data Full LPR dataset
#' @param interval Interval in which the CCIs should be calculated
#' @param structure Whether CCI should be the full 18 parametres or binned into 1, 2-3, 4-5 and 6+
#' @param format Whether the output should be in long or wide format
#' @param comorb Whether the specific comorbidites in the CCI score should be extracted. Default is FALSE.
#'
#' @return
#' cci_full_long/wide: Dates of all CCI scores. Repeted scores in adjacent rows are removed as the status is not changed.
#' cci_bin_long/wide: Same as cci_all with CCI scores binned 1, 2-3, 4-5 and 6+
#' comorb_long/wide: Dates of all comorbidites
#'
#' @export
#'
#'

# set.seed(1)
# lpr <- simAdmissionData(n=500, m = 20)
#
# cci_time(lpr, structure = "full", format = "wide", comorb=T)

cci_time <- function(data, interval = 365.25/2, structure = "binned", format = "long", comorb=F) {

  structure <- match.arg(structure, c("binned", "full"))
  format <- match.arg(format, c("long", "wide"))

  cci_total <- lapply(c(seq(as.Date("1977-01-01"), as.Date("2024-12-31"), by = interval)), function(x) {
    df <- copy(data)[inddto <= x][,charlson.date := x]
    ci <- charlsonIndex(df,ptid="pnr",vars="diag",data.date="inddto",
                        charlson.date="charlson.date")
    lst <- list(ci[[1]], ci[[2]])
    names(lst) <- c("cci", "comorb")
    return(lst)
  })


  if(structure == "full" & format == "long") {
  cci_full_long <<- rbindlist(lapply(cci_total, function(x) {
       x$cci
     }))[!is.na(pnr)] %>%
         arrange(pnr, charlson.date) %>%
         group_by(pnr) %>%
    filter(charlson.index != lag(charlson.index) | row_number() == 1) %>%
    ungroup() %>%
    mutate(cci = as.numeric(charlson.index)) %>%
    select(-charlson.index)

  }

  if(structure == "binned" & format == "long") {
    cci_bin_long <<-
      rbindlist(lapply(cci_total, function(x) {
        x$cci
      }))[!is.na(pnr)] %>%
      arrange(pnr, charlson.date) %>%
      group_by(pnr) %>%
      filter(charlson.index != lag(charlson.index) | row_number() == 1) %>%
      ungroup() %>%
      mutate(cci = as.numeric(charlson.index)) %>%
      select(-charlson.index) %>%
      mutate(cci_bin = case_match(cci,
                                  1 ~ "cci_1",
                                  c(2,3) ~ "cci_2-3",
                                  c(4,5) ~ "cci_4-5",
                                  seq(6,18) ~ "cci_6+"))
  }

    if(structure == "full" & format == "wide") {
    cci_full_wide <<-
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
  }

  if(structure == "binned" & format == "wide") {
 cci_bin_wide <<-
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
   pivot_wider(names_from=cci, values_from = date) %>%
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
  }

if(comorb & structure == "full" & format == "long") {
 comorb_long <<- rbindlist(lapply(cci_total, function(x) {
   x$comorb %>%
     pivot_longer(cols=c(-matches("pnr|charlson.date")), names_to = "disease", values_to = "score") %>%
     filter(score == 1)
   }), fill=T)[!is.na(pnr)] %>%
   group_by(pnr, disease) %>%
   filter(disease != lag(disease) | row_number() == 1) %>%
   ungroup() %>%
   select(pnr, charlson.date, disease)
}

 if(comorb & format == "wide") {
 comorb_wide <<- rbindlist(lapply(cci_total, function(x) {
    x$comorb %>%
      pivot_longer(cols=c(-matches("pnr|charlson.date")), names_to = "disease", values_to = "score") %>%
      filter(score == 1)

  }), fill=T)[!is.na(pnr)] %>%
    group_by(pnr, disease) %>%
    slice(1) %>%
    ungroup() %>%
    pivot_wider(names_from=disease, values_from = charlson.date)
 }

}







