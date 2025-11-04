set.seed(1)

intervals <- seq(as.Date('2010/01/01'), as.Date('2024/01/01'), by=365.25/2)

covariates_df <-
  bind_rows(lapply(seq_along(match_df$pnr), function(i) {

  bind_rows(
    #CCI
    data.frame(pnr = i,
               date = intervals,
               var = "cci",
               value = sample(c("cci_0",
                                "cci_1",
                                "cci_2-3",
                                "cci_4-5",
                                "cci_6+"), size = length(intervals), replace=TRUE)),
    #Education
    data.frame(pnr = i,
               date = intervals,
               var = "education",
               value = sample(c("low", "medium", "high"), size = length(intervals), replace=TRUE)),

    #Income
    data.frame(pnr = i,
               date = intervals,
               var = "income",
               value = sample(c("q1", "q2", "q3", "q4"), size = length(intervals), replace=TRUE)),

    #marital
    data.frame(pnr = i,
               date = intervals,
               var = "marital",
               value = sample(c("married",
                                "unmarried",
                                "divorced"), size = length(intervals), replace=TRUE)),

    #region
    data.frame(pnr = i,
               date = intervals,
               var = "region",
               value = sample(c("capital",
                                "north",
                                "south",
                                "zealand",
                                "central"), size = length(intervals), replace=TRUE)),

    #degurba
    data.frame(pnr = i,
               date = intervals,
               var = "degurba",
               value = sample(c("city",
                                "suburb",
                                "rural"), size = length(intervals), replace=TRUE)),

    #Comorbidities
    bind_rows(lapply(c("infection",
                       "cancer",
                       "hema",
                       "endo",
                       "psych",
                       "neuro",
                       "cvd",
                       "lungs",
                       "gi",
                       "skin",
                       "connective",
                       "urinary",
                       "congenital"), function(v) {

                         data.frame(pnr = i,
                                    date = intervals,
                                    var = v,
                                    value = as.character(cummax(rbinom(length(intervals), 1, 0.1))))



                       }))


  )



})) %>%
  group_by(pnr, var) %>%
  filter(value != lag(value) | row_number() == 1) %>%
  pivot_wider(names_from=var, values_from=value) %>%
  fill(everything(), .direction = "down")

usethis::use_data(covariates_df, overwrite = TRUE)
