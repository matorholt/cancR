covariates_df <-
  simulatR("covariates",
           start.date = "1980-01-01",
         n=4000)

usethis::use_data(covariates_df, overwrite = TRUE)
