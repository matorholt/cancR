match_df <-
  simulatR("match",
         n=4000,
         match.cases = 0.0025*4000)

usethis::use_data(match_df, overwrite = TRUE)
