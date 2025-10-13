set.seed(1)
mp <- 40
(ses <- data.frame(var = sample(c("marital",
                                  "education",
                                  "income",
                                  "cci",
                                  "region"),
                                size = no*mp,
                                replace=TRUE),
                   date = sample(c(sample(seq(as.Date('1980/01/01'), as.Date('2000/01/01'), by="day"))), size = no*mp, replace=TRUE),
                   id = sample(match_df$id, size = no*mp, replace=TRUE)) %>%
    arrange(id) %>%
    mutate(val = case_when(var %in% "income" ~ sample(paste("q", seq(1,4), sep=""), no*mp, prob = rep(0.25,4), replace=TRUE),
                           var %in% "education" ~ sample(c("low", "medium", "high"), no*mp, prob = rep(1/3,3), replace=TRUE),
                           var %in% "cci" ~ sample(c("cci_0",
                                                        "cci_1",
                                                        "cci_2-3",
                                                        "cci_4-5",
                                                        "cci_6+"), no*mp, prob = rep(0.20,5), replace=TRUE),
                           var %in% "region" ~ sample(c("the_capital_region_of_denmark",
                                                        "region_zealand",
                                                        "the_north_denmark_region",
                                                        "central_denmark_region",
                                                        "the_region_of_southern_denmark"), no*mp, prob = rep(0.20,5), replace=TRUE),
                           var %in% "marital" ~ sample(c("married",
                                                        "unmarried",
                                                        "divorced"), no*mp, prob = rep(0.20,3), replace=TRUE))) %>%
    select(id, date, var, val))

covariates_df <- ses %>% arrange(id, date) %>%
  distinct(id, date, .keep_all = T) %>%
  pivot_wider(names_from=var, values_from = val) %>%
  fill(income, marital, region, education, cci, .direction = "down")


usethis::use_data(covariates_df, overwrite = TRUE)
