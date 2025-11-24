set.seed(1)
n <- 500

redcap_df <-
  data.frame(id = 1:n,
             sex = sample(c(1,2),
                          size = n,
                          replace = TRUE),
             birth = as.character(sample(seq(as.Date("1920-01-01"),
                                             as.Date("1980-12-31"),
                                             by = "days"),
                                         size = n,
                                         replace = T)),
             followup = as.character(sample(seq(as.Date("2021-01-01"),
                                                as.Date("2025-12-31"),
                                                by = "days"),
                                            size = n,
                                            replace = T)),
             dead = rbinom(n, 1, 0.4),
             event1 = rbinom(n,2,0.5),
             event2 = rbinom(n,2,0.2),
             date_of_surgery = as.character(sample(seq(as.Date("1990-01-01"),
                                                       as.Date("2009-12-31"),
                                                       by = "days"),
                                                   size = n,
                                                   replace = T)),
             size = runif(n, min = 2, max = 50),
             type = rbinom(n, size = 2, prob = 0.5),
             localisation = rbinom(n, size = 5, prob = 0.5),
             necrosis = sample(c(0,1,NA), size = n, replace = TRUE),
             margins = sample(c("0","1"), size = n, replace=TRUE),
             cd10 = sample(c(0,1,NA), size = n, replace = TRUE),
             sox10 = sample(c(0,1,NA), size = n, replace = TRUE),
             ck = sample(c(0,1,NA), size = n, replace = TRUE)) %>%
  mutate(death_date = ifelse(dead == 1, as.character(sample(seq(as.Date("2016-01-01"),
                                                                as.Date("2020-12-31"),
                                                                by = "days"))), NA),
         recurrence_date = ifelse(event1 == 1, as.character(sample(seq(as.Date("2010-01-01"),
                                                                       as.Date("2015-12-31"),
                                                                       by = "days"))), NA),
         metastasis_date = ifelse(event2 == 1, as.character(sample(seq(as.Date("2010-01-01"),
                                                                       as.Date("2015-12-31"),
                                                                       by = "days"))), NA)) %>%
  mutate(age = round(as.numeric(as.Date(date_of_surgery) - as.Date(birth)) / 365.25, 1),
         across(c(birth, followup), ~ str_replace(., "(\\d{4})-(\\d{2})-(\\d{2})", "\\3-\\2-\\1"))) %>%
  select(-dead, -event1, -event2) %>%
  select(id, sex, age, birth, everything())

usethis::use_data(redcap_df, overwrite = TRUE)
