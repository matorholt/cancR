n <- 2000
set.seed(1)
df <- riskRegression::sampleData(n, outcome="survival")
df$time <- round(df$time,1)*12
df$time2 <- df$time + rnorm(n)
df$X1 <- factor(rbinom(n, prob = c(0.3,0.4) , size = 2), labels = paste0("T",0:2))
df$X3 <- factor(rbinom(n, prob = c(0.3,0.4,0.3) , size = 3), labels = paste0("T",0:3))
df$event2 <- rbinom(n, 2, prob=.3)
df$event3 <- rbinom(nrow(df), 3, 0.5)
df <- as.data.frame(df)

analysis_df <- df %>% mutate(X2 = ifelse(row_number()==1, NA, X2),
                     id = sample(seq(1,700), size = n, replace=TRUE)) %>%
  factR(c(X2, event)) %>%
  cutR(c(X6:X10),
       list("quantile", c(0, 0.25,0.5,0.75, 1)),
       name.pattern = "_bin",
       digits = 4) %>%
  rename(ttt = time) %>%
  select(-eventtime, -censtime)

usethis::use_data(analysis_df, overwrite = TRUE)
