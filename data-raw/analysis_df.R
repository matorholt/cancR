n <- 2000
set.seed(1)
df <- riskRegression::sampleData(n, outcome="survival")
df$time <- round(df$time,1)*12
df$time2 <- df$time + rnorm(n)
df$X1 <- factor(rbinom(n, prob = c(0.3,0.4) , size = 2), labels = paste0("T",0:2))
df$X3 <- factor(rbinom(n, prob = c(0.3,0.4,0.3) , size = 3), labels = paste0("T",0:3))
df$event2 <- rbinom(n, 2, prob=.3)
df <- as.data.frame(df)

analysis_df <- df %>% mutate(X2 = ifelse(row_number()==1, NA, X2),
                     event = as.factor(event)) %>%
  rename(ttt = time) %>%
  select(-eventtime, -censtime)

usethis::use_data(analysis_df, overwrite = TRUE)
