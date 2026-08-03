removR <- function(script) {
  script <- str_replace_all(script, "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}", "<TIMESTAMP>")
  script <- str_replace_all(script, "\\d+(\\.\\d+)?\\s*(secs?|mins?|ms)", "<RUNTIME>")
  script
}

n=2000
c=10


pop <- simulatR("match",
                n=n,
                match.cases = c) %>%
  mutate(byear = round(runif(n, 1955,1965),0),
         ethnic = sample(c("euro", "africa", "asia"), n, replace=TRUE))

covariates_df
set.seed(1)
update.frame <- data.frame(pnr = seq(0,100),
                           connective = sample(c(as.Date("2000-01-01") + sample(0:365*10, n, replace = TRUE), as.Date(NA)), 101, replace=TRUE),
                           new = sample(c(as.Date("2000-01-01") + sample(0:365*10, n, replace = TRUE), as.Date(NA)), 101, replace=TRUE))





split(unique(covariates_df$pnr), cut(seq_along(unique(covariates_df$pnr)), 10, labels = FALSE))

test_that("updatR, t1", {
  expect_snapshot(updatR(covariates_df,
                         update.frame,
                         vars = c(connective, new),
                         indices = pop[pop$case == 1,"index"],
                         pnrs = pop$pnr)
                  ,
                  transform = removR)
})
