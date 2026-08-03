removR <- function(script) {
  script <- str_replace_all(script, "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}", "<TIMESTAMP>")
  script <- str_replace_all(script, "\\d+(\\.\\d+)?\\s*(secs?|mins?|ms)", "<RUNTIME>")
  script
}

n=400
c=10

set.seed(1)
pop <- simulatR("match",
                n=n,
                match.cases = c) %>%
  mutate(byear = round(runif(n, 1955,1965),0),
         ethnic = sample(c("euro", "africa", "asia"), n, replace=TRUE))
#
set.seed(1)
covariates <- simulatR("covariates",
                       n=n+c)
set.seed(1)
covariates_long <- simulatR("covariates",
                            format = "long",
                            n=n+c)

test_that("matchR, t1", {
  expect_snapshot(matchR(data=pop,
                         follow=follow,
                         fixed.vars = c(byear, sex, ethnic),
                         td.vars = c(education, cancer),
                         exclude = c(skinc, imm_sup),
                         td.frame = covariates_df,
                         n.controls=2,
                         seed=1,
                         cores = NULL,
                         dt = T),
                  transform = removR)
})
