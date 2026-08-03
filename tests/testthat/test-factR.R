#Removal of timestamps
removR <- function(script) {
  script <- str_replace_all(script, "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}", "<TIMESTAMP>")
  script <- str_replace_all(script, "\\d+(\\.\\d+)?\\s*(secs?|mins?|ms)", "<RUNTIME>")
  script
}

set.seed(1)

n=50

(df <-
    data.frame(v1 = sample(letters[1:5], size = n, replace=TRUE),
               v2 = sample(letters[1:5], size = n, replace=TRUE),
               v3 = sample(letters[1:5], size = n, replace=TRUE),
               vnum = sample(c("<40", "50-60", "10-20", "100-110", ">110", "cci_0"), size = n, replace=TRUE),
               comorb = rbinom(n, 1, 0.5)))


test_that("factR, already a factor", {
  expect_snapshot(df %>%
                  factR(comorb) %>%
                  factR(num.vars = comorb),
                  transform = removR)
})


test_that("factR, setting reference levels", {
  expect_snapshot(df %>%
                    factR(vars = c(v1, v2,v3),
                    reference = list("v1" = "c")) %>%
                    str,
                  transform = removR)
})


test_that("factR, change labels without changing levels", {
  expect_snapshot(df %>%
                  factR(c(v1, v2, v3), labels = list("v3" = list("alpha" = "a")), lab_to_lev = F) %>%
                  str,
                  transform = removR)
})


test_that("factR, Sort pseudo numeric character variable", {
  expect_snapshot(df %>%
                  factR(num.vars=vnum,
                        labels = list("vnum" = c("test" = "cci_0"))),
                  transform = removR)
})
