#Removal of timestamps
removR <- function(script) {
  script <- str_replace_all(script, "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}", "<TIMESTAMP>")
  script <- str_replace_all(script, "\\d+(\\.\\d+)?\\s*(secs?|mins?|ms)", "<RUNTIME>")
  script
}

df2 <- analysis_df %>%
  mutate(X4 = ifelse(X4 == 1, "No CLL", "CLL"))

test_that("extractR, t1", {
  expect_snapshot(estimatR(df2, ttt, event2, X4, vars = c(X6,X7)),
                  transform = removR)
})

test_that("extractR, t2", {
  expect_snapshot(estimatR(df2, ttt, event2, X5, vars = c(X6,X7)),
                  transform = removR)
})
