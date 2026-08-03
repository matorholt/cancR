#Removal of timestamps
removR <- function(script) {
  script <- str_replace_all(script, "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}", "<TIMESTAMP>")
  script <- str_replace_all(script, "\\d+(\\.\\d+)?\\s*(secs?|mins?|ms)", "<RUNTIME>")
  script
}

test_that("inferencR, rates", {
  expect_snapshot(inferencR(analysis_df,
                            treatment = X4,
                            timevar = ttt,
                            event = event2,
                            vars = c(X6, X7, X8),
                            outcome.vars = X10,
                            estimator = "GFORMULA",
                            plot=F,
                            weights = F)$table,
                  transform = removR)
})

