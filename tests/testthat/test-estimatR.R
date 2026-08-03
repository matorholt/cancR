#Removal of timestamps
removR <- function(script) {
  script <- str_replace_all(script, "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}", "<TIMESTAMP>")
  script <- str_replace_all(script, "\\d+(\\.\\d+)?\\s*(secs?|mins?|ms)", "<RUNTIME>")
  script
}

test_that("estimatR, univariate", {
  expect_snapshot(estimatR(analysis_df, t_event, event, g2),
                  transform = removR)
   })

test_that("estimatR, multivariate with multiple causes", {
  expect_snapshot(estimatR(analysis_df,
                             timevar=t_event,
                             event=event,
                             group=g4,
                             strata = F,
                             method = "cox",
                             survscale = "AM",
                             vars = c(x1,x2,x6),
                             #event.form = "g4 * x1 + x3",
                             cause = 1,
                             plot=T,
                             event.digits = 2,
                             diagnostics = F,
                             conditional = F,
                             proportions = F,
                             survtime = F,
                             unique.events = F,
                             dt=F),
                  transform = removR)
})
