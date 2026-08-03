#Removal of timestamps
removR <- function(script) {
  script <- str_replace_all(script, "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}", "<TIMESTAMP>")
  script <- str_replace_all(script, "\\d+(\\.\\d+)?\\s*(secs?|mins?|ms)", "<RUNTIME>")
  script
}

test_that("iteratR, estimatR", {
  expect_snapshot(iteratR(analysis_df,
                          timevar = "ttt",
                          event = c("event", "event", "event2"),
                          group = "g2",
                          survscale = c("OS", "OS", "AM"),
                          time = list(60,60,120),
                          labels = c("m1", "m2", "m3"),
                          vars = c("X6", "X7"),
                          cancR.method = "estimatR"),
                  transform = removR)
})


test_that("iteratR, extractR", {
  expect_snapshot(iteratR(analysis_df,
                          timevar = "ttt",
                          event = c("event", "event", "event2"),
                          group = "g2",
                          survscale = c("OS", "OS", "AM"),
                          time = list(60,60,120),
                          labels = c("m1", "m2", "m3"),
                          vars = c("X6", "X7"),
                          cancR.method = "estimatR"),
                  transform = removR)
})

