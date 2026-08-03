removR <- function(script) {
  script <- str_replace_all(script, "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}", "<TIMESTAMP>")
  script <- str_replace_all(script, "\\d+(\\.\\d+)?\\s*(secs?|mins?|ms)", "<RUNTIME>")
  script
}


analysis_df <- analysis_df %>%
  mutate(X11 = sample(c(NA, "no", "yes"), nrow(analysis_df), replace=TRUE)) %>%
  factR(X11)

model <- glm(g2 == "T1" ~ X4 + X5 + X6 + X7, data = analysis_df, family = "binomial")

test_that("weightR, t1", {
  expect_snapshot(weightR(analysis_df %>% factR(g2),
                          model,
                          treatment = g2,
                          vars = c(X4, X5, X6, X7, X11))$table,
                  transform = removR)
})


