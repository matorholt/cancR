removR <- function(script) {
  script <- str_replace_all(script, "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}", "<TIMESTAMP>")
  script <- str_replace_all(script, "\\d+(\\.\\d+)?\\s*(secs?|mins?|ms)", "<RUNTIME>")
  script
}


test_that("datR, t1", {
  expect_snapshot(redcap_df %>%
                    datR(c(birth, date_of_surgery, followup, death_date)) %>%
                    str,
                  transform = removR)
})

