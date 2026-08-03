removR <- function(script) {
  script <- str_replace_all(script, "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}", "<TIMESTAMP>")
  script <- str_replace_all(script, "\\d+(\\.\\d+)?\\s*(secs?|mins?|ms)", "<RUNTIME>")
  script
}


test_that("cutR, t1", {
  expect_snapshot(redcap_df %>%
                    datR(c(birth, date_of_surgery)) %>%
                    cutR(vars = c(age, birth, size, type, date_of_surgery),
                         seq.list = list(age = "10y",
                                         birth = "year",
                                         size = "quartile",
                                         type = c(0,2,10),
                                         date_of_surgery = "quarter")),
                  transform = removR)
})


test_that("cutR, t2", {
  expect_snapshot(redcap_df %>%
                    datR(c(birth, date_of_surgery)) %>%
                    cutR(c(age, birth), "5y",
                         name.pattern = "_bin"),
                  transform = removR)
})

