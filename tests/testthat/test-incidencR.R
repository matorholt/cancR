#Removal of timestamps
removR <- function(script) {
  script <- str_replace_all(script, "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}", "<TIMESTAMP>")
  script <- str_replace_all(script, "\\d+(\\.\\d+)?\\s*(secs?|mins?|ms)", "<RUNTIME>")
  script
}

test_that("incidencR, rates", {
  expect_snapshot((rates <-
                     incidencR(redcap_df %>%
                                 recodR(list("sex" = list("Female" = 1,
                                                          "Male" = 2))),
                               index = date_of_surgery,
                               group = type,
                               unit = 100000,
                               reference = "full",
                               #reference = "partial",
                               ci.method = "lognormal",
                               strata = list(c("year"),
                                             c("age", "sex"),
                                             c("year", "type"),
                                             c("year", "type", "age"),
                                             c("type", "age"),
                                             c("year", "age", "sex", "type")))),
                  transform = removR)
})

