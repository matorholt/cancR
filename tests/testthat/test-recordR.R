removR <- function(script) {
  script <- str_replace_all(script, "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}", "<TIMESTAMP>")
  script <- str_replace_all(script, "\\d+(\\.\\d+)?\\s*(secs?|mins?|ms)", "<RUNTIME>")
  script
}

set.seed(1)
df <-
  data.frame(diag = sample(c("DX123", "DC123", "DC234", "DG123", "DG234"), 20, replace=TRUE),
             type = sample(c("DY", "DY234", "DY123"), 20, replace=TRUE),
             type2 = sample(c("DC123", "DC234", "DG123"), 20, replace=TRUE),
             split = c("1,11", "2,10", "2,4", "2,15"))

test_that("recordR, t1", {
  expect_snapshot(recodR(df, namelist = list("diag" = list("KOL" = "DX",
                                            "Astma" = c("DC123", "DC2"),
                                            "AMI" = c("DG123", "DG234"))),
                         match = "start"),
                  transform = removR)
})


test_that("recordR, t2", {
  expect_snapshot(df %>%
                    factR(c(diag, type, type2)) %>%
                    recodR(list("diag" = list("KOL" = "DX123",
                                              "Astma" = c("DC123", "DC234"),
                                              "AMI" = list("DG123", "DG234"))),
                           match = "exact"),
                  transform = removR)
})


test_that("recordR, t3", {
  expect_snapshot(df %>%
                    recodR(list("split" = list("one" = "1",
                                               "four" = "4",
                                               "ten" = "10",
                                               "fifteen" = "15")),
                           match = "boundary"),
                  transform = removR)
})

