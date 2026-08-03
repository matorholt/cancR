removR <- function(script) {
  script <- str_replace_all(script, "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}", "<TIMESTAMP>")
  script <- str_replace_all(script, "\\d+(\\.\\d+)?\\s*(secs?|mins?|ms)", "<RUNTIME>")
  script
}

test_that("simulatR", {
  expect_snapshot(
    simulatR(c("lpr", "lmdb", "opr", "pato"),
             n = 20,
             pattern.list = list("lpr" = c("DX1", "DZ2"),
                                 "lmdb" = c("C10", "R10"),
                                 "opr" = c("KZ123"),
                                 "pato" = list("t.codes" = c("T123", "T234"),
                                               "m.codes" = c("M80")))),
                  transform = removR)
})
