removR <- function(script) {
  script <- str_replace_all(script, "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}", "<TIMESTAMP>")
  script <- str_replace_all(script, "\\d+(\\.\\d+)?\\s*(secs?|mins?|ms)", "<RUNTIME>")
  script
}

reverse_list <- list("first" = "a1",
                     "second" = "b2")

test_that("listR, t1", {
  expect_snapshot(listR(reverse_list, type = "reverse"),
                  transform = removR)
})


vec <- letters[1:5]


test_that("listR, t2", {
  expect_snapshot(listR(vec, type = "vec2list"),
                  transform = removR)
})


peel_list <- list(lpr = list(exclusion = list(immun = "a",
                                              other = "b",
                                              test = "c"),
                             covariates = list(immun = "d",
                                               endo = "e")))

test_that("listR, t3", {
  expect_snapshot(listR(peel_list, "peel", layer = 2, collapse = T),
                  transform = removR)
})





