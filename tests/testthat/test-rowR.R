#Removal of timestamps
removR <- function(script) {
  script <- str_replace_all(script, "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}", "<TIMESTAMP>")
  script <- str_replace_all(script, "\\d+(\\.\\d+)?\\s*(secs?|mins?|ms)", "<RUNTIME>")
  script
}

df <- data.frame(x=c(NA,2,3, NA,NA),
                 y=c(1,2,5,3,NA),
                 z=c(0,4,7,4,NA),
                 z_z=c(10,10,10,NA,NA))

test_that("rowR, pmin/pmax", {
  r1 <-
    df %>%
    rowR(c(x,y,z), "pmax", na.rm=T, label = "labs") %>%
    rowR(c(x,y,z), "pmin", na.rm=F) %>%
    rowR(c(x,y,z), "sum")

  expect_identical(r1,
               structure(list(x = c(NA, 2, 3, NA, NA), y = c(1, 2, 5, 3, NA),
                              z = c(0, 4, 7, 4, NA), z_z = c(10, 10, 10, NA, NA),
                              labs = c(1, 4, 7, 4, NA),
                              pmin = c(NA, 2, 3, NA, NA),
                              sum = c(1, 8, 15, 7, 0)), row.names = c(NA, -5L), class = "data.frame"))
})

test_that("rowR,NA", {
  expect_identical(
    df %>%
      rowR(type = "any.na", label = "any.na_all") %>%
      rowR(vars = c(y,z), type = "any.na", label = "any.na_yz") %>%
      rowR(type = "all.na") %>%
      rowR(type = "sum.na")
    ,
    structure(list(x = c(NA, 2, 3, NA, NA), y = c(1, 2, 5, 3, NA),
                   z = c(0, 4, 7, 4, NA), z_z = c(10, 10, 10, NA, NA), any.na_all = c(1L,
                                                                                      0L, 0L, 1L, 1L), any.na_yz = c(0L, 0L, 0L, 0L, 1L), all.na = c(0L,
                                                                                                                                                     0L, 0L, 0L, 0L), sum.na = c(1, 0, 0, 2, 4)), row.names = c(NA,
                                                                                                                                                                                                                -5L), class = "data.frame")
  )
})

test_that("rowR, NA2", {
  expect_identical(
    df %>%
      rowR(type = "any.na", filter = "remove")
    ,
    structure(list(x = c(2, 3), y = c(2, 5), z = c(4, 7), z_z = c(10,
                                                                  10)), row.names = c(NA, -2L), class = "data.frame")
  )
})

test_that("rowR, NA2", {
  expect_snapshot(
    df %>%
      rowR(vars = c(y:z_z), type = "any.na", filter = "remove"))
})

test_that("rowR, leftright", {
  expect_snapshot(df %>% rowR(type = "fill", direction = "leftright"),
                  transform = removR)
})

test_that("rowR, paste", {
  expect_snapshot(df %>%
                  rowR(vars = c(x,y), type = "paste", collapse = "|"),
                  transform = removR)
})


