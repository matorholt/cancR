df <- data.frame(id=c(1,1,1,1,2,2,2,3,3,3,4,4,4,4),
                 x=c(5,5,6,6,7,8,9,1,2,1,4,1,2,4))

test_that("rollR, default", {

  expect_equal(df %>%
                     arrange(x) %>%
                     rollR(),
               structure(list(id = c(3, 3, 4, 3, 4, 4, 4, 1, 1, 1, 1, 2, 2, 2),
                              x = c(1, 1, 1, 2, 2, 4, 4, 5, 5, 6, 6, 7, 8, 9),
                              grp = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L)), row.names = c(NA, -14L), class = "data.frame"))

})

test_that("rollR, count", {

  r2 <- df %>%
    rollR(by = id, type = "count")

  expect_equal(r2,
               structure(list(id = c(1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4,
                                     4), x = c(5, 5, 6, 6, 7, 8, 9, 1, 2, 1, 4, 1, 2, 4), grp = c(1L,
                                                                                                  2L, 3L, 4L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 4L)), row.names = c(NA,
                                                                                                                                                                      -14L), class = "data.frame"))



})
