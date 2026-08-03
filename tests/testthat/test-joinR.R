removR <- function(script) {
  script <- str_replace_all(script, "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}", "<TIMESTAMP>")
  script <- str_replace_all(script, "\\d+(\\.\\d+)?\\s*(secs?|mins?|ms)", "<RUNTIME>")
  script
}

df_list <- list(data.frame(id = c(1,2,3),
                           pnr = c(1,2,3),
                           x = c(1,1,2),
                           v1 = c(1,1,2)),
                data.frame(id = c(2,3,3),
                           pnr2 = c(2,3,3),
                           x2 = c(1,1,2),
                           v1 = c(1,1,2),
                           y = c(2,2,2)),
                data.frame(id = c(2,4,4),
                           pnr3 = c(2,4,4),
                           x3 = c(1,3,3),
                           v1 = c(1,3,3),
                           z = c(3,3,3)))

df_2 <- data.frame(id = c(1,2,3),
                   pnr = c(11,22,33),
                   x = c(11,11,22),
                   v1 = c(11,11,12))


test_that("joinR, t1", {
  expect_snapshot(joinR(df_list, by = "id", type = "left", dt=T),
                  transform = removR)
})

test_that("joinR, t2", {
  expect_snapshot(joinR(df_list[[1]], df_list[[3]], by=c("id", "v1")),
                  transform = removR)
})

test_that("joinR, t3", {
  expect_snapshot(joinR(df_list, by = c("id", "v1"), type = "left", dt=T),
                  transform = removR)
})

test_that("joinR, t4", {
  expect_snapshot(joinR(df_list, by = list(c("pnr", "pnr2", "pnr3")), type = "full"),
                  transform = removR)
})

test_that("joinR, t5", {
  expect_snapshot(joinR(df_list, by = list(c("pnr", "pnr2", "pnr3"),
                                           c("x", "x2", "x3")), type = "left", dt=T),
                  transform = removR)
})

test_that("joinR, t6", {
  expect_snapshot(joinR(df_list, as.data.table(df_2), by = "id"),
                  transform = removR)
})



