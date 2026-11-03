#Test data
x <- data.table(
  id    = c(1L, 2L, 3L, 3L),
  grp   = c("g1", "g1", "g1", "g2"),
  name  = c("a", "b", "c", "d"),
  score = c(10, NA, 30, NA)
)

y <- data.table(
  id     = 2:4,
  grp    = c("g1", "g2", "g1"),
  city   = c("X", "Y", "Z"),
  active = c(TRUE, FALSE, TRUE)
)

new_rows <- data.table(
  id    = 2:4,
  name  = c("b2", "c2", "d"),
  score = c(20, 35, 40)
)

new_rows_grp <- data.table(
  id    = 2:4,
  grp   = c("g1", "g2", "g1"),
  name  = c("b2", "c2", "d"),
  score = c(20, 35, 40)
)

z <- data.table(pnr = c(1L, 3L, 5L), region = c("N", "S", "E"))

df_list <- list(
  data.table(id2 = 1:3,           grp = c("g1", "g1", "g2"), val_a = c("a1", "a2", "a3")),
  data.table(pnr = c(1L, 2L, 4L), grp = c("g1", "g2", "g1"), val_b = c("b1", "b2", "b4")),
  data.table(id  = c(1L, 3L, 4L), grp = c("g1", "g2", "g1"), val_c = c("c1", "c3", "c4"))
)

# --- Basic joins -------------------------------------------------------
test_that("basic joins", {
  expect_snapshot(joinR(x, y, by = id, type = left))
  expect_snapshot(joinR(x, y, by = c(id, grp), type = left))
  expect_snapshot(joinR(x, y, by = id, type = full))
  expect_snapshot(joinR(x, y, by = c(id, grp), type = inner))
})

# --- Filter joins ------------------------------------------------------
test_that("filter joins", {
  expect_snapshot(joinR(x, y, by = id, type = semi))
  expect_snapshot(joinR(x, y, by = c(id, grp), type = anti))
})

# --- Update / insert joins ---------------------------------------------
test_that("insert and update joins on id only", {
  expect_snapshot(joinR(x, new_rows, by = id, type = insert))
  expect_snapshot(joinR(x, new_rows, by = id, type = update))
})

test_that("update joins only the columns in `vars`", {
  expect_snapshot(joinR(x, new_rows, by = id, type = update, vars = score))
})

test_that("insert and update joins on id and grp", {
  expect_snapshot(joinR(x, new_rows_grp, by = c(id, grp), type = insert))
  expect_snapshot(joinR(x, new_rows_grp, by = c(id, grp), type = update))
  expect_snapshot(joinR(x, new_rows_grp, by = c(id, grp), type = update, vars = score))
})

# --- Different key names -----------------------------------------------
test_that("keys with different names", {
  expect_snapshot(joinR(x, z, by = "id = pnr", type = left))
})

test_that("joining a list of data.tables", {
  expect_snapshot(joinR(x, df_list, by = c("id = id2 = pnr = id"), type = left))
  expect_snapshot(
    joinR(x, y, df_list,
          by = list(c("id", "id", "id2", "pnr", "id"), "grp"),
          type = "left")
  )
})
