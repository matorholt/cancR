test_that("defusR", {

  test_c <- defusR(c(test, best))

  defusR(test_c)

  myfun <- function(data, vars) {

    vars_c <- defusR(vars)

    #Basic usage
    df_vars <- redcap_df[, vars_c]

    redcap_dt <- as.data.table(redcap_df)

    dt_keep <- redcap_dt[, .SD, .SDcols = vars_c]

    dt_remove <- redcap_dt[, c(vars_c) := NULL]

    #Nested functions
    row_fun <- rowR(redcap_df, vars = vars_c, type = "sum")
    roll_fun <- rollR(redcap_df, type = "count", order = vars_c)

    return(list(test_c, df_vars, dt_keep, dt_remove, row_fun, roll_fun))
  }

  expect_snapshot(myfun(redcap_df, vars = c(type:necrosis)))

})
