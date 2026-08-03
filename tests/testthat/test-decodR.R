removR <- function(script) {
  script <- str_replace_all(script, "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}", "<TIMESTAMP>")
  script <- str_replace_all(script, "\\d+(\\.\\d+)?\\s*(secs?|mins?|ms)", "<RUNTIME>")
  script
}

codelist_matching <- list(
  case = list("lpr" = c("DA", "DB"),
              "pato" = "M(83)"),
  lpr = list("immun_lpr" = c("DC", "DD", "DE"),
             "leukemia" = c("DF", "DG")),
  lmdb = list("immun_atc" = c("A","B"),
              "chemo" = "G"),
  opr = list("immun_opr" = c("KA","KB")),
  design = list(age.limit = 18,
                period = c("2000-01-01", "2022-12-31"),
                exclusion = c("sc_date"),
                matching = "all")

)

test_that("decodR, t1", {
  expect_snapshot(decodR(codelist_matching),
                  transform = removR)
})
