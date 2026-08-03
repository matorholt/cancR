removR <- function(script) {
  script <- str_replace_all(script, "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}", "<TIMESTAMP>")
  script <- str_replace_all(script, "\\d+(\\.\\d+)?\\s*(secs?|mins?|ms)", "<RUNTIME>")
  script
}


n=10
df <-
  data.frame(
    id = seq(1,n),
    opdate = c(rep("2000-01-01", n-1), "2030-01-01"),
    follow = rep("2025-01-01", n),
    recurrence_date = c(NA, "2005-01-01", NA, NA, NA, "2005-01-01", "2005-01-01", NA, "1995-01-01", "2005-01-01"),
    metastasis_date = c(NA, NA, "2007-01-01", NA, NA, "2006-01-01", "2005-01-01", NA, NA, NA),
    dsd_date = c(NA,NA, "2008-01-01", "2009-01-01", NA, NA, NA, NA, NA, NA),
    death_date = c(NA, NA, "2008-01-01", "2009-01-01", NA, "2010-01-01", "2010-01-01", "2024-01-01", "2019-01-01", "1999-01-01"),
    second_date = c(NA, NA, NA, NA, "2008-01-01", NA, "2001-01-01", NA, NA, NA)) %>%
  datR(c(opdate:second_date))

df <- df[c(1:(n-1)), ]

df <- df[c(1:(n-2)), ]

# test_that("structR, t1", {
#   expect_snapshot(structR(df,
#                           index = opdate,
#                           fu = follow,
#                           outcomes=c(recurrence_date, metastasis_date),
#                           competing = c(death_date, second_date),
#                           composite = list("pfs" = list("outcomes" = c("recurrence_date", "metastasis_date", "death_date")),
#                                            "relapse" = list("outcomes" = c("recurrence_date", "metastasis_date", "dsd_date"),
#                                                             "competing" = c("death_date")),
#                                            "test" = list("outcomes" = c("metastasis_date"),
#                                                          "competing" = c("recurrence_date", "death_date"))),
#                           keep.dates = T,
#                           check = T,
#                           remove = T),
#                   transform = removR)
# })

