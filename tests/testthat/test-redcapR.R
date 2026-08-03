removR <- function(script) {
  script <- str_replace_all(script, "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}", "<TIMESTAMP>")
  script <- str_replace_all(script, "\\d+(\\.\\d+)?\\s*(secs?|mins?|ms)", "<RUNTIME>")
  script
}

cpr <- readR("../../Atypical fibroxanthoma/Surgical risk factors of AFX recurrence/Statistics/CPRLIST.csv") %>%
  select(id, cpr)
raw <- readR("../../Atypical fibroxanthoma/Surgical risk factors of AFX recurrence/Statistics/data/afx_data_16.09.2025.csv")
raw <- raw %>%
  filter(study_id %nin% raw$study_id[raw$study_id %nin% cpr$id])
dict <- readR("../../Atypical fibroxanthoma/Surgical risk factors of AFX recurrence/Statistics/data/data_dict.csv")



# test_that("redcapR, t1", {
#   expect_snapshot(redcapR(raw,
#                           dict,
#                           namelist = list("name" = list("n1" = "1",
#                                                         "n2" = "2",
#                                                         "n3" = "3",
#                                                         "n4" = "4")),
#                           formatlist = list("pos" = "Positive",
#                                             "neg" = "Negativ"),
#                           cprlist = cpr,
#                           index = datesurg),
#                   transform = removR)
# })


