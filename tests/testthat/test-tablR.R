removR <- function(script) {
  script <- str_replace_all(script, "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}", "<TIMESTAMP>")
  script <- str_replace_all(script, "\\d+(\\.\\d+)?\\s*(secs?|mins?|ms)", "<RUNTIME>")
  script
}


test_that("tablR, t1", {
  expect_snapshot(tablR(population_denmark,
                        group = sex,
                        vars = c(age_group, population)),
                  transform = removR)
})

test_that("tablR, t2", {
  expect_snapshot(redcap_df %>%
                    mutate(margins = sample(c("0","1"), nrow(redcap_df), replace=TRUE),
                           w = runif(nrow(redcap_df), 1,5)) %>%
                    #mutate(type = ifelse(row_number() == 1, NA, type)) %>%
                    factR(c(type, sex, localisation, cd10, sox10, ck, margins, necrosis)) %>%
                    tablR(group=type,
                          numeric = c("meansd","range"),
                          vars = c(age, sex, localisation, cd10, sox10, ck, necrosis, margins),
                          labs.groups = list("type" = list("Benign" = "0",
                                                           "In situ" = "1",
                                                           "Malignant" = "2")),
                          reverse = T,
                          labs.headings = list("Age at Debut" = "age",
                                               "gender" = "sex",
                                               "Cluster of diff 10" = "cd10",
                                               "SOX10" = "sox10"),
                          labs.subheadings = list("sex" = list("Female" = "2",
                                                               "Male" = "1"),
                                                  "localisation" = list("Neck" = "0",
                                                                        "Head" = "1",
                                                                        "Trunk" = "2",
                                                                        "Upper Extremity" = "3",
                                                                        "Lower Extremity" = "4",
                                                                        "Unspecified" = "5")),
                          reference = list("sex" = c("male")),
                          #simplify = c("necrosis", "margins"),
                          # simplify=list("Immunohistochemistry" = c("cd10", "sox10", "ck"),
                          #               "Tumor" = c("necrosis", "margins")),
                          simplify=list("Immunohistochemistry" = c("cd10", "sox10", "ck"),
                                        "necrosis",
                                        "margins"),
                          print=F,
                          weights = w),
                  transform = removR)
})




