removR <- function(script) {
  script <- str_replace_all(script, "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}", "<TIMESTAMP>")
  script <- str_replace_all(script, "\\d+(\\.\\d+)?\\s*(secs?|mins?|ms)", "<RUNTIME>")
  script
}

pato <- tribble(
  ~ pnr, ~ date,   ~ snomed,
  #Three unique tumors and one
  1, "2012-01-01", "T02120 M80703 M84003",
  1, "2012-02-01", "T02121 M80704 M80003",
  1, "2012-08-31", "T29030 M80706",
  1, "2013-04-04", "T0220B M80903",
  1, "2013-05-04", "T0220B M87003",
  1, "2013-06-04", "T29030 M80906",
  1, "2016-01-01", "T0282D TY9000 M80703",
  1, "2017-01-01", "T0282C M80703",
  #Single tumor
  2, "2012-01-01", "T0282C M87003",
  #One skin cancer, one competing cancer and three metastases
  3, "2012-01-01", "T0282C M80703",
  3, "2015-01-01", "T29030 M80703" ,
  3, "2016-01-01", "T29030 M80706" ,
  3, "2016-03-01", "T0848F M80706",
  3, "2016-04-01", "T29031 M80706",
  #One skin cancer, change in diagnosis BCC -> MM
  4, "2013-04-04", "T0220B M80903",
  4, "2013-05-01", "T0220B M87003",
  #One skin cancer, change in diagnosis UNS -> Sarcoma + UNS recurrence
  5, "2001-01-01", "T0220B M88003",
  5, "2001-03-01", "T0220B M88903",
  5, "2005-01-01", "T0220B M88003",
  #One skin cancer, specific to unspecific shift
  6, "2001-01-01", "T0282C M80503",
  6, "2001-02-01", "T0282C M80703",
  #Different sarcomas to test m88 vs. m8830, m8832, m889
  7, "2001-01-01", "T0282C M88303",
  7, "2003-02-01", "T0220B M88323",
  7, "2006-02-01", "T02120 M88903",
  7, "2009-02-01", "T02120 M88933",
  # Skin+Ear < 90
  8, "2001-01-01", "T01000 M80503",
  8, "2001-02-01", "T0220B M80503",
  # Skin+Ear > 90
  9, "2001-01-01", "T01000 M80503",
  9, "2003-02-01", "T0220B M80503",
  # Ear+Skin < 90
  10, "2001-01-01", "T0220B M80503",
  10, "2001-02-01", "T01000 M80503",
  # Ear+Skin > 90
  11, "2001-01-01", "T0220B M80503",
  11, "2003-02-01", "T01000 M80503",
  # Skin+Skin < 90 - BCC
  12, "2001-01-01", "T01000 M80903",
  12, "2001-02-01", "T01000 M80903",
  # Skin+Skin > 90 - BCC
  13, "2001-01-01", "T01000 M80903",
  13, "2003-02-01", "T01000 M80903",
  # Skin+Skin < 90 - PCC
  14, "2001-01-01", "T00100 M80703",
  14, "2001-02-01", "T01000 M80703",
  # Skin+Skin > 90 - PCC
  15, "2001-01-01", "T01000 M80703",
  15, "2003-02-01", "T01000 M80703",
  # Ear+leg+Skin > 90 - PCC
  16, "2001-01-01", "T0220B M80703",
  16, "2003-02-01", "T02121 M80703",
  16, "2005-02-01", "T01000 M80703",
  # Only mets - out
  17, "2001-01-01", "T0220B M80706",
  17, "2003-02-01", "T02121 M80706",
  17, "2005-02-01", "T01000 M80706",
  # BCC and metastasizing PCC. Change in first metastasis code from BCC to PCC. Multiple coupled metastasis + one late
  18, "2000-01-01", "T0220B M80903",
  18, "2001-01-01", "T0220B M80703",
  18, "2003-01-01", "T29030 M80906",
  18, "2003-02-01", "T29030 M80706",
  18, "2003-02-05", "T08000 M80706",
  18, "2004-01-01", "T02121 M80706",
  18, "2004-02-01", "T0220B M80706",
  18, "2003-03-02", "T10501 M80706",
  18, "2006-03-02", "T10503 M80706",
  18, "2015-01-01", "T02121 M80703",
  # Patient with two primary PCCS and unallocable met
  19, "2001-01-01", "T0220B M80703",
  19, "2001-02-01", "T02121 M80703",
  19, "2001-07-01", "T0810S M80706"
) %>%
  datR(date)

tumor_list <- list("pcc" = c("m807", "m805"),
                   "bcc" = "m809",
                   "mm" = "m87",
                   #"sarcoma" = "m88",
                   "ups" = "m8830",
                   "dfsp" = "m8832",
                   "lms" = "m889[02]"
)



test_that("tumR, t1", {
  expect_snapshot(tumR(pato,
                       tumor = tumor_list,
                       verbose = F,
                       loc.exact = F),
                  transform = removR)
})

