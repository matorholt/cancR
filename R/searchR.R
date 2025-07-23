#' searchR
#'
#' @description
#' Function to obtain covariates or outcomes from the most common registers
#'
#'
#' @param reglist list of dataframe(s)
#' @param search.list list with list(label = diagnosis code) structure.
#' @param sub.list list of variables where the diagnosis code should be kept (e.g. list("KOL" = "subtype"))
#' @param sub.labels list of labels for the diagnosis code colum (e.g. list("subtype" = list("a" = c("DC1", "DC2"))))
#' @param exclusion vector of diagnosis codes for exclusion
#' @param slice which rows should be selected (first(default)/last/all)
#' @param format whether selected rows should contain date or 1
#' @param match the match of the regex code (match, start, end or contains(default))
#' @param casename the name of the case variable (default = "index")
#' @param pnr name of the pnr column
#'
#' @return A dataframe with the selected columns with corresponding diagnosis codes
#' @export
#'
#'


# dfs <- list(lpr = simAdmissionData(n=100),
#             opr = simAdmissionData(n=100) %>% rename(opr = diag),
#             lmdb = simPrescriptionData(n=100))
#
# dfs <- lapply(dfs, as.data.frame)
#
# clist <- decodR(list("lpr_case" = list("kidney" = c("DD", "DQ"),
#                                    "lung" = c("DF", "DM")),
#                      "lpr_ex" = list("immune" = "DZ",
#                                      "cll" = c("DB", "DN")),
#                      "lmdb_ex" = list("immune" = "C0"),
#                      "opr_ex" = list("sotr" = c("DT", "DG", "DK")),
#                      "labels" = list("lpr_case" = "SOTR",
#                                      "lpr_ex" = "immsup"),
#                      "exclusion" = c("DQ","ZZ2")))
#
#
#
# searchR(dfs,
#         clist$searchR.list,
#         sub.list = clist$searchR.keep,
#         sub.labels = clist$recodR.labels,
#         exclusion = clist$searchR.exclusion) %>%
#   group_by(SOTR) %>%
#   summarise(n = n())

searchR <- function(reglist,
                    search.list,
                    sub.list,
                    sub.labels = NULL,
                    exclusion = "NULL",
                    slice = "first",
                    format = "date",
                    match = "contains",
                    casename = "index",
                    pnr = pnr) {

  match <- match.arg(match, c("start", "end", "exact", "contains"))
  format <- match.arg(format, c("categorical", "date"))
  slice <- match.arg(slice, c("first", "last", "all"))

  if(class(reglist) == "data.frame") {
    reglist <- lst(reglist)
  }

  pnr_c <- reglist[[1]] %>% select(pnr) %>% names()

  reglist <- lapply(reglist, function(d) {
    colnames(d)[which(str_detect(colnames(d), "eksd|inddto"))] <- "date"
    colnames(d)[which(str_detect(colnames(d), "diag|opr|atc"))] <- "code"
    d
  })

  slist <- list()

  for(i in names(search.list)) {
    reg <- str_extract(i, "lpr|lmdb|opr")

    switch(match,
           "start" = {regex <- c("^(", ")")},
           "end" = {regex <- c("(", ")$")},
           "exact" = {regex <- c("^(", ")$")},
           "contains" = {regex <- c("(", ")")}
    )

    pattern <- paste0(regex[1], paste0(search.list[[i]], collapse="|"), regex[2])

    exclude <- paste0(regex[1], paste0(exclusion, collapse="|"), regex[2])


    data <- as.data.table(reglist[[reg]])[str_detect(code, pattern) & str_detect(code, exclude, negate=T)]

    if(format == "categorical") {
      data <- data[, c(i) := 1]
    } else {
      data <- data[, c(i) := date]
    }

    if(i %in% names(sub.list)) {

      for(j in sub.list[[i]]) {

        data <- data[, c(j) := code]
      }
    }

    switch(slice,
           "first" = {range <- 1},
           "last" = {range <- ".N"},
           "all" = {range <- "1:.N"})

    data <- data[, .SD[eval(parse(text=range))], by=c(pnr_c), .SDcols = c(i, sub.list[[i]])]

    slist[[i]] <- data
  }

  joined_data <- plyr::join_all(slist, by = "pnr", type = "full") %>%
    arrange(pnr) %>%
    rename_with(~ paste0(casename), contains("case"))

  if(!is.null(sub.labels)) {

    joined_data <- joined_data %>%
      recodR(sub.labels)

  }

  joined_data

}
