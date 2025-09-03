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
#
# set.seed(1)
# dfs <- list(lpr = simAdmissionData(n=100, m=10),
#             opr = simAdmissionData(n=100, m=10) %>% rename(opr = diag),
#             lmdb = simPrescriptionData(n=100))
#
# dfs <- lapply(dfs, as.data.frame)
#
# clist <- decodR(list("lpr_case" =
#                        list(supergroup_a =
#                               list(group_a1 = list("sg1" = c("DB6", "DB7"),
#                                                   "sg2" = c("DD22", "DD23")),
#                                    group_b1 = list("sg3" = c("DD4"))),
#
#                             supergroup_b =
#                               list(group_a2 = list("sg4" = c("DE5", "DF"),
#                                                    "sg5" = c("DF", "DG"))),
#
#                             supergroup_c =
#                               list(group_a3 = list("sg6" = c("DJ", "DK"),
#                                                    "sg7" = c("DL", "DM")),
#                                    group_b3 = list("sg8" = c("DN")))),
#
#                      "lpr_ex" = list("e1" = c("DO", "DP"),
#                                      "c2" = c("DR", "DQ")),
#                      "lmdb_ex" = list("immune" = "C0"),
#                      "opr_ex" = list("sotr" = c("DT", "DG", "DK")),
#                      "labels" = list("lpr_case" = c("sg_level", "g_level", "sub_level"),
#                                      "lpr_ex" = "immsup"),
#                      "exclusion" = c("DQ","ZZ2")))
#
#
# t <- searchR(dfs,
#         clist$searchR.list,
#         sub.list = clist$searchR.keep,
#         sub.labels = clist$recodR.labels,
#         exclusion = clist$searchR.exclusion)

searchR <- function(reglist,
                    search.list,
                    sub.list = list(),
                    sub.labels = NULL,
                    exclusion = "NULL",
                    slice = "first",
                    format = "date",
                    match = "start",
                    casename = "index",
                    pnr = pnr) {

  tickR()

  start <- tickR.start

  cat(paste0("\nInitializing searchR algorithm: ", tockR("time"), "\n\n"))

  match <- match.arg(match, c("start", "end", "exact", "contains"))
  format <- match.arg(format, c("categorical", "date"))
  slice <- match.arg(slice, c("first", "last", "all"))

  if(class(reglist) == "data.frame") {
    reglist <- lst(reglist)
  }

  if(sum(str_detect(names(search.list), "case")) > 1) {

    return(cat("Only one list can be named case. Change to other suffix, e.g. pato_supp"))

  }

  pnr_c <- reglist[[1]] %>% select(pnr) %>% names()

  reglist <- lapply(reglist, function(d) {
    colnames(d)[which(str_detect(colnames(d), "eksd|inddto"))] <- "date"
    colnames(d)[which(str_detect(colnames(d), "diag|opr|atc|snomed"))] <- "code"
    d
  })

  slist <- list()

  for(i in names(search.list)) {
    reg <- str_extract(i, "lpr|lmdb|opr|pato")

    tickR()

    cat(paste0(reg, ": "))

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

    cat(paste0("Completed - ", tockR("time"), ", runtime: ", tockR("diff"), "\n"))

  }

  joined_data <- plyr::join_all(slist, by = "pnr", type = "full") %>%
    arrange(pnr) %>%
    rename_with(~ paste0(casename), contains("case"))

  if(!is.null(sub.labels)) {

    joined_data <- joined_data %>%
      recodR(sub.labels)

  }

  cat("\nSearching complete!\n")
  cat("Total runtime: \n")
  cat(tockR("diff", start), "\n\n")

  joined_data

}

