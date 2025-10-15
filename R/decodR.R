#' Decoding of the main codelist for loading and searching in registries
#'
#'
#' @param codelist List of lists with registries, diagnosis codes and labels. See example.
#'
#' @return
#' codes: Original codelist without modification \cr
#' regs: The specified registries for the project. First input in loadR \cr
#' loadR: The pattern.list in loadR \cr
#' searchR: The pattern.list in searchR \cr
#' regex: The codelist converted to regex-expressions with | as separator
#' @export
#'
#'

# codelist <- list("lpr_case" = list("abdomen" = list("kidney" = c("tx1a","tx1b","tx1c"),
#                                                    "liver" = c("tx2a", "tx2b", "tx2c")),
#                                    "thorax" = list("heart" = c("tx3a", "tx3b", "tx3c"),
#                                                    "lung" = c("tx4a", "tx4b", "tx3c"))),
#                  "lpr_ex" = list("immune_diag" = "a3",
#                                  "cll" = c("a4", "b4")),
#                  "lmdb_ex" = list("immune_drugs" = "a5"),
#                  "opr_ex" = list("trans" = "t5"),
#                  "pato_supp" = list("PCC" = "M80"),
#                  "labels" = list("lpr_case" = c("SOTR", "region"),
#                                  "lpr_ex" = "immsup"),
#                  "exclusion" = c("z1","z2"))
#
# clist <- decodR(codelist)





decodR <- function(codelist,
                   regs = c("pop", "sc", "meta", "dsd")) {

  if(any(str_detect(names(codelist), "cases"))) {
    return(cat("ERROR: Change suffix to case instead of cases"))
  }

  if(sum(str_detect(names(codelist), "case")) > 1) {
    return(cat("ERROR: Only one list can be named case"))
  }

  if(sum(str_detect(names(flatten(codelist)), "case$")) > 1) {
    return(cat("ERROR: Multiple labels named case. Add _header as suffix"))
  }

  #Copy
  main <- codelist

  c_lab <- names(codelist)[str_detect(names(codelist), "case")]

  #Flatten list
  cl_melt <- rrapply::rrapply(codelist, how = "melt")

  #Fill
  codelist <-
    bind_cols(cl_melt[, 1:ncol(cl_melt)-1] %>%
                rowR(type = "fill", direction = "right"),
              cl_melt[ncol(cl_melt)]) %>%
    select(1, (ncol(.)-1), ncol(.)) %>%
    rrapply::rrapply(how = "unmelt")

  #Registies
  registries <- unique(str_extract(names(codelist)[str_detect(names(codelist), "lpr|opr|lmdb|cancer|pato")], "lpr|opr|lmdb|cancer|pato"))

  #LoadR + searchR
  loadlist <- list()
  searchlist <- list()

  for(i in registries) {

    loadlist[[i]] <- c(unlist(codelist[str_detect(names(codelist), i)]), use.names=F)

    for(j in names(codelist)[str_detect(names(codelist), i)]) {

      searchlist[[j]] <-c(unlist(codelist[[j]]), use.names=F)

    }

  }

  #Exclusions
  ex_list <- unique(str_extract(names(codelist)[str_detect(names(codelist), "_ex")], ".*_ex"))

  list <- list(
    main = main,
    loadR.regs = c(registries, regs),
    loadR.list = loadlist,
    searchR.list = searchlist,
    includR.exclude = ex_list
  )

  if("labels" %in% names(codelist)) {

    list <- append(list, list(searchR.keep = codelist[["labels"]]))

    lab_list <- list()

    for(n in names(codelist[["labels"]])) {

      lab_df <- cl_melt[cl_melt[[1]] == n,]

      levels <- codelist[["labels"]][[n]]

      for(l in seq_along(levels)) {

        sub_df <- lab_df[, c(l+1,ncol(lab_df))]

        lab_list[[levels[l]]] <-
          rrapply::rrapply(sub_df %>%
                             unnest(value) %>%
                             group_by(!!sym(colnames(sub_df)[1])) %>%
                             summarise(value = list(value)),
                           how = "unmelt")

      }

    }

    list <- append(list, list(recodR.labels = lab_list))

  }

  if("exclusion" %in% names(codelist)) {

    list <- append(list, list(searchR.exclusion = codelist[["exclusion"]]))
  }

  if("lpr_case" %in% names(codelist)) {

    if(any(unlist(codelist[["lpr_case"]]) %in% unlist(charlson.codes))) {

      cat("Cases have diagnoses codes as a part of the Charlson Comorbidity Index - remember to use the updatR() function")


      list <- append(list, list(updatR.exclusion = unlist(codelist[str_detect(names(codelist), "lpr_case")], use.names = F)))

    }

  }

  print(viewR(main))

  list



}

