#' decodR
#'
#' @description
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

# codelist <- list("lpr_case" = list("kidney" = c("a1","b1","c1"),
#                                    "lung" = c("a2", "b2", "c2")),
#                  "lpr_ex" = list("immune" = "a3",
#                                  "cll" = c("a4", "b4")),
#                  "lmdb_ex" = list("immune" = "a5"),
#                  "opr_ex" = list("trans" = "t5"),
#                  "labels" = list("lpr_case" = "SOTR",
#                                  "lpr_ex" = "immsup"),
#                  "exclusion" = c("z1","z2"))
#
# (clist <- decodR(codelist))


decodR <- function(codelist) {

  clist <- codelist

  codelist <- codelist[str_detect(names(codelist), "lpr|lmdb|opr|pato")]

  #Registies
  registries <- unique(str_extract(names(codelist), "lpr|opr|lmdb|cancer|pato"))

  #LoadR + searchR
  loadlist <- list()
  searchlist <- list()

  for(i in registries) {

    #loadlist[[i]] <- c(unlist(codelist[str_detect(names(codelist), i)]), use.names=F)
    loadlist[[i]] <- c(unlist(codelist[str_detect(names(codelist), i)]), use.names=F)

    for(j in names(codelist)[str_detect(names(codelist), i)]) {

      searchlist[[j]] <-c(unlist(codelist[[j]]), use.names=F)

    }

  }

  list <- list(
    codes = clist,
    loadR.regs = c(registries, "pop", "sc", "meta", "dsd"),
    loadR.list = loadlist,
    searchR.list = searchlist
  )

  if("labels" %in% names(clist)) {
    list <- append(list, list(searchR.keep = clist[["labels"]]))

    labs <-
      clist[c(names(clist[["labels"]]))] %>% set_names(clist[["labels"]])

    list <- append(list, list(recodR.labels = labs))

  }

  if("exclusion" %in% names(clist)) {

    list <- append(list, list(searchR.exclusion = clist[["exclusion"]]))
  }

  if("lpr_case" %in% names(clist)) {

  list <- append(list, list(updatR.exclusion = unlist(codelist[str_detect(names(codelist), "lpr_case")], use.names = F)))

  }

  list
}
