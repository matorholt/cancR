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
#                  "lmdb_ex" = list("immune" = "a5"))
#
# decodR(codelist)


decodR <- function(codelist) {

  #Registies
  registries <- unique(str_extract(names(codelist), "lpr|opr|lmdb|cancer|pato"))

  #LoadR + searchR
  loadlist <- list()
  searchlist <- list()

  for(i in registries) {

    #loadlist[[i]] <- c(unlist(codelist[str_detect(names(codelist), i)]), use.names=F)
    loadlist[[i]] <- c(unlist(codelist[str_detect(names(codelist), i)]), use.names=F)

    for(j in names(codelist)[str_detect(names(codelist), i)]) {

      searchlist[[i]][[j]] <-c(unlist(codelist[[j]]), use.names=F)

    }

  }

  #Regexes
  regex <- list()

  for(i in names(codelist)) {

    regex[[i]] <- lapply(codelist[[i]], function(x) {
      paste0(x, collapse="|")

    })

  }


  lst(
    codes = codelist,
    regs = registries,
    loadR = loadlist,
    searchR = searchlist,
    regex = regex
  )

}
