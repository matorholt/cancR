#' Combiner
#'
#' @description
#' All possible combinations of a single vector (ABC: ABC, ACB, BAC, BCA, CAB, CBA) or two separate vectors (AB, CD: ABCD, CDAB)
#' The function does not allow for replacements.
#'
#' @param letters Vector either of length 1 (will be split for each subelement) og length >1
#' @param letters2 Optional second vector if chunks are to be combined
#' @param list Whether all combinations should be returned as a list (useful for looping with lapply)
#'
#' @return Returns all possible combinations of the input vector(s)
#' @export
#'
#'
combiner <- function(letters, letters2=NULL, list=F) {
  #Convenience split if only one combination is provided
  if(length(letters) == 1 & is.null(letters2)) {
    letters <- unlist(stringr::str_split(letters, ""))
  }
  #Assumes single vectorif two vectors both of length 1 is provided (e.g. "AB", "CD" -> c("AB", "CD"))
  if(length(letters2) == 1 ) {
    letters <- as.vector(c(letters, letters2))
    letters2 <- NULL
  }
  #If two vectors are provided, all possible combinations are found
  if(!is.null(letters2)) {
    as.vector(apply(expand.grid(letters, letters2), 1, function(x) paste0(x, collapse="")))
    #If only one vector is provided, all possible unique combinations are found
  } else if(!list){
    as.vector(na.omit(apply(expand.grid(mget(rep("letters", length(letters)))), 1, function(x) ifelse(length(unique(x)) == length(letters), paste0(x, collapse=""), NA ))))
  } else {
    (str_split(as.vector(na.omit(apply(expand.grid(mget(rep("letters", length(letters)))), 1, function(x) ifelse(length(unique(x)) == length(letters), paste0(x, collapse=";"), NA )))), ";"))
  }
}

