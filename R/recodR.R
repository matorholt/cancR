#' recodR
#'
#' @description
#' Simultaneous recoding of multiple variables. Works with piping and
#'
#'
#' @param data dataframe
#' @param namelist list og variables with named vector(s) containing diagnosis codes (e.g. list(var1 = list(name = diagnosis codes)))
#' @param match Whether the provided diagnosis codes should be matched exactly, start with/end with or contain (default)
#'
#' @return the input data frame with recoded variables
#' @export
#'
#'

# set.seed(1)
# df <-
#   data.frame(diag = sample(c("DC123", "DC234", "DG123"), 10, replace=TRUE),
#              type = sample(c("DY", "DY234", "DY123"), 10, replace=TRUE),
#              type2 = sample(c("DC123", "DC234", "DG123"), 10, replace=TRUE))
#
#
# df %>%
#   recodR(list("diag" = list("KOL" = "DG123",
#                          "Astma" = "DC123",
#                          "AMI" = list("DC234", "DC235")),
#               "type" = list("Cancer" = "DY")),
#          match = "exact")


recodR <- function(data, namelist, match = "contains") {

  match <- match.arg(match, c("start", "end", "exact", "contains"))

  if(class(namelist) != "list") {
    return(cat("ERROR: The variable names are not provided as a list"))
  }

  #Seq through variables (1st order in namelist)
  for(i in names(namelist)) {

    #Remove number from vectors with length > 1
    uninames <- unique(str_remove(names(namelist[[i]]), "\\d$"))

    #Seq through names (2nd order i namelist)
    for(j in uninames) {

      pattern <- paste0(namelist[[i]][str_detect(names(namelist[[i]]), j)], collapse="|")

      switch(match,
             "start" = {pattern <- paste0("^", "(", pattern, ")")},
             "end" = {pattern <- paste0("(", pattern, ")$")},
             "exact" = {pattern <- paste0("^(", pattern, ")$")}
      )

      data[,i][str_detect(data[[i]], pattern)] <- j

    }

  }

  return(data)

}

