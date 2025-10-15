#' Recode multiple variables
#'
#'
#' @param data dataframe
#' @param namelist list og variables with named vector(s) containing diagnosis codes (e.g. list(var1 = list(name = diagnosis codes)))
#' @param match Whether the provided diagnosis codes should be matched exactly, start with/end with, be bounded by or contain (default)
#'
#' @return the input data frame with recoded variables
#' @export
#'
#'

# set.seed(1)
# df <-
#   data.frame(diag = sample(c("DX123", "DC123", "DC234", "DG123", "DG234"), 20, replace=TRUE),
#              type = sample(c("DY", "DY234", "DY123"), 20, replace=TRUE),
#              type2 = sample(c("DC123", "DC234", "DG123"), 20, replace=TRUE),
#              split = c("1,11", "2,10", "2,4", "2,15"))
#
# df %>%
#   recodR(list("diag" = list("KOL" = "DX123",
#                             "Astma" = c("DC123", "DC2"),
#                             "AMI" = list("DG123", "DG234"))),
#          match = "exact")
#
# df %>%
#   factR(c(diag, type, type2)) %>%
#   recodR(list("diag" = list("KOL" = "DX123",
#                             "Astma" = c("DC123", "DC234"),
#                             "AMI" = list("DG123", "DG234"))),
#          match = "exact")
#
# df %>%
#   recodR(list("split" = list("one" = "1",
#                              "four" = "4",
#                              "ten" = "10",
#                              "fifteen" = "15")),
#          match = "boundary")


recodR <- function(data, namelist, match = "contains") {

  match <- match.arg(match, c("start", "end", "exact", "contains", "boundary"))

  if(class(namelist) != "list") {
    return(cat("ERROR: The variable names are not provided as a list"))
  }

  switch(match,
         "start" = {regex <- c("^(", ")")},
         "end" = {regex <- c("(", ")$")},
         "exact" = {regex <- c("^(", ")$")},
         "contains" = {regex <- c("(", ")")},
         "boundary" = {regex <- c("\\b", "\\b")}
  )

  setDT(data)


  #Paste diagnosis codes
  reglist <- modify_depth(namelist, 2, function(x) paste0(regex[1], paste0(x, collapse="|"), regex[2]))

  #Seq through variables (1st order in namelist)
  for(i in names(reglist)) {

    #Seq through names (2nd order in namelist)
    for(j in seq_along(names(reglist[[i]]))) {

      if(is.factor(data %>% select(i) %>% pull)) {

        data <- as.data.frame(data)

        #Looping through all indiviual diag codes
        for(k in seq_along(namelist[[i]][[j]])) {

       data <- data %>%
       factR(i, labels = unlist(c(namelist[[i]][[j]][k])) %>% set_names(names(namelist[[i]])[[j]]))

        }


      } else {

        data[, substitute(i) := ifelse(str_detect(get(i), reglist[[i]][[j]]), names(reglist[[i]])[j], get(i))]

      }
    }

  }

 as.data.frame(data)


}
