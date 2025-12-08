#' Routine modifications of lists
#'
#' @description
#' Routine modification of lists such as reverse values and names for use in other functions.
#' The functions is also a wrapper for the rrapply and map_depth functions with the dots argument.
#'
#'
#' @param list the list to modify
#' @param type type of modification of the list. See details
#' @param ... arguments for the rrapply function
#'
#' @details
#' "reverse" reverses the values and names of a list which is used as inputs in functions such as str_replace.
#'
#'
#' @returns returns a modified list based on the "type" argument
#' @export
#'
#'

# reverse_list <- list("first" = "a1",
#                      "second" = "b2")
#
# listR(reverse_list, type = "reverse")

listR <- function(list, type, ...) {

  if(type == "reverse") {

   return(names(list) %>% set_names(list))


  }

  return(rrapply(list, ...))

}


