#' Routine modifications of lists
#'
#' @description
#' Routine modification of lists such as reverse values and names for use in other functions.
#' The functions is also a wrapper for the rrapply and map_depth functions with the dots argument.
#'
#'
#' @param input the list or vector
#' @param type type of modification of the list. See details
#' @param ... arguments for the rrapply function
#'
#' @details
#' "reverse" reverses the values and names of a list which is used as inputs in functions such as str_replace.
#' "vec2list" converts a vector to a list with names corresponding to the vector elements
#'
#'
#' @returns returns a modified list based on the "type" argument
#' @export
#'
#' @examples
#'
#' reverse_list <- list("first" = "a1",
#'                      "second" = "b2")
#'
#' listR(reverse_list, type = "reverse")

# reverse_list <- list("first" = "a1",
#                      "second" = "b2")
#
# listR(reverse_list, type = "reverse")

# vec <- letters[1:5]
# listR(vec, type = "vec2list")

listR <- function(input, type, ...) {

  if(type == "reverse") {

   return(names(input) %>% set_names(input))

  }

  if(type == "vec2list") {

    return(as.list(input) %>% set_names(input))

  }

  return(rrapply(input, ...))

}
