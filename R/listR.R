#' Routine modifications of lists
#'
#' @description
#' Routine modification of lists such as reverse values and names for use in other functions.
#' The functions is also a wrapper for the rrapply and map_depth functions with the dots argument.
#'
#'
#' @param input the list or vector
#' @param type type of modification of the list. See details
#' @param layer vector of integers indicating which layer to remove if type is "peel" or to keep if type is "pick". Layer 1 is the top layer
#' @param collapse whether duplicated elements should be collapsed (defualt = F)
#' @param ... arguments for the rrapply function
#'
#' @details
#' "reverse" reverses the values and names of a list which is used as inputs in functions such as str_replace.
#' "vec2list" converts a vector to a list with names corresponding to the vector elements
#' "peel" and "pick" depends on "layer" and either drops or keeps the specified vector.
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
#
# vec <- letters[1:5]
# listR(vec, type = "vec2list")
#
# peel_list <- list(lpr = list(exclusion = list(immun = "a",
#                                           other = "b",
#                                           test = "c"),
#                          covariates = list(immun = "d",
#                                            endo = "e")))
# listR(peel_list, "peel", layer = 2, collapse = T)
# listR(peel_list, "pick", layer = c(1,3,4), collapse = F)


listR <- function(input, type, layer, collapse = F, ...) {

  if(type == "reverse") {

   return(as.list(names(input)) %>% set_names(input))

  }

  if(type == "vec2list") {

    return(as.list(input) %>% set_names(input))

  }

  if(type == "pick") {

    type <- "peel"
    layer <- -layer

    if(length(layer) <2) return(cli::cli_alert_danger("Error: More than one layer must be picked"))

  }

  if(type == "peel") {

    melted <- rrapply(input, how = "melt")[, -layer]

    if(collapse) {
      melted <- melted %>%
        group_by(across(1:(ncol(.) - 1))) %>%
        summarise(
          across(last_col(), ~ paste(.x, collapse = ", ")),
          .groups = "drop"
        )
    }

    return(rrapply(melted, how = "unmelt"))

  }

  return(rrapply(input, ...))

}
