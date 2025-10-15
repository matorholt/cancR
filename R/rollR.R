#' Assign rolling ID.
#'
#' @description
#' Useful after sorting a dataset, which changes the ID order. Works with piping
#'
#'
#' @param data dataset
#' @param id column which unique ids should be based on
#' @param lab label for new unique id column
#'
#' @return adds a new column to the dataset with unique ids based on original id conditional on a sorting
#' @export
#'
#'
# data.frame(id=c(1,1,1,1,2,2,2,3,3,3,4,4,4,4),
#            x=c(5,5,6,6,7,8,9,1,2,1,4,1,2,4)) %>%
#   arrange(x) %>%
#   rollR()


rollR <- function(data, id = id, lab = order) {

  data %>% mutate({{lab}} := match({{id}}, unique({{id}})))
}


