#' tablR.match
#'
#' @description
#' Automatic tablR function for overview of matched and unmatched cases
#'
#' @param data matched dataset
#' @param casename name or number of cases (e.g. 1 or "CLL")
#' @param n.controls n.controls from the matchR function
#' @param vars vars that should be in the table
#' @param labels List specifying labels of the specific labels for each variable
#' @param headings List specifying labels for variable names
#' @param filename
#'
#' @return Returns a table of the matching overview and exports a word-file (optional if filename is provided)
#' @export
#'
#'

tablR.match <- function(data,
                        casename,
                        n.controls,
                        ...) {

  m <- paste0(seq(n.controls,0, -1), " Matches")
  names(m) <- as.character(seq(n.controls,0, -1))

  data %>%
    group_by(set) %>%
    mutate(n_controls = as.character(n() - 1)) %>%
    ungroup() %>%
    factR(n_controls, labels=m, lab_to_lev=T) %>%
    filter(case == casename) %>%
    tablR(n_controls,
          ...)

}

