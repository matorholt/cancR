#' @title Defuse arguments to character string regardless of quoted or non-quoted in functions.
#'
#' @param input quoted or non-quoted vector (e.g. variable names)
#' @param data data frame if tidyselection is used.
#'
#' @returns the defused argument as a character string of class "def_ex" for later detection
#' @export
#'
#'

# test_c <- defusR(c(test, best))
#
# defusR(test_c)
#
# myfun <- function(data, vars) {
#
#   vars_c <- defusR(vars)
#
#   #Basic usage
#   df_vars <- redcap_df[, vars_c]
#
#   redcap_dt <- as.data.table(redcap_df)
#
#   dt_keep <- redcap_dt[, .SD, .SDcols = vars_c]
#
#   dt_remove <- redcap_dt[, c(vars_c) := NULL]
#
#   #Nested functions
#   row_fun <- rowR(redcap_df, vars = vars_c, type = "sum")
#   roll_fun <- rollR(redcap_df, type = "count", order = vars_c)
#
#   return(list(test_c, df_vars, dt_keep, dt_remove, row_fun, roll_fun))
# }
#
# myfun(redcap_df, vars = c(type:necrosis))

defusR <- function(input, data = NULL) {

  #Tidyselect commands
  tidys <- c("contains", "matches", "starts_with", "ends_with", "everything", ":")

  #If already defused - return character
  val <- tryCatch(input, error = function(e) NULL)
  if(is.character(val)) {

    #if(!inherits(val, "def_ex")) class(val) <- c("def_ex", "character")

    return(val)
  }

  # Otherwise defuse with substitute from parent envir
  input_expr <- eval(substitute(substitute(x, parent.env),
                                list(x = substitute(input),
                                     parent.env = parent.frame())))

  #is.call as it is an argument
  is_tidy <- is.call(input_expr) && any(str_detect(as.character(input_expr), paste0(tidys, collapse = "|")))

  if (is_tidy && is.null(data)) {
    data <- tryCatch(get("data", envir = parent.frame()), error = function(e) NULL)
  }

  out <- if (is_tidy && !is.null(data)) {
    tidyselect::eval_select(input_expr, data) %>% names
  } else {
    if (length(as.character(input_expr)) > 1) as.character(input_expr)[-1] else as.character(input_expr)
  }

  if(length(out) == 0) out <- NULL

  out
}
