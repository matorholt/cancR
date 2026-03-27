#' Perform rowwise operations
#'
#'
#' @param data dataset
#' @param vars vector of vars the rowwise operation should be perfomed. If missing, all columns are assigned
#' @param type pmin, pmax, all.na, any.na, sum.na, sum or fill
#' @param label label of the new variable. If missing, type is inserted (e.g. pmin)
#' @param na.rm whether NAs should be removed (default = T)
#' @param filter whether rows with all.na or any.na should be removed
#' @param direction the direction of the rowwise fill. Can be "left", "rigth", "leftright" and "rightleft". Corresponds to "updown".
#'
#' @return the parent data frame with a new variable based on the selected rowwise operator or a filtered dataset
#' @export
#'
#'

# df <- data.frame(x=c(NA,2,3, NA,NA),
#                  y=c(1,2,5,3,NA),
#                  z=c(0,4,7,4,NA),
#                  z_z=c(10,10,10,NA,NA))
#
# #Pmin/pmax/sum
# df %>%
#   rowR(c(x,y,z), "pmax", na.rm=T, label = "labs") %>%
#   rowR(c(x,y,z), "pmin", na.rm=F) %>%
#   rowR(c(x,y,z), "sum")
#
# #NAs
# df %>%
#   rowR(type = "any.na", label = "any.na_all") %>%
#   rowR(vars = c(y,z), type = "any.na", label = "any.na_yz") %>%
#   rowR(type = "all.na") %>%
#   rowR(type = "sum.na")
#
# #NA filters
# df %>%
#   rowR(type = "any.na", filter = "remove")
#
# df %>%
#   rowR(type = "all.na", filter = "remove")
#
# df %>%
#   rowR(type = "fill", direction = "leftright")





rowR <- function(data,
                 vars,
                 type,
                 label,
                 na.rm = T,
                 filter = NULL,
                 direction,
                 dt=F) {

  type <- match.arg(type, c("pmin", "pmax", "sum", "all.na", "any.na", "sum.na", "fill"))

  if(missing(vars)) {
    vars_c <- names(data)
  } else {
  vars_c <- data %>% select({{vars}}) %>% names()
  }

  if(!missing(label)) {
  label <- as.character(label)
  } else {
    label <- type
  }

  dat <- as.data.table(data)

  if(type == "sum") {

    dat[, substitute(label) := rowSums(.SD, na.rm = na.rm), .SDcols = vars_c]


  }

  if(type == "pmin") {

    dat[, substitute(label):= do.call(pmin, c(.SD, list(na.rm=na.rm))), .SDcols= vars_c]

  }

  if(type == "pmax") {

    dat[, substitute(label):= do.call(pmax, c(.SD, list(na.rm=na.rm))), .SDcols= vars_c]
  }

  if(type == "all.na") {

    if(!is.null(filter)) {

      if(filter == "remove") {

        dat <- dat[dat[,!Reduce(`&`, lapply(.SD, is.na)), .SDcols = vars_c]]

      } else {
        dat <- dat[dat[, Reduce(`&`, lapply(.SD, is.na)), .SDcols = vars_c]]

      }

    } else {
      dat[, substitute(label) := as.integer(Reduce(`&`, lapply(.SD, is.na))), .SDcols = vars_c]

  }

  }

  if(type == "any.na") {

    if(!is.null(filter)) {

      if(filter == "remove") {

        dat <- dat[dat[,!Reduce(`|`, lapply(.SD, is.na)), .SDcols = vars_c]]

      } else {

        dat <- dat[dat[, Reduce(`|`, lapply(.SD, is.na)), .SDcols = vars_c]]

      }

    } else {
      dat[, substitute(label) := as.integer(Reduce(`|`, lapply(.SD, is.na))), .SDcols = vars_c]

    }
  }

  if(type == "sum.na") {

    dat[, substitute(label) := rowSums(is.na(.SD)), .SDcols = vars_c]

  }

  if(type == "fill") {

    #Convert to matrix
    m <- as.matrix(dat[, ..vars_c])
    n <- nrow(m)
    p <- ncol(m)

    #Right function
    fill_right <- function(mat) {
      for (j in 2:p) {
        idx <- is.na(mat[, j])
        mat[idx, j] <- mat[idx, j-1]
      }
      mat
    }

    #Left function
    fill_left <- function(mat) {
      for (j in (p-1):1) {
        idx <- is.na(mat[, j])
        mat[idx, j] <- mat[idx, j+1]
      }
      mat
    }

    switch(direction,
           "right" = {m_filled <- fill_right(m)},
           "left" = {m_filled <- fill_left(m)},
           "leftright" = {m_filled <- fill_left(m) ; m_filled <- fill_right(m_filled)},
           "rightleft" = {m_filled <- fill_right(m) ; m_filled <- fill_left(m_filled)})

    #Assign back
    for (j in seq_along(vars_c)) {
      dat[[vars_c[j]]] <- m_filled[, j]
    }




  }

 if(dt) return(dat) else return(as.data.frame(dat))

}
