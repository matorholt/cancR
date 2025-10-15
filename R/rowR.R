#' Perform rowwise operations
#'
#'
#' @param data dataset
#' @param vars vector of vars the rowwise operation should be perfomed
#' @param type pmin, pmax, all.na, any.na, sum.na, sum
#' @param new name of the new variable. If missing, type is inserted (e.g. pmin)
#' @param na.rm whether NAs should be removed (default = T)
#' @param filter whether rows with all.na or any.na should be removed
#' @param match whether the provided diagnosis codes should be matched exactly, start with/end with or contain (default)
#' @param direction the direction of the rowwise fill. Can be "left", "rigth", "leftright" and "rightleft". Corresponds to "updown".
#'
#' @return the parent data frame with a new variable based on the selected rowwise operator or a filtered dataset
#' @export
#'
#'

# data.frame(x=c(NA,2,3),
#            y=c(1,2,5),
#            z=c(0,4,7),
#            z_z=c(10,10,10)) %>%
#   rowR(c(x,y,z), "pmax", na.rm=T, new = test2, match = "exact") %>%
#   rowR(c(x,y,z), "sum", sum, na.rm=T, match = "contains")
#
#
# data.frame(x=c(NA,2,3),
#            y=c(NA,NA,5),
#            z=c(NA,4,7),
#            z_z=c(10,10,NA)) %>%
#   rowR(c(x,y,z), "all.na") %>%
#   rowR(c(x,y,z), "any.na") %>%
#   rowR(c(x,y,z), "sum.na", match = "exact")
#
# data.frame(x=c(NA,2,3),
#            y=c(NA,NA,5),
#            z=c(NA,4,7),
#            z_z=c(10,10,NA)) %>%
#   rowR(c(x,y,z), "any.na", any.na, match = "exact")
#
#
# data.frame(x=c(NA,2,3),
#            y=c(NA,NA,5),
#            z=c(NA,4,7),
#            z_z=c(10,10,10)) %>%
#   rowR(c(x,y,z), "all.na", filter=F, test, match = "exact")
#
# data.frame(x=c(NA,2,3),
#            y=c(NA,NA,5),
#            z=c(NA,4,7),
#            z_z=c(NA,10,10)) %>%
#   rowR(c(x,y,z), "all.na", new="test")

#Rowfill
# data.frame(x=c(NA,2,3),
#            y=c(NA,2,NA),
#            z=c(NA,NA,NA),
#            z_z=c(10,10,NA)) %>%
#   rowR(type = "fill", direction = "left")






rowR <- function(data, vars, type, new, na.rm = T, filter = NULL, match = "contains", direction) {

  type <- match.arg(type, c("pmin", "pmax", "sum", "all.na", "any.na", "sum.na", "fill"))
  match <- match.arg(match, c("contains", "exact", "start", "end"))

  vars_c <- data %>% select({{vars}}) %>% names()

  if(!missing(new)) {
  new <- substitute(new)
  } else {
    new <- type
  }

  switch(match,
         "start" = {regex <- c("^(", ")")},
         "end" = {regex <- c("(", ")$")},
         "exact" = {regex <- c("^(", ")$")},
         "contains" = {regex <- c("(", ")")}
  )

  vars_pat <- paste0(regex[1], paste0(vars_c, collapse="|"), regex[2])

  if(type == "sum") {

    data <- data %>% mutate(!!sym(new) := rowSums((select(., matches(c(vars_pat)))), na.rm=na.rm))

  }

  if(type == "pmin") {

    data <- data %>%
      mutate(!!sym(new) := select(., matches(c(vars_pat))) %>% reduce(pmin, na.rm = na.rm))

    #DT: dat[, col_max:= do.call(pmax, c(.SD, list(na.rm=TRUE))), .SDcols= c("x","y","z")]

  }

  if(type == "pmax") {

    data <- data %>%
      mutate(!!sym(new) := select(., matches(c(vars_pat))) %>% reduce(pmax, na.rm = na.rm))

  }

  if(type == "all.na") {

    if(!is.null(filter)) {

      if(filter == "remove") {

      data <- data %>% filter(!(if_all(matches(c(vars_pat)), ~ is.na(.))))


      } else {
        data <- data %>% filter(if_all(matches(c(vars_pat)), ~ is.na(.)))

      }

    } else {
    data <- data %>% mutate(!!sym(new) := if_else(rowSums(is.na(select(., matches(c(vars_pat))))) == ncol(select(., matches(c(vars_pat)))), 1, 0))

  }

  }

  if(type == "any.na") {

    if(!is.null(filter)) {

      if(filter == "remove") {

        data <- data %>% filter(!(if_any(matches(c(vars_pat)), ~ is.na(.))))

      } else {

        data <- data %>% filter(if_any(matches(c(vars_pat)), ~ is.na(.)))
      }

    } else {

    data <- data %>% mutate(!!sym(new) := if_else(rowSums(is.na(select(., matches(c(vars_pat))))) > 0, 1, 0))

    }
  }

  if(type == "sum.na") {

    data <- data %>% mutate(!!sym(new) := rowSums(is.na(select(., matches(c(vars_pat))))))

  }

  if(type == "fill") {

    direction <- str_replace_all(direction, c("left" = "up",
                                       "right" = "down"))

    data <- data %>% mutate(row = row_number()) %>%
      pivot_longer(cols=-contains("row")) %>%
      group_by(row) %>%
      fill(value, .direction = direction) %>%
      pivot_wider(names_from="name", values_from = "value") %>%
      ungroup %>%
      select(-row)




  }

 data

}



