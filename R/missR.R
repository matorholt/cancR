#' missR
#'
#' @description
#' Fast overview of NAs in a dataframe
#'
#'
#' @param data data frame
#' @param vars vars where the na check should be beformed. If missing the whole data frame is analysed
#' @param id name of the id-column. If missing a guess at the most common names is attempted.
#' @param print whether the NA check should be printed in the console
#'
#' @return Prints whether any NAs are detected and returns a data frame with IDs and columns with NA
#' @export
#'
#'

# n=200
# set.seed(1)
# df <- data.frame(id=seq(1:n),
#                  group=sample(c("pre", "sub"), n, replace=T),
#                  sex=factor(sample(c("M","F"), n, replace=T)),
#                  age_group=sample(c("<50",">50"),n,replace=T),
#                  chemo = sample(c("yes","no"), n, replace=T),
#                  age = sample(c(seq(50,60), 50), n, replace=TRUE),
#                  hospital = sample(c("rh","herlev","roskilde"), n, replace=T)) %>%
#   mutate(hospital = ifelse(group %in% "sub", "roskilde", hospital),
#          chemo = ifelse(group %in% "pre", "yes", chemo),
#          age_group = ifelse(group %in% "sub", "<50", age_group),
#          hospital = as.factor(hospital))
#
# #add random NA
# df <- apply (df, 2, function(x) {x[sample( c(1:n), floor(n/10))] <- NA; x} ) %>%
#   as_tibble()
#
# missR(df)

missR <- function(data, vars, id=id, print=T) {

  id_syn <- c("id","ID","pnr","pt_id","study_id")

  if(missing(id)) {
    if(sum(id_syn %in% colnames(data)) > 1) {
      return(cat("Multiple ID columns detected - pick only one"))
    }
    id_c <- data %>% select(matches(id_syn)) %>% names()
  } else {
    id_c <- data %>% select({{id}}) %>% names()
  }

  if(!missing(vars)) {
    vars_c <- data %>% select({{vars}}, -{{id}}) %>% names()

  } else {
    vars_c <- data %>% select(-{{id}}) %>% names()
  }

  #Counts
  d <- data.frame(variable = vars_c,
                  NAs = 0)

  for(v in 1:length(vars_c)) {
    d[v, "NAs"] <- sum(is.na(data[,vars_c[v]]))
  }

  d <- d %>% filter(NAs > 0) %>% arrange(desc(NAs))


  #Invididual data frame
  ind <- data %>% select({{id}}) %>% drop_na({{id}})

  for(v in vars_c) {
    ind <- left_join(ind,
                     data %>%
                       select({{id}}, !!sym(v)) %>%
                       filter(is.na(!!sym(v))) %>%
                       mutate(!!sym(v) := v),
                     by= id_c)
  }

  ind <- ind %>% rowR(vars = vars_c, type = "all.na", filter = "remove")

  if(nrow(d) == 0) {
    if(print) {
      cat("No NAs detected")
    }
    invisible(NULL)
  } else {
    if(print) {
      cat("Nas detected in the following variables:\n\n")
      print(d)
    }
    invisible(list(counts=d, idframe=ind, ids = ind %>% select({{id_c}}) %>% pull({{id_c}})))
  }



}
