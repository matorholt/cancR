#' Cut numeric variables into intervals
#'
#' @description
#' Wrapper for cut() with simplified labels and automatic conversion to factor based on most frequent levels
#'
#' @param data dataframe. Works with piping.
#' @param vars Vector of vars which should be cut
#' @param seqlist List of the split patterns for each var. E.g. list("v1" = seq(0,10), "v2" = round(quantile(df %>% pull(age), seq(0,1,0.1)),0))
#' @param name.list Whether the split should overwrite or as a new variable
#' @param name.pattern Optional naming pattern that should automatically be pasted on the end of the variable name.
#'
#' @return The inputted dataframe with the cut variables
#' @export
#'
#' @examples
#' redcap_df %>%
#'   cutR(size, seq(0,100,10)) %>%
#'   str
#'

# #Multiple vars
# redcap_df %>%
#   datR(c(birth, date_of_surgery)) %>%
#   cutR(vars=c(size, birth, type, date_of_surgery),
#        seqlist = list("size" = list("quantile", c(0, 0.25,0.5,0.75, 1)),
#                       "birth" = list("seq", c(1800,3000,10)),
#                       "type" = c(0,1),
#                       "date_of_surgery" = seq(1800,3000,10)),
#        names = list("size" = "sizef",
#                     "birth" = "byear")) %>%
#   str
#
# #Simple
# redcap_df %>%
#   cutR(size, seq(0,100,10)) %>%
#   str
#
#
# #Simple
# redcap_df %>%
#   cutR(size, list("quantile", c(0,0.5,1))) %>%
#   str
#
# # Multiple variables with same sequence 1
# redcap_df %>%
#   datR(c(birth, followup, date_of_surgery)) %>%
#   cutR(vars = c(birth, followup, date_of_surgery),
#        seqlist = seq(1800,2050,10)) %>%
#   str
#
# Multiple variables with same sequence 2
# redcap_df %>%
#   #Induce NA
#   mutate(size = ifelse(row_number() %in% ceiling(runif(20, 1, 500)), NA, size)) %>%
#   datR(c(birth, followup, date_of_surgery)) %>%
#   cutR(vars = c(size, type, localisation),
#        seqlist = list("quantile", c(0,0.5,1)),
#        name.pattern = "_bin") %>%
#   str


cutR <- function(data, vars, seqlist, name.list = list(), name.pattern = NULL, digits = 0) {

  vars_c <-
    data %>% select({{vars}}) %>% names()

  if(length(vars_c) == 1 & (is.null(names(seqlist)) | class(seqlist) == "numeric")) {
    seqlist <- list(seqlist)
    names(seqlist) <- vars_c

  }

  if(length(vars_c) > 1 & class(seqlist) == "numeric") {

    seq_value <- seqlist
    seqlist <- list()


  } else if((length(seqlist) != length(vars_c)*2) & length(names(seqlist)) != length(vars_c)) {

    seq_value <- seqlist
    seqlist <- list()
  }

  if(class(name.list) != "list") {

    names <- list(name.list)
    names(names) <- vars_c
  }

  if(!is.null(name.pattern)) {

    name.list <- as.list(paste0(vars_c, name.pattern)) %>% set_names(vars_c)

  }



  #Default newvars
    default_names <- as.list(vars_c)
    names(default_names) <- vars_c

  newvars <- modifyList(default_names, name.list)

  for(v in vars_c) {

    #Check class
    if(any(class(data[[v]]) %in% c("character", "logical"))) {
      return(cat(paste0("Error: ", v, " is ", class(data[[v]]), " and needs to be converted into numerical, date or integer")))
    }

    #Extract year from dates
    if(class(data %>% pull(v)) == "Date") {
      data <- data %>%
        mutate(!!sym(newvars[[v]]) := as.numeric(str_extract(!!sym(v), "\\d{4}")))
    } else if(class(data %>% pull(v)) != "numeric") {
      data <- data %>%
        mutate(!!sym(newvars[[v]]) := as.numeric(!!sym(v)))
    }
    #Copy frame
    else {
      data <- data %>%
        mutate(!!sym(newvars[[v]]) := !!sym(v))
    }

    #Modify seqlist
    if(exists("seq_value")) {
      seqlist[[v]] <- seq_value
    }


    if(seqlist[[v]][[1]] %in% "quantile") {

      seqlist[[v]] <- unique(quantile(data[[v]], seqlist[[v]][[2]], na.rm=T))

    } else if(seqlist[[v]][[1]] %in% "seq") {

      seqlist[[v]] <- do.call(seq, as.list(seqlist[[v]][[2]]))
    }

    data <- data %>%
      mutate(!!sym(newvars[[v]]) := str_replace(cut(round(!!sym(newvars[[v]]), digits),
                                                    breaks = round(seqlist[[v]],digits),
                                                    include.lowest = TRUE,
                                                    right = F,
                                                    dig.lab=4),
                                                "\\W(-?\\d+\\.?\\d*)\\,(-?\\d+\\.?\\d*)\\W",
                                                "\\1-\\2")) %>%
      factR(!!sym(newvars[[v]]))




  }

  data



}

