#' Load csv, excel and rds files
#'
#' @description
#' Wrapper for the fread, readxl and readRDS functions with automatic detection of file extension
#'
#'
#' @param path path for the file to load
#' @param ... arguments passes to subfunctions
#'
#' @returns the given path imported as a data frame
#' @export
#'
#'


readR <- function(path, leading.zeros = T, na = "", ...) {

  if(str_detect(path, ".csv|.txt")) {

    return(fread(path,
                 keepLeadingZeros = leading.zeros,
                 na.strings=na,
                 data.table = FALSE, ...))

  }

  if(str_detect(path, "rds")) {

    return(readRDS(path, ...))

  }

  if(str_detect(path, ".xlsx")) {

    return(as.data.frame(readxl::read_xlsx(path,
                                           na = na,
                                           ...)))

  }

  if(str_detect(path, ".xls")) {

    return(as.data.frame(readxl::read_xls(path,
                                          na = na,
                                          ...)))

  }

}

