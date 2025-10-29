#' Load csv, excel and rds files
#'
#' @description
#' Wrapper for the fread, readxl and readRDS functions with automatic detection of file extension
#'
#'
#' @param path path for the file to load
#'
#' @returns the given path imported as a data frame
#' @export
#'
#'


readR <- function(path) {

  if(str_detect(path, ".csv")) {

    return(as.data.frame(fread(path)))

  }

  if(str_detect(path, "rds")) {

    return(readRDS(path))

  }

  if(str_detect(path, ".xlsx")) {

    return(as.data.frame(readxl::read_xlsx(path)))

  }

  if(str_detect(path, ".xls")) {

    return(as.data.frame(readxl::read_xls(path)))

  }

}

