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


readR <- function(path, extension, leading.zeros = T, na = "", ...) {

  if(str_detect(path, ".csv|.txt")) {

    return(fread(path,
                 keepLeadingZeros = leading.zeros,
                 na.strings=na,
                 data.table = FALSE, ...))

  }

  if(any(str_detect(path, "rds") | extension == "rds")) {

    return(readRDS(path, ...))

  }

  if(any(str_detect(path, ".xlsx") | extension == "xlsx")) {

    return(as.data.frame(readxl::read_xlsx(path,
                                           na = na,
                                           ...)))

  }

  if(any(str_detect(path, ".xls") | extension == "xls")) {

    return(as.data.frame(readxl::read_xls(path,
                                          na = na,
                                          ...)))

  }

  if(any(str_detect(path, ".parquet") | extension == "parquet")) {

    return(as.data.frame(read_parquet(path,
                                      as_data_frame = T,
                                      ...)))

  }


}

