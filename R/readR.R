#' Load csv, excel, rds and parquet files
#'
#' @description
#' Wrapper for the fread, readxl, readRDS and read_parquet functions with automatic detection of file extension.
#'
#'
#' @param path path for the file to load.
#' @param ... arguments passes to subfunctions
#'
#' @returns the given path imported as a data frame
#' @export
#'
#'


readR <- function(path, extension, leading.zeros = T, na = "", ...) {

  if(str_detect(path, ".(csv|txt|rds|xls|parquet)", negate=T)) {

    path <- fs::dir_ls("../", recurse=2)[str_detect(fs::dir_ls("../", recurse=2), path)]

    #return(path)

    if(length(path) > 1) {
      cat(paste0("Error: Multiple files detected, please provide the file name with an extension such as myfile.csv\n\n"))
      return(cat("Detected files:\n", paste0(path, sep="\n")))

    }

    cat(paste0("No extension provided. Guessing at file: ", path, "\n\n"))


  }

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

