#' Load csv, excel, rds and parquet files
#'
#' @description
#' Wrapper for the fread, readxl, readRDS and read_parquet functions with automatic detection of file extension.
#'
#'
#' @param path path for the file to load.
#' @param leading.zeros whether leading zeros should be kept (default = T)
#' @param na character vector specifying NA strings (default = "")
#' @param ... arguments passes to subfunctions
#'
#' @returns the given path imported as a data frame
#' @export
#'
#'


readR <- function(path, leading.zeros = T, na = "", ...) {

  if(str_detect(path, ".(csv|txt|rds|xls|parquet|sas7bdat)", negate=T)) {


    path <- fs::dir_ls("../", recurse=TRUE)[str_detect(fs::dir_ls("../", recurse=TRUE), regex(paste0(path), ignore_case = TRUE))]

    if(length(path) > 1) {
      cli::cli_alert_danger("Error: Multiple files detected, please provide the file name with an extension such as myfile.csv")
      cli::cli_abort("Detected files: {paste0(path, sep='\n\')}")

    }

    if(length(path) == 0) {
      cli::cli_abort("No files detected")
    }

    cli::cli_alert_info("No extension provided. Guessing at file: {path}")


  }

  if(str_detect(path, ".csv|.txt")) {

    return(fread(path,
                 keepLeadingZeros = leading.zeros,
                 na.strings=na,
                 data.table = FALSE, ...) %>% as.data.frame)

  }

  if(any(str_detect(path, "rds"))) {

    return(readRDS(path, ...) %>% as.data.frame)

  }

  if(any(str_detect(path, ".sas7bdat"))) {

    return(haven::read_sas(path,
                           ...) %>% as.data.frame)

  }

  if(any(str_detect(path, ".xlsx"))) {

    return(readxl::read_xlsx(path,
                             na = na,
                             ...) %>% as.data.frame)

  }

  if(any(str_detect(path, ".xls"))) {

    return(readxl::read_xls(path,
                            na = na,
                            ...) %>% as.data.frame)

  }

  if(any(str_detect(path, ".parquet"))) {

    return(arrow::read_parquet(path,
                               as_data_frame = T,
                               ...) %>% as.data.frame)

  }


}
