#' cpR
#'
#' @description
#' Fixes CPR numbers with removed leading zeros. Works with piping
#'
#'
#' @param data dataset
#' @param cpr cpr-column
#' @param extract TRUE if age and date of birth should be extracted
#'
#' @return Returns same dataset with correct CPR numbers and optionally age and date of birth. Invalid CPRs stops the function and returns the invalid CPRs as a vector.
#' @export
#'


cpR <- function(data, cpr=cpr, extract=F) {

  if(any(str_detect(data %>% pull({{cpr}}), "^\\d{9,10}$|^\\d{5,6}-?\\w{4}$", negate=T) |
     str_count(data %>% pull({{cpr}})) == 10 & str_sub(data %>% pull({{cpr}}), 1,2) %in% c("00", seq(32,99)) |
     str_count(data %>% pull({{cpr}})) == 10 & str_sub(data %>% pull({{cpr}}), 3,4) %in% c("00", seq(13,99)) |
     str_count(data %>% pull({{cpr}})) == 9 & str_sub(data %>% pull({{cpr}}), 2,3) %in% c("00", seq(13,99)) |
     is.na(data %>% pull({{cpr}})))
                ) {
    errors <- data %>% filter(str_detect(data %>% pull({{cpr}}), "^\\d{9,10}$|^\\d{5,6}-?\\w{4}$", negate=T) |
                                str_count(data %>% pull({{cpr}})) == 10 & str_sub(data %>% pull({{cpr}}), 1,2) %in% c("00", seq(32,99)) |
                                str_count(data %>% pull({{cpr}})) == 10 & str_sub(data %>% pull({{cpr}}), 3,4) %in% c("00", seq(13,99)) |
                                str_count(data %>% pull({{cpr}})) == 9 & str_sub(data %>% pull({{cpr}}), 2,3) %in% c("00", seq(13,99)) |
                        is.na(data %>% pull({{cpr}})))
    warning(paste0(nrow(errors), " invalid CPR", rep("s", nrow(errors)>1), " detected and returned as vector"))
    return(errors %>% pull({{cpr}}))

    }

  data <- data %>%
    mutate({{cpr}} := str_pad(str_remove_all({{cpr}}, "-"), width=10, pad="0"))

  if(extract) {
    data <- data %>%
      mutate(sex = case_when(str_sub({{cpr}}, 10) %in% seq(0,8,2) ~ "F",
                             str_sub({{cpr}}, 10) %in% seq(1,9,2) ~ "M"),
             birth = case_when(str_sub({{cpr}}, 5,6) %in% str_pad(seq(0,36), 2, pad="0") &
                                  str_sub({{cpr}}, 7,7) %in% c(4, 9) ~ as.Date(str_c("20", str_replace_all({{cpr}}, "(\\d{2})(\\d{2})(\\d{2})(\\w{4})", "\\3-\\2-\\1"), sep="")),
                               str_sub({{cpr}}, 5,6) %in% str_pad(seq(0,57), 2, pad="0") &
                                 str_sub({{cpr}}, 7,7) %in% seq(5,8) ~ as.Date(str_c("20", str_replace_all({{cpr}}, "(\\d{2})(\\d{2})(\\d{2})(\\w{4})", "\\3-\\2-\\1"), sep="")),
                               str_sub({{cpr}}, 5,6) %in% str_pad(seq(58,99), 2, pad="0") &
                                 str_sub({{cpr}}, 7,7) %in% seq(5,8) ~ as.Date(str_c("18", str_replace_all({{cpr}}, "(\\d{2})(\\d{2})(\\d{2})(\\w{4})", "\\3-\\2-\\1"), sep="")),

                               T ~ as.Date(str_c("19", str_replace_all({{cpr}}, "(\\d{2})(\\d{2})(\\d{2})(\\w{4})", "\\3-\\2-\\1"), sep=""))))
  }

  return(data)
}



