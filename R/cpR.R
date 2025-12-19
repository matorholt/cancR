#' Fix CPR numbers with removed leading zeros
#'
#'
#' @param data dataset
#' @param cpr cpr-column
#' @param extract TRUE if age and date of birth should be extracted
#' @param remove.cpr whether invalid CPRs should be removed, default = F
#' @param return.cpr whether the invalid CPRs should be returned as a vector, default = F
#'
#' @return Returns same dataset with correct CPR numbers and optionally age and date of birth. Invalid CPRs stops the function and returns the invalid CPRs as a vector.
#' @export
#'

# data.frame(cpr = c("010101-1234",
#                    "0101011234",
#                    "9999999999",
#                    "101011234"
# )) %>%
#   cpR(extract=T,
#       return.cpr=F,
#       remove.cpr=T)


cpR <- function(data, cpr=cpr,extract=F, remove.cpr = F, return.cpr = F) {

  data <- data %>% ungroup()

  cpr_c <- data %>% select({{cpr}}) %>% names

    errors <- data %>% filter(str_detect(data[[cpr_c]], "^\\d{9,10}$|^\\d{5,6}-?\\w{4}$", negate=T) |
                                str_count(data[[cpr_c]]) == 10 & str_sub(data[[cpr_c]], 1,2) %in% c("00", seq(32,99)) |
                                str_count(data[[cpr_c]]) == 10 & str_sub(data[[cpr_c]], 3,4) %in% c("00", seq(13,99)) |
                                str_count(data[[cpr_c]]) == 9 & str_sub(data[[cpr_c]], 2,3) %in% c("00", seq(13,99)) |
                        is.na(data[[cpr_c]]))

    if(nrow(errors) > 0) {

   if(return.cpr) {
     warning(paste0(nrow(errors), " invalid CPR", rep("s", nrow(errors)>1), " detected and returned as vector"))
   return(errors[[cpr_c]])
   } else {

     if(remove.cpr) {
       warning(paste0(nrow(errors), " invalid CPR", rep("s", nrow(errors)>1), " detected and removed"))
     data <- data %>% filter(!!sym(cpr_c) != errors[[cpr_c]])
     } else {
       warning(paste0(nrow(errors), " invalid CPR", rep("s", nrow(errors)>1), " detected"))
     }
   }
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

                               T ~ as.Date(str_c("19", str_replace_all({{cpr}}, "(\\d{2})(\\d{2})(\\d{2})(\\w{4})", "\\3-\\2-\\1"), sep=""))),
             across(c(sex, birth), ~ case_when(cpr %in% errors[[cpr_c]] ~ NA,
                                               T ~ .)))
  }

  return(data)
}

