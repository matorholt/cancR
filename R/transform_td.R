#' transform_td
#'
#' @description
#' Transformation of matching data frame with wide format to long format with one row per pnr
#'
#'
#' @param data dataframe after matching
#' @param level whether the last value or maximal value should be picked. Maximal applies for ranked variables. Otherwise the last will be chosen.
#'
#' @return dataframe with one row per pnr with the last or maximal value of the matched time-dependent variables
#' @export
#'
# test <- data.frame(pnr = rep(c(1:5),each=36),
#                    var = rep(c("gift", "ugift", "hovedstaden", "sjælland", "nordjylland", "midt", "syddanmark", "Basic", "Bachelor", "'Vocational training'", "'Higher education'", "'Upper secondary'", "lon", "off", "Q1", "Q2", "Q3", "Q4", "cci_1", "cci_2", "cci_3", "cci_4", "cci_5", "cci_6", "cci_7", "cci_8", "cci_9", "cci_10", "cci_11", "cci_12", "cci_13", "cci_14", "cci_15", "cci_16", "cci_17", "cci_18"),5),
#                    date = sample(c(rep(as.Date(NA), 100), seq(as.Date("2000-01-01"), as.Date("2020-01-01"), by = "month")), 36*5, replace = T)) %>%
#   pivot_wider(names_from = var, values_from = date) %>%
#   mutate(index = rep(as.Date("2010-01-01"),5)) %>%
#   select(pnr, index, everything())
#
#transform_td(test, level = "max")
#
#
#
# transform_td(test, level = "frequent")
#
# transform_td(test, level = "last")
#

transform_td <- function(data, level = "last") {
  level <- match.arg(level, c("max", "last", "frequent"))

  long <- data %>% pivot_longer(cols = c(gift:cci_18), names_to = "var", values_to = "date") %>%
    filter(date <= index) %>%
    arrange(pnr, date) %>%
    mutate(category = case_when(str_detect(var, "cci") ~ "CCI",
                                str_detect(var, "gift") ~ "marital",
                                str_detect(var, "hovedstaden|sjælland|nordjylland|midt|syddanmark") ~ "region",
                                str_detect(var, "Basic|Bachelor|Vocational training|Higher education|Upper secondary") ~ "education",
                                str_detect(var, "lon|off") ~ "support",
                                str_detect(var, "Q\\d") ~ "income"))

  if(level == "last") {
    wide <- long %>%
      group_by(pnr, category) %>%
      slice(n()) %>%
      ungroup() %>%
      select(-date) %>%
      pivot_wider(names_from = category, values_from = var)
  }

  if(level == "frequent") {
    wide <- long %>%
      group_by(pnr, category) %>%
      summarise(index = first(index),
                var = cancR::mode(var, "last")) %>%
      pivot_wider(names_from = category, values_from = var)
  }

  if(level == "max") {
    wide <- long %>%
    group_by(pnr, category) %>%
      mutate(rank = case_when(category %in% c("income", "CCI") ~ as.numeric(str_extract(var, "\\d+")),
                              var %in% c("ugift", "Basic", "off") ~ 1,
                              var %in% c("gift", "'Upper secondary'", "lon") ~ 2,
                              var %in% "'Vocational training'" ~ 3,
                              var %in% "Bachelor" ~ 4,
                              var %in% "'Higher education'" ~ 5,
                              category %in% "region" & row_number() ==  n() ~ 1)) %>%
      slice(which.max(rank)) %>%
      select(-rank, -date) %>%
      pivot_wider(names_from = category, values_from = var)
  }

return(wide)
}


