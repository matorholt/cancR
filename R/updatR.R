#' updatR
#'
#' @description
#' Updating of Charlson Comorbidity Index values over time#'
#'
#' @param data data frame with case IDS
#' @param td.frame Time-dependent frame containing CCI values to be changed
#' @param exclusion Vector of diagnosis codes to be excluded
#'
#' @return An updated td.frame
#' @export
#'


updatR <- function(data, td.frame, exclusion) {

  lpr <- loadR("lpr", id.filter = data %>% filter(case == 1) %>% select(pnr) %>% as.data.frame())

  setDT(lpr)

  lpr_dim <- lpr[str_detect(diag, exclusion, negate=T)] %>% as.data.frame()

  cci <- cci_timR(lpr_dim) %>%
    select(-cci) %>%
    rename(cci = cci_bin,
           date = charlson.date)

  rows_upsert(td_frame, cci, by = c("pnr", "date")) %>%
    arrange(pnr, date) %>%
    fill(region, income, education, marital, .direction = "down")

}
