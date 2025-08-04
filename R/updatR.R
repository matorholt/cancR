#' updatR
#'
#' @description
#' Updating of Charlson Comorbidity Index values over time#'
#'
#' @param data data frame with case IDS
#' @param exclusion Vector of diagnosis codes to be excluded (e.g. c("DC1", "DC2", "DC3"))
#' @param lpr lpr data frame. Loaded automatically if missing
#' @param td.frame Time-dependent frame containing CCI values to be changed. Loaded automatically if missing
#'
#' @return An updated td.frame
#' @export
#'


updatR <- function(data, exclusion, match = "start") {

  cat(paste0("\nInitializing updatR algorithm: ", tockR("time"), "\n"))

  tickR()

  start <- tickR.start

  cat(paste0("\nLoading LPR and SES: "))

  reglist <- loadR(c("lpr", "ses"), id.filter = data[data$case == 1, "pnr"])

  lpr <- reglist$lpr
  td.frame <- reglist$ses


  tickR()

  cat(paste0("\nUpdating CCI scores: "))

  switch(match,
         "start" = {regex <- c("^(", ")")},
         "end" = {regex <- c("(", ")$")},
         "exact" = {regex <- c("^(", ")$")},
         "contains" = {regex <- c("(", ")")}
  )

  exclude <- paste0(regex[1], paste0(exclusion, collapse="|"), regex[2])

  setDT(lpr)

  lpr_dim <- lpr[str_detect(diag, exclude, negate=T)]

  cci <- cci_timR(lpr_dim) %>%
    select(-cci) %>%
    rename(cci = cci_bin,
           date = charlson.date)

  updated_frame <- rows_upsert(td.frame, cci, by = c("pnr", "date")) %>%
    arrange(pnr, date) %>%
    group_by(pnr) %>%
    fill(region, income, education, marital, .direction = "down") %>%
    ungroup()

  cat(paste0("Complete - ", tockR("time"), "runtime: ", tockR("diff"), "\n"))

  cat(paste0("\nTotal runtime: \n"))
  cat(tockR("diff", start))

  updated_frame

}
