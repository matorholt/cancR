#' load_regs
#'
#' @description
#' Simple loading function of the most used registers including variable selection, regex filtering and id_list filtering.
#'
#' @param regs which registers should be loaded. Default is all (lpr, pop, pato, cancer, lmdb and opr)
#' @param keep which variables should be kept provided as a list("lpr" = c("vars")).
#' @param n number of observations that should be loaded
#' @param id_filter optional possibility to limit the registers to a defined patient population of PNRs
#' @param pattern regex patterns provided as a list("lpr" = "DC92|DC21")
#'
#' @return Returns the selected registers to the global environment
#' @export
#'
#'
load_regs <- function(regs = c("lpr", "pop", "pato", "cancer", "lmdb", "opr"),
                      keep,
                      n = NULL,
                      id_filter = NULL,
                      pattern) {

  regs <- match.arg(regs, several.ok = T)

  if(!missing(keep) & class(keep) != "list") {
    stop('Format the argument "keep" as a list with the structure list("lpr" = c("vars"), "lmdb" = c("vars"))')
  }

  if(!missing(pattern) & class(pattern) != "list") {
    stop('Format the argument "pattern" as a list with the structure list("lpr" = "DC92|DC239", "lmdb" = "L04|L01"))')
  }

  keep_default <-
    list("lpr" = c("PNR","diag","inddto"),
       "pop" = c("V_PNR", "C_STATUS", "C_KON", "D_FODDATO", "D_STATUS_HEN_START"),
       "pato" = c("pnr", "k_matnr", "D_MODTDATO", "C_SNOMEDKODE"),
       "cancer" = c("pnr", "D_DIAGNOSEDATO", "C_ICD10", "C_MORFO3"),
       "lmdb" = c("pnr", "eksd", "apk", "ATC", "strnum", "strunit", "PACKSIZE"),
       "opr" = c("pnr", "opr", "inddto"))

  pattern_default <-
    list("lpr" = "",
         "pop" = "",
         "pato" = "",
         "cancer" = "",
         "lmdb" = "",
         "opr" = ""
    )

  if(!missing(keep)) {
    keep <- modifyList(keep_default, keep)
  }

  if(!missing(pattern)) {
    pattern <- modifyList(pattern_default, pattern)
  }



  df_list <- list()

  if("lpr" %in% regs) {
    suppressWarnings(df_list[1] <-
                         importSAS("X:/Data/Rawdata_Hurtig/709545/Grunddata/LPR/diag_indl.sas7bdat",
                                   obs = n,
                                   keep = keep$lpr,
                                   filter = id_filter,
                                   where = paste0("prxmatch('/", pattern$lpr, "/', diag)")
                                   ))
    }

  if("pop" %in% regs) {
    suppressWarnings(df_list[2] <-
                       importSAS("X:/Data/Rawdata_Hurtig/709545/Grunddata/Population/t_person.sas7bdat",
                                 obs = n,
                                 keep = keep$pop,
                                 filter = id_filter,
                                 where = paste0("prxmatch('/", pattern$pop, "/', diag)")
                       ))
  }

  if("pato" %in% regs) {
    suppressWarnings(df_list[3] <-
                       readRDS("V:/Data/Workdata/709545/Mathias Ørholt/DATASETS/PATO.rds")
                       )
  }

  if("cancer" %in% regs) {
    suppressWarnings(df_list[4] <-
                       importSAS("X:/Data/Rawdata_Hurtig/709545/Grunddata/Cancer/t_tumor.sas7bdat",
                                 obs = n,
                                 keep = keep$cancer,
                                 filter = id_filter,
                                 where = paste0("prxmatch('/", pattern$cancer, "/', diag)")
                       ))
  }

  if("lmdb" %in% regs) {
    suppressWarnings(df_list[5] <-
                       rbindlist(lapply(seq(1995,2023), function(year) {
                         importSAS(paste0("X:/Data/Rawdata_Hurtig/709545/Grunddata/medication/lmdb", year, "12.sasbdat", sep=""),
                                   obs = n,
                                   keep = keep$lmdb,
                                   filter = id_filter,
                                   where = paste0("prxmatch('/", pattern$lmdb, "/', diag)"))

                       })))
  }

  if("opr" %in% regs) {
    suppressWarnings(df_list[6] <-
                       importSAS("X:/Data/Rawdata_Hurtig/709545/Grunddata/LPR/opr.sas7bdat",
                                 obs = n,
                                 keep = keep$opr,
                                 filter = id_filter,
                                 where = paste0("prxmatch('/", pattern$opr, "/', diag)")
                       ))
  }

  df_list <- df_list %>% discard(is.null)

  names(df_list) <- regs

  return(list2env(df_list, envir = globalenv()))
}

