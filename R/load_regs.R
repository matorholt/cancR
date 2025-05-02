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

load_regs <- function(regs = c("lpr", "pop", "cancer", "lmdb", "opr"),
                      keep = NULL,
                      n = NULL,
                      id_filter = NULL,
                      pattern = NULL) {

  regs <- match.arg(regs, c("lpr", "pop", "cancer", "lmdb", "opr", "pato"), several.ok = T)

  if(is.null(n) & is.null(id_filter) & missing(keep) & missing(pattern)) {
  user_input <- readline(cat("All filters are NULL. Are you sure you want to run this? (yes/no)"))
  if(user_input != "yes")
    return(cat("Load_regs cancelled"))
    }

  if(!is.null(keep) & class(keep) != "list") {
    stop('Format the argument "keep" as a list with the structure list("lpr" = c("vars"), "lmdb" = c("vars"))')
  }

  if(!is.null(pattern) & class(pattern) != "list") {
    stop('Format the argument "pattern" as a list with the structure list("lpr" = "DC92|DC239", "lmdb" = "L04|L01"))')
  }

  keep_default <-
    list("lpr" = c("pnr","diag","inddto"),
       "pop" = c("v_pnr", "c_status", "c_kon", "d_foddato", "d_status_hen_start"),
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

  if(!is.null(keep)) {
    keep <- modifyList(keep_default, keep)
  } else {
    keep <- keep_default
  }

  if(!is.null(pattern)) {
    pattern <- modifyList(pattern_default, pattern)
  } else {
    pattern <- pattern_default
    }


  if("lpr" %in% regs) {
    if(is.null(n) & is.null(id_filter) & missing(keep) & missing(pattern)) {
      lpr <<- readRDS("V:/Data/Workdata/709545/Mathias Ørholt/DATASETS/LPR.rds")
    } else {
    lpr <<- importSAS("X:/Data/Rawdata_Hurtig/709545/Grunddata/LPR/diag_indl.sas7bdat",
                     obs = n,keep = keep$lpr,
                     filter = id_filter,
                     where = paste0("prxmatch('/", pattern$lpr, "/', diag)")
                     )
    }
  }

  if("pop" %in% regs) {
    pop <<- readRDS("V:/Data/Workdata/709545/Mathias Ørholt/DATASETS/POP.rds")

  }

  if("pato" %in% regs) {
    pato <<- readRDS("V:/Data/Workdata/709545/Mathias Ørholt/DATASETS/PATO.rds")

  }

  if("cancer" %in% regs) {
    if(is.null(n) & is.null(id_filter) & missing(keep) & missing(pattern)) {
     cancer <<- readRDS("V:/Data/Workdata/709545/Mathias Ørholt/DATASETS/CANCER.rds")
    } else {
      cancer <<- importSAS("X:/Data/Rawdata_Hurtig/709545/Grunddata/Cancer/t_tumor.sas7bdat",
                           obs = n,
                           keep = keep$cancer,
                           filter = id_filter,
                           where = paste0("prxmatch('/", pattern$cancer, "/', c_morfo3)")
                           )
    }
  }

  if("lmdb" %in% regs) {
    if(is.null(n) & is.null(id_filter) & missing(keep) & missing(pattern)) {
     lmdb <- readRDS("V:/Data/Workdata/709545/Mathias Ørholt/DATASETS/LMDB.rds")
    } else {
    lmdb <<- rbindlist(lapply(seq(1995,2023), function(year) {
      importSAS(paste0("X:/Data/Rawdata_Hurtig/709545/Grunddata/medication/lmdb", year, "12.sasbdat", sep=""),
                obs = n,
                keep = keep$lmdb,
                filter = id_filter,
                where = paste0("prxmatch('/", pattern$lmdb, "/', atc)"))
      }))
    }
  }

  if("opr" %in% regs) {
    if(is.null(n) & is.null(id_filter) & missing(keep) & missing(pattern)) {
      opr <- readRDS("V:/Data/Workdata/709545/Mathias Ørholt/DATASETS/OPR.rds")
    } else {
    opr <- importSAS("X:/Data/Rawdata_Hurtig/709545/Grunddata/LPR/opr.sas7bdat",
                     obs = n,
                     keep = keep$opr,
                     filter = id_filter,
                     where = paste0("prxmatch('/", pattern$opr, "/', opr)"))
    }
  }

}

