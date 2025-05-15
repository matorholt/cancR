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
#' @param pattern2 supplemental pattern if multiple columns should be filtered. Works the same way as pattern
#' @param vars which columns should the pattern filter be applied to. Defaults to diag, atc, opr and c_morfo3,
#'
#'
#' @return Returns the selected registers to the global environment
#' @export
#'
#'

load_regs <- function(regs = c("lpr", "pop", "cancer", "lmdb", "opr"),
                      keep = NULL,
                      n = NULL,
                      id_filter = NULL,
                      pattern = NULL,
                      pattern2 = NULL,
                      vars = NULL) {



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
       "cancer" = c("pnr", "d_diagnosedato", "c_icd100", "c_morfo3"),
       "lmdb" = c("pnr", "eksd", "apk", "atc", "strnum", "strunit", "PACKSIZE"),
       "opr" = c("pnr", "opr", "inddto"))

  pattern_default <-
    list("lpr" = "",
         "pop" = "",
         "pato" = "",
         "cancer" = "",
         "lmdb" = "",
         "opr" = ""
    )

  pattern2_default <-
    list("lpr" = "",
         "pop" = "",
         "pato" = "",
         "cancer" = "",
         "lmdb" = "",
         "opr" = ""
    )

  vars_default <-
    list("lpr" = "diag",
         "pato" = "snomed",
         "cancer" = c("c_morfo3", "c_icd10"),
         "lmdb" = "atc",
         "opr" = "opr")

  if(!is.null(keep)) {
    keep_vars <- modifyList(keep_default, keep)
  } else {
    keep_vars <- keep_default
  }

  if(!is.null(pattern)) {
    pattern_filter <- modifyList(pattern_default, pattern)
  } else {
    pattern_filter <- pattern_default
  }

  if(!is.null(pattern2)) {
    pattern2_filter <- modifyList(pattern2_default, pattern2)
  } else {
    pattern2_filter <- pattern2_default
  }

  if(!is.null(vars)) {
    vars_select <- modifyList(vars_default, vars)
  } else {
    vars_select <- vars_default
  }


  if("lpr" %in% regs) {
    if(is.null(n) & is.null(id_filter) & !("lpr" %in% names(keep)) & !("lpr" %in% names(pattern))) {
      lpr_df <<- readRDS("V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/LPR.rds")
    } else {
    lpr_df <<- importSAS("X:/Data/Rawdata_Hurtig/709545/Grunddata/LPR/diag_indl.sas7bdat",
                     obs = n,keep = keep_vars$lpr,
                     filter = id_filter,
                     where = paste0("prxmatch('/", pattern_filter$lpr, "/', diag)")
                     )
    }
  }

  if("pop" %in% regs) {
    pop_df <<- readRDS("V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/POPULATION.rds")

  }

  if("pato" %in% regs) {
    pato_df <<- readRDS("V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/PATOBANK.rds")

  }

  if("cancer" %in% regs) {
    if(is.null(n) & is.null(id_filter) & !("cancer" %in% names(keep)) & !("cancer" %in% names(pattern))) {
     cancer_df <<- readRDS("V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/CANCER.rds")
    } else {
      if(!is.null(pattern2)) {
        p <- paste0("prxmatch('/", pattern_filter$cancer, "/', ", vars_select$cancer[1], ") OR", paste0(" prxmatch('/", pattern2_filter$cancer, "/', ", vars_select$cancer[2], ")"))
      }
      else {
        p <- paste0("prxmatch('/", pattern_filter$cancer, "/', ", vars_select$cancer[1], ")")
      }

      cancer_df <<- importSAS("X:/Data/Rawdata_Hurtig/709545/Grunddata/Cancer/t_tumor.sas7bdat",
                           obs = n,
                           keep = keep_vars$cancer,
                           filter = id_filter,
                           where = p)
    }
  }

  if("lmdb" %in% regs) {
    if(is.null(n) & is.null(id_filter) & !("lmdb" %in% names(keep)) & !("lmdb" %in% names(pattern))) {
     lmdb_df <<- readRDS("V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/LMDB.rds")
    } else {
    lmdb_df <<- rbindlist(lapply(seq(1995,2023), function(year) {
      importSAS(paste0("X:/Data/Rawdata_Hurtig/709545/Grunddata/medication/lmdb", year, "12.sas7bdat", sep=""),
                obs = n,
                keep = keep_vars$lmdb,
                filter = id_filter,
                where = paste0("prxmatch('/", pattern_filter$lmdb, "/', atc)"))
      }))
    }
  }

  if("opr" %in% regs) {
    if(is.null(n) & is.null(id_filter) & !("opr" %in% names(keep)) & !("opr" %in% names(pattern))) {
      opr_df <<- readRDS("V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/OPR.rds")
    } else {
    opr_df <<- importSAS("X:/Data/Rawdata_Hurtig/709545/Grunddata/LPR/opr.sas7bdat",
                     obs = n,
                     keep = keep_vars$opr,
                     filter = id_filter,
                     where = paste0("prxmatch('/", pattern_filter$opr, "/', opr)"))
    }
  }

}


