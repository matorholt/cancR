#' loadR
#'
#' @description
#' Simple loading function of the most used registers including variable selection, regex filtering and id_list filtering.
#'
#' @param regs which registers should be loaded. Default is all (lpr, pop, pato, cancer, lmdb and opr)
#' @param keep which variables should be kept provided as a list("lpr" = c("vars")).
#' @param n number of observations that should be loaded
#' @param id.filter optional possibility to limit the registers to a defined patient population of PNRs
#' @param pattern.list regex patterns provided as a list("lpr" = c("DC92", "DC21"))
#' @param pattern.list2 supplemental pattern if multiple columns should be filtered. Works the same way as pattern
#' @param vars which columns should the pattern filter be applied to. Defaults to diag, atc, opr and c_morfo3,
#' @param lmdb.start first year of LMDB
#' @param lmdb.stop last year of LMDB
#'
#'
#' @return Returns the selected registers to the global environment
#' @export
#'
#'

loadR <- function(regs = c("lpr", "pop", "cancer", "lmdb", "opr"),
                      keep = NULL,
                      n = NULL,
                      id.filter = NULL,
                      pattern.list = NULL,
                      pattern.list2 = NULL,
                      vars = NULL,
                  lmdb.start = 1995,
                  lmdb.stop = 2023) {



  regs <- match.arg(regs, c("lpr", "pop", "cancer", "lmdb", "opr", "pato"), several.ok = T)

  if(is.null(n) & is.null(id.filter) & missing(keep) & missing(pattern.list)) {
  user.input <- readline(cat("Loading complete registers. Are you sure you want to run this? (yes/no)"))
  if(user.input != "yes")
    return(cat("Loading cancelled"))
    }

  if(!is.null(keep) & class(keep) != "list") {
    stop('Format the argument "keep" as a list with the structure list("lpr" = c("vars"), "lmdb" = c("vars"))')
  }

  if(!is.null(pattern.list) & class(pattern.list) != "list") {
    stop('Format the argument "pattern.list" as a list with the structure list("lpr" = c("DC92", "DC239"), "lmdb" = c("L04", "L01"))')
  }

  keep.default <-
    list("lpr" = c("pnr","diag","inddto"),
       "pop" = c("v_pnr", "c_status", "c_kon", "d_foddato", "d_status_hen_start"),
       "pato" = c("pnr", "k_matnr", "D_MODTDATO", "C_SNOMEDKODE"),
       "cancer" = c("pnr", "d_diagnosedato", "c_icd100", "c_morfo3"),
       "lmdb" = c("pnr", "eksd", "apk", "atc", "strnum", "strunit", "PACKSIZE"),
       "opr" = c("pnr", "opr", "inddto"))

  pattern.default <-
    list("lpr" = "",
         "pop" = "",
         "pato" = "",
         "cancer" = "",
         "lmdb" = "",
         "opr" = ""
    )

  pattern2.default <-
    list("lpr" = "",
         "pop" = "",
         "pato" = "",
         "cancer" = "",
         "lmdb" = "",
         "opr" = ""
    )

  vars.default <-
    list("lpr" = "diag",
         "pato" = "snomed",
         "cancer" = c("c_morfo3", "c_icd10"),
         "lmdb" = "atc",
         "opr" = "opr")

  if(!is.null(keep)) {
    keep.vars <- modifyList(keep.default, keep)
  } else {
    keep.vars <- keep.default
  }

  if(!is.null(pattern.list)) {

    pattern.list <-
      lapply(pattern.list, function(x) {

      paste0(x, collapse="|")

    })

    pattern.filter <- modifyList(pattern.default, pattern.list)

  } else {

    pattern.filter <- pattern.default
  }

  if(!is.null(pattern.list2)) {

    pattern.list2 <-
      lapply(pattern.list2, function(x) {

        paste0(x, collapse="|")

      })

    pattern2.filter <- modifyList(pattern2.default, pattern.list2)

  } else {

    pattern2.filter <- pattern2.default
  }

  if(!is.null(vars)) {
    vars.select <- modifyList(vars.default, vars)
  } else {
    vars.select <- vars.default
  }


  if("lpr" %in% regs) {
    if(is.null(n) & is.null(id.filter) & !("lpr" %in% names(keep)) & !("lpr" %in% names(pattern.list))) {
      lpr.df <<- readRDS("V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/LPR.rds")
    } else {
    lpr.df <<- importSAS("X:/Data/Rawdata_Hurtig/709545/Grunddata/LPR/diag_indl.sas7bdat",
                     obs = n,keep = keep.vars$lpr,
                     filter = id.filter,
                     where = paste0("prxmatch('/", pattern.filter$lpr, "/', diag)")
                     )
    }
  }

  if("pop" %in% regs) {
    pop.df <<- readRDS("V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/POPULATION.rds")

  }

  if("pato" %in% regs) {
    pato.df <<- readRDS("V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/PATOBANK.rds")

  }

  if("cancer" %in% regs) {
    if(is.null(n) & is.null(id.filter) & !("cancer" %in% names(keep)) & !("cancer" %in% names(pattern.list))) {
     cancer.df <<- readRDS("V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/CANCER.rds")
    } else {
      if(!is.null(pattern.list2)) {
        p <- paste0("prxmatch('/", pattern.filter$cancer, "/', ", vars.select$cancer[1], ") OR", paste0(" prxmatch('/", pattern2.filter$cancer, "/', ", vars.select$cancer[2], ")"))
      }
      else {
        p <- paste0("prxmatch('/", pattern.filter$cancer, "/', ", vars.select$cancer[1], ")")
      }

      cancer.df <<- importSAS("X:/Data/Rawdata_Hurtig/709545/Grunddata/Cancer/t_tumor.sas7bdat",
                           obs = n,
                           keep = keep.vars$cancer,
                           filter = id.filter,
                           where = p)
    }
  }

  if("lmdb" %in% regs) {
    if(is.null(n) & is.null(id.filter) & !("lmdb" %in% names(keep)) & !("lmdb" %in% names(pattern.list))) {
     lmdb.df <<- readRDS("V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/LMDB.rds")
    } else {
    lmdb.df <<- rbindlist(lapply(seq(lmdb.start,lmdb.stop), function(year) {
      importSAS(paste0("X:/Data/Rawdata_Hurtig/709545/Grunddata/medication/lmdb", year, "12.sas7bdat", sep=""),
                obs = n,
                keep = keep.vars$lmdb,
                filter = id.filter,
                where = paste0("prxmatch('/", pattern.filter$lmdb, "/', atc)"))
      }))
    }
  }

  if("opr" %in% regs) {
    if(is.null(n) & is.null(id.filter) & !("opr" %in% names(keep)) & !("opr" %in% names(pattern.list))) {
      opr.df <<- readRDS("V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/OPR.rds")
    } else {
    opr.df <<- importSAS("X:/Data/Rawdata_Hurtig/709545/Grunddata/LPR/opr.sas7bdat",
                     obs = n,
                     keep = keep.vars$opr,
                     filter = id.filter,
                     where = paste0("prxmatch('/", pattern.filter$opr, "/', opr)"))
    }
  }

}


