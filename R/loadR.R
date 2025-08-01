#' loadR
#'
#' @description
#' Simple loading function of the most used registers including variable selection, regex filtering and id_list filtering.
#'
#' @param regs which registers should be loaded. Default is all (lpr, pop, pato, cancer, lmdb and opr)
#' @param pattern.list regex patterns provided as a list("lpr" = c("DC92", "DC21"))
#' @param pattern.list2 supplemental pattern if multiple columns should be filtered. Works the same way as pattern
#' @param n number of observations that should be loaded
#' @param id.filter optional possibility to limit the registers to a defined patient population of PNRs
#' @param keep.list which variables should be kept provided as a list("lpr" = c("vars")).
#' @param vars.list which columns should the pattern filter be applied to. Defaults to diag, atc, opr and c_morfo3,
#' @param lmdb.start first year of LMDB
#' @param lmdb.stop last year of LMDB
#'
#'
#' @return Returns the selected registers to the global environment
#' @export
#'
#'

loadR <- function(regs,
                  pattern.list = NULL,
                  pattern.list2 = NULL,
                  n = NULL,
                  id.filter = NULL,
                  keep.list = NULL,
                  vars.list = NULL,
                  lmdb.start = 1995,
                  lmdb.stop = 2023) {

  tickR()

  tickR.first <- tickR.start

  cat(paste0("\nInitializing loadR algorithm: ", tockR("time"), "\n\n"))

  regs <- match.arg(regs, c("lpr", "lmdb", "pop", "pato", "cancer", "opr", "sc", "meta", "dsd", "ses"), several.ok = T)

  if(is.null(n) & is.null(id.filter) & missing(keep.list) & missing(pattern.list)) {
    user.input <- readline(cat("Loading complete registers. Are you sure you want to run this? (yes/no)"))
    if(user.input != "yes")
      return(cat("Loading cancelled"))
  }

  if(!is.null(keep.list) & class(keep.list) != "list") {
    stop('Format the argument "keep" as a list with the structure list("lpr" = c("vars"), "lmdb" = c("vars"))')
  }

  if(!is.null(pattern.list) & class(pattern.list) != "list") {
    stop('Format the argument "pattern.list" as a list with the structure list("lpr" = c("DC92", "DC239"), "lmdb" = c("L04", "L01"))')
  }

  pathlist <-
    list("rds" = list("lpr" = "V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/LPR.rds",
                      "lmdb" = "V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/LMDB.rds",
                      "pop" = "V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/POPULATION.rds",
                      "pato" = "V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/PATOBANK.rds",
                      "cancer" = "V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/CANCER.rds",
                      "opr" = "V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/OPR.rds",
                      "sc" = "V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/SKIN_CANCER.rds",
                      "meta" = "V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/SKIN_METASTASIS.rds",
                      "dsd" = "V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/SKIN_DEATH.rds",
                      "ses" = "V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/SES_wide.rds"),
         "sas" = list("lpr" = "X:/Data/Rawdata_Hurtig/709545/Grunddata/LPR/diag_indl.sas7bdat",
                      "cancer" = "X:/Data/Rawdata_Hurtig/709545/Grunddata/Cancer/t_tumor.sas7bdat",
                      "opr" = "X:/Data/Rawdata_Hurtig/709545/Grunddata/LPR/opr.sas7bdat"))


  keep.default <-
    list("lpr" = c("pnr","diag","inddto"),
         "pop" = c("v_pnr", "c_status", "c_kon", "d_foddato", "d_status_hen_start"),
         "pato" = c("pnr", "k_matnr", "D_MODTDATO", "C_SNOMEDKODE"),
         "cancer" = c("pnr", "d_diagnosedato", "c_icd10", "c_morfo3"),
         "lmdb" = c("pnr", "eksd", "apk", "atc", "strnum", "strunit", "PACKSIZE"),
         "opr" = c("pnr", "opr", "inddto"))

  vars.default <-
    list("lpr" = "diag",
         "pato" = "snomed",
         "cancer" = c("c_morfo3", "c_icd10"),
         "lmdb" = "atc",
         "opr" = "opr")

  if(!is.null(keep.list)) {
    keep.vars <- modifyList(keep.default, keep.list)
  } else {
    keep.vars <- keep.default
  }

  if(!is.null(vars.list)) {
    vars.select <- modifyList(vars.default, vars.list)
  } else {
    vars.select <- vars.default
  }

  reglist <- list()

  for(i in regs) {

    tickR()

    cat(paste0(i,": "))

    if(i %in% names(pattern.list2)) {
      pattern <- paste0("prxmatch('/", paste0(pattern.list[[i]], collapse="|"), "/', ", vars.select$cancer[1], ") OR prxmatch('/", paste0(pattern.list2[[i]], collapse="|"), "/', ", vars.select[[i]][2], ")")
    } else if(i %in% names(pattern.list)) {
      pattern <- paste0("prxmatch('/", paste0(pattern.list[[i]], collapse="|"), "/', ", vars.select[[i]][1], ")")
    } else {
      pattern <- NULL
    }

    if((is.null(n) & is.null(id.filter) & !(i %in% names(keep.list)) & !(i %in% names(pattern.list))) | i %in% c("pop", "sc", "meta", "dsd", "ses")) {

      reglist[[i]] <- readRDS(pathlist[["rds"]][[i]])

    } else if(! == "pato") {

      dat <- readRDS(pathlist[["rds"]][[i]])

      setDT(dat)

      reglist[[i]] <- dat[str_detect(vars.select[[i]], paste0(pattern.list[[i]], collapse="|")),]


    } else if(i == "lmdb") {

      reglist[[i]] <- rbindlist(lapply(seq(lmdb.start,lmdb.stop), function(year) {
        importSAS(paste0("X:/Data/Rawdata_Hurtig/709545/Grunddata/medication/lmdb", year, "12.sas7bdat", sep=""),
                  obs = n,
                  keep = keep.vars[[i]],
                  filter = id.filter,
                  where = pattern)
      }))

    } else {

      reglist[[i]] <-
        importSAS(pathlist[["sas"]][[i]],
                  obs = n,
                  keep = keep.vars[[i]],
                  filter = id.filter,
                  where = pattern)

    }

    cat(paste0("Completed - ", tockR("time"), ", runtime: ", tockR("diff"), "\n"))

  }

  tickR.start <- tickR.first

  cat(paste0("\nTotal runtime: \n"))
  cat(tockR("diff"))

  if(length(reglist) == 1) {

    return(reglist[[1]])
  } else {

    return(reglist)
  }


}
