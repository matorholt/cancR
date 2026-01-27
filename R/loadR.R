#' Load registers
#'
#' @description
#' Simple loading function of the most used registers including variable selection, regex filtering and id_list filtering.
#'
#' @param regs which registers should be loaded. Default is all (lpr, pop, pato, cancer, lmdb and opr)
#' @param pattern.list list of vectors of diagnoses codes for each register in the format ("lpr" = c("DC92", "DC21"))
#' @param pattern.list2 supplemental pattern if multiple columns should be filtered. Works the same way as pattern
#' @param n number of observations that should be loaded
#' @param id.filter optional possibility to limit the registers to a defined patient population of PNRs
#' @param keep.list which variables should be kept provided as a list("lpr" = c("vars")).
#' @param vars.list which columns should the pattern filter be applied to. Defaults to diag, atc, opr and c_morfo3,
#' @param lmdb.start first year of LMDB
#' @param lmdb.stop last year of LMDB
#' @param simulation whether the registers should be simulated
#' @param ... arguments passed to simulatR()
#'
#'
#' @return Returns the selected registers to the global environment
#' @export
#'
#'


# reglist <- loadR(c("lpr", "lmdb", "pato"),
#                  n=20,
#                  pattern.list = list("lpr" = c("DX1", "DZ2"),
#                                      "lmdb" = c("R0", "C10")))

loadR <- function(regs,
                  pattern.list = NULL,
                  pattern.list2 = NULL,
                  n = NULL,
                  id.filter = NULL,
                  keep.list = NULL,
                  vars.list = NULL,
                  lmdb.start = 1995,
                  lmdb.stop = 2023,
                  simulation = F,
                  cores = 4,
                  ...) {


  tickR()

  start <- tickR.start

  cat(paste0("\nInitializing loadR algorithm: ", tockR("time"), "\n\n"))

  regs <- match.arg(regs, c("lpr", "lmdb", "pop", "pato", "cancer", "opr", "sc", "meta", "dsd", "ses", "covariates"), several.ok = T)

  if(!is.null(keep.list) & class(keep.list) != "list") {
    stop('Format the argument "keep" as a list with the structure list("lpr" = c("vars"), "lmdb" = c("vars"))')
  }

  if(!is.null(pattern.list) & class(pattern.list) != "list") {
    stop('Format the argument "pattern.list" as a list with the structure list("lpr" = c("DC92", "DC239"), "lmdb" = c("L04", "L01"))')
  }

  if(class(id.filter) %in% c("character", "numeric", "integer")) {

    id.filter <- data.frame(pnr = id.filter)

  }

  if(missing(simulation) & str_detect(getwd(), "V:|X:", negate=T)) {
    simulation <- T
  }

  if(simulation) {

    if(is.null(n)) n <- 10
    start.date <- "2000-01-01"

    reglist <- list()

    for(i in regs) {

      tickR()

      cat(paste0(i,": "))

      reglist[[i]] <- simulatR(i,
                               n = n,
                               start.date = start.date,
                               pattern.list,
                               ...)

      cat(paste0("Completed - ", tockR("time"), ", runtime: ", tockR("diff"), "\n"))

    }

    cat(paste0("\nTotal runtime: \n"))
    cat(paste0(tockR("diff", start), "\n\n"))

    if(length(reglist) == 1) {

      return(reglist[[1]])
    } else {

      return(reglist)
    }

  }

  else {

  pathlist <-
    list("parquet" = list("lpr" = "V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/LPR.parquet",
                      "pop" = "V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/POPULATION.rds",
                      "pato" = "V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/PATOBANK.parquet",
                      "cancer" = "V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/CANCER.parquet",
                      "opr" = "V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/OPR.parquet",
                      "sc" = "V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/SKIN_CANCER.rds",
                      "meta" = "V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/SKIN_METASTASIS.rds",
                      "dsd" = "V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/SKIN_DEATH.rds",
                      "ses" = "V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/SES_wide.rds",
                      "covariates" = "V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/TIME_COVARIATES.parquet"),
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

      reglist[[i]] <- readRDS(pathlist[["parquet"]][[i]])

    } else if(i == "pato") {

      dat <- readRDS(pathlist[["parquet"]][[i]])

      setDT(dat)

      reglist[[i]] <- as.data.frame(dat[str_detect(snomed, paste0(pattern.list[[i]], collapse="|")),])


    } else if(i == "lmdb") {
      cl <- parallel::makeCluster(cores)
      registerDoParallel(cl)

      reglist[[i]] <- as.data.frame(rbindlist(parLapply(cl, seq(lmdb.start,lmdb.stop), function(year) {
        heaven::importSAS(paste0("X:/Data/Rawdata_Hurtig/709545/Grunddata/medication/lmdb", year, "12.sas7bdat", sep=""),
                  obs = n,
                  keep = keep.vars[[i]],
                  filter = id.filter,
                  where = pattern)
      })))
      parallel::stopCluster(cl)

    } else {

      reglist[[i]] <-
        as.data.frame(
        importSAS(pathlist[["sas"]][[i]],
                  obs = n,
                  keep = keep.vars[[i]],
                  filter = id.filter,
                  where = pattern))

    }

    cat(paste0("Completed - ", tockR("time"), ", runtime: ", tockR("diff"), "\n"))

  }

  cat(paste0("\nTotal runtime: \n"))
  cat(paste0(tockR("diff", start), "\n\n"))

  if(length(reglist) == 1) {

    return(as.data.frame(reglist[[1]]))
  } else {

    return(reglist)
  }

  }

}
