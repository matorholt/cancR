#' Load registers
#'
#' @description
#' Simple loading function of the most used registers including variable selection, regex filtering and id_list filtering.
#'
#' @param regs which registers should be loaded. Default is all (lpr, pop, pato, cancer, lmdb and opr)
#' @param pattern.list named list of vectors of diagnoses codes for each register in the format ("lpr" = c("DC92", "DC21")). If multiple columns should be searched, an extra list layer is added ("lpr" = list("diag" = c("DC1"), "tildiag" = "DC2"))
#' @param pattern.custom named list for custom filter expressions
#' @param n number of observations that should be loaded
#' @param id.filter optional possibility to limit the registers to a defined patient population of PNRs
#' @param keep.list which variables should be kept provided as a list("lpr" = c("vars")).
#' @param vars.list which columns should the pattern filter be applied to. Defaults to diag, atc, opr and c_morfo3,
#' @param lmdb.start first year of LMDB
#' @param lmdb.stop last year of LMDB
#' @param simulation whether the registers should be simulated
#' @param cores number of cores for parallel processing
#' @param dt whether the returned data should be in data.table format
#' @param gb max size for future options
#' @param cancR.covariates which covariates that should be loaded. Options are: main (non-major), major (only major) and all.
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
                  pattern.custom = NULL,
                  n = NULL,
                  id.filter = NULL,
                  keep.list = NULL,
                  vars.list = NULL,
                  lmdb.start = 1995,
                  lmdb.stop = 2023,
                  simulation = F,
                  cores = 4,
                  dt = F,
                  gb = NULL,
                  cancR.covariates = "main",
                  ...) {


  tickR()

  start <- tickR.start

  cli::cli_h2("Initializing loadR algorithm: {tockR(\'time\')}")

  regs <- match.arg(regs, c("lpr", "lmdb", "pop", "pato", "cancer", "opr", "sc", "meta", "dsd", "covariates", "dcr", "immune"), several.ok = T)

  if(!is.null(keep.list) & class(keep.list) != "list") {
    return(cli::cli_alert_danger("Error: Format the argument \'keep\' as a list with the structure list(lpr = c(\'vars\'), lmdb = c(\'vars\'))"))
  }

  if(class(id.filter) %in% c("character", "numeric", "integer")) {

    id.filter <- data.frame(pnr = id.filter)

  }

  if(missing(simulation) & str_detect(getwd(), "V:|X:|~", negate=T)) {
    simulation <- T
    cli::cli_alert_warning("SIMULATION OF DATASETS")
  }


    pathlist <-
      list(
        "cancer" = "V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/CANCER.parquet",
        "opr" = "V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/OPR.parquet",
        "lpr" = "V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/LPR.parquet",
        "pop" = "V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/POPULATION.parquet",
        "pato" = "V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/PATO.parquet",
        "sc" = "V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/SKIN_CANCER.parquet",
        "meta" = "V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/SKIN_METASTASIS.parquet",
        "dsd" = "V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/SKIN_DEATH.parquet",
        "covariates" = "V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/COVARIATES.parquet",
        "dcr" = "V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/DCR.parquet",
        "immune" = "V:/Data/Workdata/709545/Mathias Oerholt/DATASETS/IMMUNE_DRUGS.parquet")

      keep.default <-
        list("pato" = c("pnr", "k_matnr", "D_MODTDATO", "C_SNOMEDKODE"),
             "cancer" = c("pnr", "d_diagnosedato", "c_icd10", "c_morfo3"),
             "lmdb" = c("pnr", "eksd", "apk", "atc", "strnum", "strunit", "PACKSIZE"))



      if("covariates" %in% regs) {
        base <- c("pnr", "from", "to", "education", "income", "region", "degurba", "marital", "cci", "cci_exact")

        switch(cancR.covariates,
               "major" = {keep.default[["covariates"]] <- c(base, names(cancR_codes)[str_detect(names(cancR_codes), "major")])},
               "main" = {keep.default[["covariates"]] <- c(base, names(cancR_codes)[str_detect(names(cancR_codes), "major", negate = T)])})
      }

        keep.vars <- list_assign(keep.default, !!!keep.list)

    vars.default <-
      list("lpr" = "diag",
           "pato" = "snomed",
           "cancer" = c("c_morfo3", "c_icd10"),
           "lmdb" = "atc",
           "opr" = "opr")
      vars.select <- list_assign(vars.default, !!!vars.list)

    #Patterns
      if(length(regs) == 1) {
        if(!is.null(pattern.list) & pluck_depth(pattern.list) < 3) {
          pattern.list <- list(pattern.list) %>% set_names(regs)
        }

        if(!is.null(pattern.custom) & pluck_depth(pattern.custom) < 3) {
          pattern.custom <- list(pattern.custom) %>% set_names(regs)
        }


      }


    pattern.list <-
      map(names(pattern.list), ~ {

        #If multiple columns in list
        if(pluck_depth(pattern.list[[.x]]) == 2) {

          paste0(
            map(names(pattern.list[[.x]]), function(i) {

            paste0("str_detect(", i, ", \'", paste0(pattern.list[[.x]][[i]], collapse="|"), "\')")


          }), collapse = " & ")



        } else {

        if(.x == "lmdb" & !simulation) {
          paste0("prxmatch(\'/", paste0(pattern.list[[.x]], collapse="|"), "/\', ", vars.select[[.x]][1], ")")
        } else {
          paste0("str_detect(", vars.select[[.x]], ", \'", paste0(pattern.list[[.x]], collapse = "|"), "\')")
        }
        }



      }) %>% set_names(names(pattern.list))


    if(length(regs[regs != "lmdb"]) > 0) {

    if(!inherits(plan(), "multisession") & !is.null(cores)) {

      multitaskR(cores = pmin(length(regs[regs != "lmdb"]), cores, gb))

      }

    #LOADING
    reglist <- future_map(regs[regs != "lmdb"], ~ {

      cli::cli_h3("Loading: {str_to_upper(.x)}")

      tickR.start <- Sys.time()

      if(simulation) {

        if(is.null(n)) n <- 10

        dat <- simulatR(.x, n = n)

      } else {

        dat <- arrow::open_dataset(pathlist[[.x]])

      }

        #SELECT
        if(.x %in% names(keep.vars)) {
          dat <- dat %>%
            select(keep.vars[[.x]])
        }

        #ID.FILTER
        if(!is.null(id.filter)) {

          dat <- dat %>%
            filter(pnr %in% id.filter$pnr)
        }

        #Filter
        if(.x %in% names(pattern.custom)) {

          dat <- dat %>%
            filter(eval(parse(text=pattern.custom[[.x]])))

        } else {

          if(.x %in% names(pattern.list)) {
            dat <- dat %>%
              filter(eval(parse(text=pattern.list[[.x]])))

          }
        }

        #OBSERVATIONS
        if(!is.null(n)) {

          dat <- dat %>%
            head(n = n)
        }


      cli::cli_alert_success("Complete: {tickR(cli=F)} - Runtime: {tockR(cli=F)}")

        if(dt) return(dat %>% collect %>% as.data.table) else return(dat %>% collect %>% as.data.frame)


      }, .options = furrr_options(seed = 1)) %>% set_names(regs[regs != "lmdb"])
    }


    if("lmdb" %in% regs) {
      if(!exists("reglist")) {
        reglist <- list()
      }



      tickR.start <- Sys.time()

      cli::cli_h3("Loading: LMDB")

      if(!inherits(plan(), "multisession") & !is.null(cores)) {

        multitaskR(cores = cores)
      }

      if(simulation) {

        if(is.null(n)) n <- 10

        dat <- simulatR("lmdb", n = n)

        if("lmdb" %in% names(pattern.custom)) {

          dat <- dat %>%
            filter(eval(parse(text=pattern.custom[["lmdb"]])))

        } else {

          if("lmdb" %in% names(pattern.list)) {
            dat <- dat %>%
              filter(eval(parse(text=pattern.list[["lmdb"]])))

          }
        }

      } else {

      dat <- rbindlist(future_map(seq(lmdb.start,lmdb.stop), function(year) {

        importSAS(paste0("X:/Data/Rawdata/709545/Grunddata/medication/lmdb", year, "12.sas7bdat", sep=""),
                  obs = n,
                  keep = keep.vars[["lmdb"]],
                  filter = id.filter,
                  where = pattern.list[["lmdb"]])

      }, .options = furrr_options(seed = 1)))
      }



      if(dt) {
        reglist[["lmdb"]] <- dat
      } else {

        reglist[["lmdb"]] <- as.data.frame(dat)
      }

      cli::cli_alert_success("Complete: {tickR(cli=F)} - Runtime: {tockR(cli=F)}")
    }

  cli::cli_h3("Loading complete!")
  cli::cli_text("Total runtime:")
  cli::cli_text(tockR("diff", start))

 if(length(reglist) == 1) return(reglist[[1]]) else return(reglist)

}
