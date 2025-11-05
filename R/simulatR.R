#' Simulate danish health registers
#'
#' @description
#' A wrapper for the simulation functions in the heaven package.
#'
#'
#' @param register vector of the registers to simulate. Choose between "lpr", "lmdb", "opr", "pop", "pato", "match" and "covariates".
#' @param n number of unique pnrs to simulate
#' @param start.date starting date of the register
#' @param pattern.list list of vectors of diagnoses codes for each register in the format ("lpr" = c("DC92", "DC21"))
#' @param lpr.diag.count maximum number of diagnoses per patient in LPR
#' @param lmdb.max.prescriptions maximum number of prescriptions per patient in LMDB
#' @param lmdb.max.packages maximum number of packages purchased per day
#' @param opr.diag.count maximum number of diagnoses per patient in OPR
#' @param pop.min.age minmmum age in the population
#' @param pop.max.age maximum age in the population
#' @param pop.sex distribution of sex in the population, default = 0.5
#' @param pop.mortality mortality rate in the population, default = 0.1
#' @param match.cases number of cases for the "match" dataset
#' @param seed for reproducibility
#'
#' @returns a single data frame or named list of data frames with simulated registers
#' @export
#'
#'

# #Simple
#simulatR(c("lpr", "lmdb"))
#
# simulatR(c("lpr", "lmdb", "opr", "pato"),
#          n = 20,
#          pattern.list = list("lpr" = c("DX1", "DZ2"),
#                              "lmdb" = c("C10", "R10"),
#                              "opr" = c("KZ123"),
#                              "pato" = list("t.codes" = c("T123", "T234"),
#                                            "m.codes" = c("M80"))))

simulatR <- function(register,
                     n = 10,
                     start.date = "2000-01-01",
                     pattern.list = list(),
                     lpr.diag.count = 5,
                     lmdb.max.prescriptions = 20,
                     lmdb.max.packages  = 3,
                     opr.diag.count = 5,
                     pop.min.age = 20,
                     pop.max.age = 100,
                     pop.sex = 0.5,
                     pop.mortality = 0.1,
                     match.cases,
                     seed = 1) {


  set.seed(seed)

  reglist <- list()



  if("lpr" %in% register) {

    if("lpr" %in% names(pattern.list)) {
      lpr.codes <- pattern.list[["lpr"]]

    } else {
    lpr.codes <- paste0("D", sample(LETTERS, size = 50, replace=TRUE), str_pad(round(runif(50, 0,999),0), width = 3, pad = "0", side = "left"))
    }

    reglist[["lpr"]] <- simAdmissionData(n,
                            m = lpr.diag.count,
                            diagnoses = lpr.codes,
                            startDate = start.date) %>%
      as.data.frame()

  }



  if("lmdb" %in% register) {

    if("lmdb" %in% names(pattern.list)) {
      lmdb.vector <- pattern.list[["lmdb"]]

    } else {

      lmdb.vector <- paste0(sample(c("A", "B", "C", "D", "G", "H", "J", "L", "M", "N", "P", "R", "S", "V"), size = 20, replace=TRUE),
                            str_pad(sample(seq(0,99), size = 20), width = 2, side = "left", pad = "0"),
                            str_pad(sample(seq(0,999), size = 20), width = 2, side = "left", pad = "0"))

    }

    lmdb.drugs <- list()

    for(i in lmdb.vector) {

      rows <- sample(c(1:5), size=1)

      i_list <- list()
      for(r in c(1:rows)) {

        i_list[[r]] <- c(sample(seq(10,300, 50), size = 1), sample(seq(100,1000,100), size = 1))


      }

      lmdb.drugs[[i]] <- i_list

    }


    reglist[["lmdb"]] <- simPrescriptionData(n = n,
                               max.prescriptions=lmdb.max.prescriptions,
                               packages = lmdb.drugs,
                               max.packages = lmdb.max.packages,
                               startDate = start.date) %>%
      as.data.frame()


  }



  if("opr" %in% register) {

    if("opr" %in% names(pattern.list)) {
      opr.codes <- pattern.list[["opr"]]

    } else {
      opr.codes <- paste0("K", sample(LETTERS, 50, replace=T), str_pad(round(runif(50, 1,4000),0), width = 4, side = "left", pad = "0"))
    }

    reglist[["opr"]] <- simAdmissionData(n,
                            m = opr.diag.count,
                            diagnoses = opr.codes,
                            startDate = start.date) %>%
      rename(opr = diag) %>%
      select(pnr, opr, inddto) %>%
      as.data.frame()


  }



  if("pop" %in% register) {

    reglist[["pop"]] <- simPop(n,
                  min.age = pop.min.age,
                  max.age = pop.max.age,
                  sex = pop.sex,
                  mortality = pop.mortality) %>%
      as.data.frame()


  }



  if("pato" %in% register) {

      code_cnt=100

      if("t.codes" %in% names(pattern.list[["pato"]])) {
        t.codes <- pattern.list[["pato"]][["t.codes"]]
      } else {
      t.codes <-
        paste0("T",
               sample(c("02", "03", "04", "08", "y", "x", seq(10,99,1)), n, replace=TRUE),
               str_pad(round(runif(20, 1,400),0), width = 3, side = "left", pad = "0"))
      }

      if("m.codes" %in% names(pattern.list[["pato"]])) {
        m.codes <- pattern.list[["pato"]][["m.codes"]]
      } else {
      m.codes <-
        paste0("M",
               sample(seq(80,99,1), n, replace=TRUE),
               str_pad(round(runif(n, 1,99),0), width = 2, side = "left", pad = "0"),
               sample(c("0","1","2","3","4","6","7","9","x"), size = n, replace=TRUE))
      }

      if("p.codes" %in% names(pattern.list[["pato"]])) {
        p.codes <- pattern.list[["pato"]][["p.codes"]]
      } else {
      p.codes <-
        paste0("P",
               str_pad(sample(seq(0,30,1), n, replace=TRUE), width= 2, side = "left", pad = "0"),
               str_pad(round(runif(n, 1,99),0), width = 3, side = "left", pad = "0"))
      }

      dflist <- NULL

      for(i in c(1:n)) {

        rows <- pmax(1, rbinom(1, 20, 0.2))

        ind <- as.Date(start.date) + floor(runif(rows,0,20*365.25))


        s_codes <- NULL

        for(c in c(1:rows)) {

          codes <-
            sample(c(1:2), size = 3, replace = T)
          s_codes <- c(s_codes, paste0(c(sample(t.codes, codes[1], replace=TRUE),
                                         sample(m.codes, codes[2], replace=TRUE),
                                         sample(p.codes, codes[3], replace=TRUE)), collapse = " "))

        }

        rows <- length(s_codes)

        dflist <- bind_rows(dflist,
                            data.frame(pnr = i,
                                       snomed = s_codes,
                                       date = ind))

      }

      reglist[["pato"]] <- dflist %>% arrange(pnr, date)


  }

  if(register == "match") {

    (c <- data.frame(pnr = seq(1,match.cases),
                     case = 1,
                     index = sample(c(sample(seq(as.Date('1990/01/01'), as.Date('2010/01/01'), by="day"))), size = match.cases, replace=TRUE),
                     follow = c(sample(seq(as.Date('2015/01/01'), as.Date('2020/01/01'), by="day"), match.cases, replace=T)),
                     birth = sample(c(sample(seq(as.Date('1958/01/01'), as.Date('1961/01/01'), by="day"))), size = match.cases, replace=TRUE),
                     byear = sample(c(seq(1958,1961)), match.cases, replace=T),
                     sex = sample(c("f","m"), match.cases, replace=T),
                     skinc = sample(c(sample(seq(as.Date('2011/01/01'), as.Date('2020/01/01'), by="day")),rep(as.Date(NA),8000)), size = match.cases, replace=TRUE),
                     imm_sup = sample(c(sample(seq(as.Date('2011/01/01'), as.Date('2020/01/01'), by="day")),rep(as.Date(NA),8000)), size = match.cases, replace=TRUE),
                     random1 = 1,
                     random2 = 2) %>%
       mutate(across(c(skinc, imm_sup), ~ if_else(. > follow | .< index, follow-100, .))))

    (cnt <- data.frame(pnr = seq(1,n),
                       case = 0,
                       follow = c(sample(seq(as.Date('1980/01/01'), as.Date('2020/01/01'), by="day"), n, replace=T)),
                       birth = sample(c(sample(seq(as.Date('1958/01/01'), as.Date('1961/01/01'), by="day"))), size = match.cases, replace=TRUE),
                       byear = sample(c(seq(1958,1961)), n, replace=T),
                       sex = sample(c("f","m"), n, replace=T),
                       skinc = sample(c(sample(seq(as.Date('1985/01/01'), as.Date('2000/01/01'), by="day")),rep(as.Date(NA),8000)), size = n, replace=TRUE),
                       imm_sup = sample(c(sample(seq(as.Date('1985/01/01'), as.Date('2000/01/01'), by="day")),rep(as.Date(NA),8000)), size = n, replace=TRUE),
                       random1 = 1,
                       random2 = 2) %>%
        mutate(across(c(skinc, imm_sup), ~ if_else(. > follow, follow-2000, .))))

    reglist[["match"]] <- bind_rows(c, cnt)



  }

  if(register == "covariates") {

    intervals <- seq(as.Date(start.date), as.Date('2024/01/01'), by=365.25/2)

    reglist[["covariates"]] <-
      bind_rows(lapply(seq(1,n), function(i) {

        bind_rows(
          #CCI
          data.frame(pnr = i,
                     date = intervals,
                     var = "cci",
                     value = sample(c("cci_0",
                                      "cci_1",
                                      "cci_2-3",
                                      "cci_4-5",
                                      "cci_6+"), size = length(intervals), replace=TRUE)),
          #Education
          data.frame(pnr = i,
                     date = intervals,
                     var = "education",
                     value = sample(c("low", "medium", "high"), size = length(intervals), replace=TRUE)),

          #Income
          data.frame(pnr = i,
                     date = intervals,
                     var = "income",
                     value = sample(c("q1", "q2", "q3", "q4"), size = length(intervals), replace=TRUE)),

          #marital
          data.frame(pnr = i,
                     date = intervals,
                     var = "marital",
                     value = sample(c("married",
                                      "unmarried",
                                      "divorced"), size = length(intervals), replace=TRUE)),

          #region
          data.frame(pnr = i,
                     date = intervals,
                     var = "region",
                     value = sample(c("capital",
                                      "north",
                                      "south",
                                      "zealand",
                                      "central"), size = length(intervals), replace=TRUE)),

          #degurba
          data.frame(pnr = i,
                     date = intervals,
                     var = "degurba",
                     value = sample(c("city",
                                      "suburb",
                                      "rural"), size = length(intervals), replace=TRUE)),

          #Comorbidities
          bind_rows(lapply(c("infection",
                             "cancer",
                             "hema",
                             "endo",
                             "psych",
                             "neuro",
                             "cvd",
                             "lungs",
                             "gi",
                             "skin",
                             "connective",
                             "urinary",
                             "congenital"), function(v) {

                               data.frame(pnr = i,
                                          date = intervals,
                                          var = v,
                                          value = as.character(cummax(rbinom(length(intervals), 1, 0.1))))



                             }))


        )



      })) %>%
      group_by(pnr, var) %>%
      filter(value != lag(value) | row_number() == 1) %>%
      pivot_wider(names_from=var, values_from=value) %>%
      fill(everything(), .direction = "down")

  }



  if(length(reglist) == 1) {

    return(reglist[[1]])
  } else {

    return(reglist)
  }

}
