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
#' @param match.birth maximum birth date. Cohorts are established backwards based on n assuming 100.000 per cohort
#' @param match.index vector of length 2 with range of dates for indices
#' @param covariates.period vector of length 2 withrange of dates for covariate status
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
                      match.cases = n * 0.1,
                      match.birth = "1960-01-01",
                      match.index = c("1990-01-01", "2010-01-01"),
                      covariates.period = c("1980-01-01", "2025-01-01"),
                     format = "wide",
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

  if("match" %in% register) {


    (c <- data.frame(pnr = seq(1,match.cases),
                     case = 1,
                     index = sample(c(sample(seq(as.Date(match.index[1]), as.Date(match.index[2]), by="day"))), size = match.cases, replace=TRUE),
                     follow = c(sample(seq((as.Date(match.index[2]) + (365.25*5)), (as.Date(match.index[2]) + (365.25*10)), by="day"), match.cases, replace=T)),
                     birth = sample(sample(seq(as.Date(match.birth) - 365*(pmax(1, n/100000)-1), as.Date(match.birth), by="day")), size = match.cases, replace=TRUE),
                     skinc = sample(c(sample(seq((as.Date(match.index[2]) + (365.25*1)), (as.Date(match.index[2]) + (365.25*10)), by="day")),rep(as.Date(NA),8000)), size = match.cases, replace=TRUE),
                     imm_sup = sample(c(sample(seq((as.Date(match.index[2]) + (365.25*1)), (as.Date(match.index[2]) + (365.25*10)), by="day")),rep(as.Date(NA),8000)), size = match.cases, replace=TRUE)) %>%
       mutate(across(c(skinc, imm_sup), ~ if_else(. > follow | .< index, follow-100, .)),
              byear = str_sub(birth, end=4)))

    (cnt <- data.frame(pnr = seq(match.cases+1,n),
                       case = 0,
                       follow = c(sample(seq((as.Date(match.index[1])-(365.25*10)), (as.Date(match.index[2]) + (365.25*10)), by="day"), n-match.cases, replace=T)),
                       birth = sample(sample(seq(as.Date(match.birth) - 365*(pmax(1, n/100000)-1), as.Date(match.birth), by="day")), size = n-match.cases, replace=TRUE),
                       skinc = sample(c(sample(seq((as.Date(match.index[1])-(365.25*10)), (as.Date(match.index[2]) + (365.25*10)), by="day")),rep(as.Date(NA),8000)), size = n-match.cases, replace=TRUE),
                       imm_sup = sample(c(sample(seq((as.Date(match.index[1])-(365.25*10)), (as.Date(match.index[2]) + (365.25*10)), by="day")),rep(as.Date(NA),8000)), size = n-match.cases, replace=TRUE)) %>%
        mutate(across(c(skinc, imm_sup), ~ if_else(. > follow, follow-2000, .)),
               byear = str_sub(birth, end=4)))

    reglist[["match"]] <- bind_rows(c, cnt) %>%
      mutate(sex = sample(c("f", "m"), size = n, replace = TRUE),
             random1 = 1,
             random2 = 2)



  }

  if("covariates" %in% register) {

    intervals <- seq(as.Date(covariates.period[1]), as.Date(covariates.period[2]), by=365.25/2)

    vars <- list("cci" = c("cci_0",
                           "cci_1",
                           "cci_2-3",
                           "cci_4-5",
                           "cci_6+"),
                 "education" = c("low", "medium", "high"),
                 "income" = c("q1", "q2", "q3", "q4"),
                 "marital" = c("married",
                               "unmarried",
                               "divorced"),
                 "region" = c("capital",
                              "north",
                              "south",
                              "zealand",
                              "central"),
                 "degurba" = c("city",
                               "suburb",
                               "rural"))

    comorb <- c("infection",
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
                "congenital")


    if(format == "wide") {

    dat <- as.data.table(expand.grid(pnr = c(1:n),
                                     date = intervals))

    for(v in seq_along(vars)) {

      dat[, names(vars)[v] := sample(vars[[v]], size = nrow(dat), replace=TRUE)]

    }

    for(v in seq_along(comorb)) {

      dat[, comorb[v] := sample(c(1, NA), size = nrow(dat), replace=TRUE, prob = c(0.005, 1-0.005))]

    }

    dat <- dat[, (comorb) := lapply(.SD, function(x) { x[1] <- 0;x}),
               by = pnr,
               .SDcols = comorb][order(pnr)]

    setnafill(dat, "locf", cols=comorb)

    #Pseudo remove unaltered status
    reglist[["covariates"]] <- as.data.frame(dat[sort(sample(1:nrow(dat), size = nrow(dat)*0.9, replace=FALSE)),])


    }

    if(format == "long") {

      intervals <- seq(as.Date(covariates.period[1]), as.Date(covariates.period[2]), by="day")
      index <- sample(c(sample(seq(as.Date(match.index[1]), as.Date(match.index[2]), by="day"))), size = n, replace=TRUE)

      dat <- bind_rows(
        lapply(seq_len(n), function(i) {

          if(i %in% seq(0,n,500)) print(i)

        vframe <- rbindlist(
          lapply(seq_along(vars), function(v) {

            m <- rbinom(1, length(vars[[v]]), 1)

            data.frame(pnr = rep(i, m),
                       date = sort(sample(intervals, m)),
                       value = sample(1:m, m),
                       var = rep(names(vars)[[v]], m)) %>%
              filter(value >= cummax(value)) %>%
              mutate(value = unlist(sapply(value, function(s) {vars[[v]][[s]]})))

          })
        , fill=TRUE) %>% as.data.frame()

        m <- rbinom(1, length(comorb), 0.7)

        cframe <- data.frame(pnr=rep(i, m),
                             date = sort(sample(intervals, m)),
                             value = as.character(1),
                             var = sample(comorb, m))

        bind_rows(vframe, cframe) %>%
        arrange(date) %>%
        pivot_wider(values_from = value, names_from = var) %>%
        mutate(across(matches(paste0("\\b",comorb)), ~ ifelse(row_number()==1, "0", .)),
               income = ifelse(row_number() == 1, "q1", income),
               marital = ifelse(row_number() == 1, "unmarried", marital),
               cci = ifelse(row_number() == 1, "cci_0", cci),
               education = ifelse(row_number() == 1, "low", education)) %>%
        fill(everything(), .direction="downup") %>%
        filter(
          row_number() %in% c(1, n()) |
            if_any(c(2:ncol(.)), ~ . != lead(.))) %>%
        mutate(to = lead(date),
               to = if_else(row_number()==n(), as.Date("2026-01-01"), to),
               date = if_else(row_number()==1, as.Date("1900-01-01"), date)) %>%
        rename(from = date) %>%
        select(pnr, from, to, everything())



      })
      ) %>%
        mutate(across(matches(paste0("\\b",comorb)), ~ ifelse(is.na(.), "0", .)))




      reglist[["covariates"]] <- dat
    }
  }



  if(length(reglist) == 1) {

    return(reglist[[1]])
  } else {

    return(reglist)
  }

}

