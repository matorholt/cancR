#' includR
#'
#' @description
#' Apply inclusion criteria more conveniently
#'
#'
#' @param data joined data frame
#' @param exclusion.ex vector of exposure variables that should not be present before index
#' @param exclusion.out vector of outcome variables that should not be present before index
#' @param subgroup filter for subgroups in the format: "level %in% c('a', 'b')" Can also be applied as custom filter
#' @param age.limit age limit for inclusion as numeric value
#' @param period enrollement period in the format: c("1980-01-01", "2022-01-01")
#' @param fu name of the follow-up date (default: fu)
#' @param birth name of the birth date variable (default: birth)
#' @param export whether a data frame of the flowchart should be exported
#'
#' @return A data frame with filtered cases and all controls + an rds file for flow chart
#' @export
#'


# set.seed(1)
#
# n=500
#
# reglist <- list(lpr = simAdmissionData(n=n, m=10),
#             opr = simAdmissionData(n=n, m=10) %>% rename(opr = diag),
#             lmdb = simPrescriptionData(n=n),
#             pop = simPop(n*10),
#             sc = data.frame(pnr = sample(seq(1,n*10), n*10*0.1, replace=F),
#                             sc_date = sample(c(sample(seq(as.Date('1990/01/01'), as.Date('2020/01/01'), by="day"))), n*10*0.1, replace=TRUE),
#                             meta_date = sample(c(sample(seq(as.Date('1990/01/01'), as.Date('2020/01/01'), by="day"))), n*10*0.1, replace=TRUE),
#                             pato_supp = sample(c(sample(seq(as.Date('1990/01/01'), as.Date('2020/01/01'), by="day"))), n*10*0.1, replace=TRUE)))
#
# reglist <- lapply(reglist, as.data.frame)
#
# clist <- decodR(list("lpr_case" =
#                        list(supergroup_a =
#                               list(group_a1 = list("sg1" = c("DB6", "DB7"),
#                                                   "sg2" = c("DD22", "DD23")),
#                                    group_b1 = list("sg3" = c("DD4"))),
#
#                             supergroup_b =
#                               list(group_a2 = list("sg4" = c("DE5", "DF"),
#                                                    "sg5" = c("DF", "DG"))),
#
#                             supergroup_c =
#                               list(group_a3 = list("sg6" = c("DJ", "DK"),
#                                                    "sg7" = c("DL", "DM")),
#                                    group_b3 = list("sg8" = c("DN")))),
#
#                      "lpr_ex" = list("e1" = c("DO", "DP"),
#                                      "c2" = c("DR", "DQ")),
#                      "lmdb_ex" = list("immune" = "C0"),
#                      "opr_ex" = list("sotr" = c("DT", "DG", "DK")),
#                      "labels" = list("lpr_case" = c("sg_level", "g_level", "sub_level"),
#                                      "lpr_ex" = "immsup"),
#                      "exclusion" = c("DQ","ZZ2")))
#
#
# indices <- searchR(reglist,
#         clist$searchR.list,
#         sub.list = clist$searchR.keep,
#         sub.labels = clist$recodR.labels,
#         exclusion = clist$searchR.exclusion)
#
# pre_join <- plyr::join_all(list(reglist$pop,
#                                 indices,
#                                 reglist$sc))
#
# data <- pre_join
#
# i <- includR(pre_join,
#              exclusion.ex = c(lpr_ex, lmdb_ex, opr_ex),
#              exclusion.out = c(sc_date, meta_date),
#              subgroup = "sg_level %in% c('supergroup_a', 'supergroup_b') | is.na(sg_level)",
#              age.limit = 18,
#              period = c("1990-01-01", "2022-01-01"),
#              fu = doddate,
#              birth = birthdate,
#              export = F)



includR <- function(data,
                    exclusion.ex,
                    exclusion.out,
                    subgroup = NULL,
                    age.limit = NULL,
                    period = NULL,
                    fu=fu,
                    birth = birth,
                    export = F) {

  cat("\nincludR initialized: ", tickR(), "\n")

  exe_c <- data %>% select({{exclusion.ex}}) %>% names()
    exo_c <- data %>% select({{exclusion.out}}) %>% names()
    fu_c <- data %>% select({{fu}}) %>% names()
    birth_c <- data %>% select({{birth}}) %>% names()


    flow_list <- list()

#split
setDT(data)

if(!missing(exclusion.ex)) {

data[, exclusion_date := do.call(pmin, c(.SD, list(na.rm=TRUE))),
    .SDcols= c(exe_c)][
      , byear := str_extract(get(birth_c), "\\d{4}")]


}

flow_list[["entry"]][["main"]] <- paste0("- / ", nrow(data[!is.na(data[["index"]]),]))

if(any(str_detect(names(data), "supp"))) {

supp_c <- data %>% select(contains("supp")) %>% names()

data[,index := pmin(index, get(supp_c), na.rm=T)]

flow_list[["entry"]][["supp"]] <- paste0("- / ", nrow(data[!is.na(data[[supp_c]]),]))

flow_list[["entry"]][["total"]] <- paste0("- / ", nrow(data[!is.na(data[["index"]]),]))

}

data[, case := ifelse(!is.na(index), 1, 0)]

#Split
cases <- data[case == 1,]
conts <- data[case == 0,]

cur_n <- nrow(cases)

cases <- cases[get(fu_c) > index,]

flow_list[["entry"]][["autopsy"]] <- paste0(cur_n - nrow(cases), " / ", nrow(cases))

cur_n <- nrow(cases)


#Subgroup
if(!is.null(subgroup)) {

  if(any(str_detect(names(data), "supp"))) {cat("Warning: NAs in subgroups due to supplemental case codes\n")}

  for(i in subgroup) {

    cases <- cases[eval(parse(text = i)),]

    flow_list[["inclusion"]][[i]] <- paste0(cur_n - nrow(cases), " / ", nrow(cases))

    cur_n <- nrow(cases)
  }

}


#Enrollment period
if(!is.null(period)) {

  cases <- cases[between(index, as.Date(period[1]), as.Date(period[2])),]

  flow_list[["inclusion"]][["period"]] <- paste0(cur_n - nrow(cases), " / ", nrow(cases))

  cur_n <- nrow(cases)

}

#Age limit
if(!is.null(age.limit)) {

  cases <- cases[(index - get(birth_c)) >= age.limit*365.25]

  flow_list[["inclusion"]][["age"]] <- paste0(cur_n - nrow(cases), " / ", nrow(cases))

  cur_n <- nrow(cases)

}


# #Exclusions, outcome
if(!missing(exclusion.out)) {

  for(i in exo_c) {

    cases <- cases[get(i) > index | is.na(get(i))]

    flow_list[["exclusion"]][[i]] <- paste0(cur_n - nrow(cases), " / ", nrow(cases))

    cur_n <- nrow(cases)
  }

}
#
# #Exclusions, exposure
if(!missing(exclusion.ex)) {

  for(i in exe_c) {


    cases <- cases[get(i) > index | is.na(get(i))]

    flow_list[["exclusion"]][[i]] <- paste0(cur_n - nrow(cases), " / ", nrow(cases))

    cur_n <- nrow(cases)


  }

}

flow_list[["final"]][["pre_match"]] <- paste0("- / ", nrow(cases))


print(flow_df <- rrapply::rrapply(flow_list, how = "melt") %>%
        mutate(excluded = str_extract(value, ".*\\s(?=(\\/))"),
               total = str_extract(value, "(?<=(\\/))\\s.*")) %>%
  rename(step = L1,
         variable = L2) %>%
  select(-value))

if(export) {

  saveRDS(flow_df, "PRE_FLOW.rds")

}

cat(paste0("\nTotal runtime: \n"))
cat(tockR("diff"))

bind_rows(cases, conts)

}



