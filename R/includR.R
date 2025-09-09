#' includR
#'
#' @description
#' Apply inclusion criteria more conveniently
#'
#'
#' @param data joined data frame
#' @param exclusion_ex vector of exposure variables that should not be present before index
#' @param exclusion_out vector of outcome variables that should not be present before index
#' @param filter filter for subgroups in the format: "level %in% c('a', 'b')"
#' @param age_limit age limit for inclusion as numeric value
#' @param period enrollement period in the format: c("1980-01-01", "2022-01-01")
#' @param fu name of the follow-up date (default: fu)
#' @param birth name of the birth date variable (default: birth)
#'
#' @return A data frame with filtered cases and all controls + an rds file for flow chart
#' @export
#'
#'

#set.seed(1)
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

# includR(pre_join,
#         exclusion_ex = c(lpr_ex, lmdb_ex, opr_ex),
#         exclusion_out = c(sc_date, meta_date),
#         filter = "sg_level %in% c('supergroup_a', 'supergroup_b')",
#         age_limit = 18,
#         period = c("1980-01-01", "2022-01-01"),
#         fu = doddate,
#         birth = birthdate)


includR <- function(data,
                    exclusion_ex,
                    exclusion_out,
                    filter = NULL,
                    age_limit = NULL,
                    period = NULL,
                    fu=fu,
                    birth = birth) {


    exe_c <- data %>% select({{exclusion_ex}}) %>% names()
    exo_c <- data %>% select({{exclusion_out}}) %>% names()
    fu_c <- data %>% select({{fu}}) %>% names()
    birth_c <- data %>% select({{birth}}) %>% names()


    flow_list <- list()

#split
setDT(data)

if(any(str_detect(names(data), "supp"))) {

  supp_c <- data %>% select(contains("supp")) %>% names()

data <- data[,index := pmin(index, get(supp_c), na.rm=T)][
  , case := ifelse(!is.na(index), 1, 0)
]



}

data <- data[get(fu_c) > index | is.na(index),]


#Split
cases <- data[case == 1,]
conts <- data[case == 0]

#Subgroup
if(!is.null(filter)) {

  cases <- cases[eval(parse(text = filter)),]

}

flow_list[["total"]] <- nrow(cases)

#Enrollment period
if(!is.null(period)) {

  cases <- cases[between(index, as.Date(period[1]), as.Date(period[2])),]

  flow_list[["period"]] <- nrow(cases)

}

#Age limit
if(!is.null(age_limit)) {

  cases <- cases[(index - get(birth_c)) >= age_limit*365.25]

  flow_list[["age"]] <- nrow(cases)

}


#Exclusions, outcome
if(!missing(exclusion_out)) {

  for(i in exo_c) {

    cases <- cases[get(i) > index | is.na(get(i))]

    flow_list[[i]] <- nrow(cases)

  }

}

#Exclusions, exposure
if(!missing(exclusion_ex)) {

  for(i in exe_c) {


    cases <- cases[get(i) > index | is.na(get(i))]

    flow_list[[i]] <- nrow(cases)


  }

}

flow_list[["pre_match"]] <- nrow(cases)

fdf <-
  data.frame(step = names(flow_list),
           n = unlist(flow_list)) %>%
  mutate(n2 = lag(n)-n,
  x.axis = ifelse(row_number() %in% c(1, length(flow_list)), 1, 2),
         y.axis = rev(row_number()))

ggplot(fdf, aes(x=x.axis, y=y.axis)) +
  geom_label(aes(label = paste0(step, " = ", n2)), label.r = unit(0, units = "mm"))

bind_rows(cases, conts)


}


