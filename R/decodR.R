#' Decoding of the main codelist for loading and searching in registries
#'
#'
#' @param codelist List of lists with registries, diagnosis codes and labels. See example.
#' @param regs additional registries for loading
#' @param type matching or rtmle, depending on the restructurering of the codelist (deafault = "matching")
#'
#' @return
#' codes: Original codelist without modification \cr
#' regs: The specified registries for the project. First input in loadR \cr
#' loadR: The pattern.list in loadR \cr
#' searchR: The pattern.list in searchR \cr
#' regex: The codelist converted to regex-expressions with | as separator
#' @export
#'
#'

# Matching codelist
# codelist <- list("lpr_case" = list("abdomen" = list("kidney" = c("tx1a","tx1b","tx1c"),
#                                                    "liver" = c("tx2a", "tx2b", "tx2c")),
#                                    "thorax" = list("heart" = c("tx3a", "tx3b", "tx3c"),
#                                                    "lung" = c("tx4a", "tx4b", "tx3c"))),
#                  "lpr_ex" = list("immune_diag" = "a3",
#                                  "cll" = c("a4", "b4")),
#                  "lmdb_ex" = list("immune_drugs" = "a5"),
#                  "opr_ex" = list("trans" = "t5"),
#                  "pato_supp" = list("PCC" = "M80"),
#                  "labels" = list("lpr_case" = c("SOTR", "region"),
#                                  "lpr_ex" = "immsup"),
#                  "exclusion" = c("z1","z2"))
#
# clist <- decodR(codelist, type = "matching")
#
#
# #RTMLE
# codelist <- list(
#
#   setup = list(period = as.Date(c("2005-01-01", "2022-12-31")),
#                look.back = 365.25*10,
#                age = 40),
#
#   exclusion = c("sc_date", "sotr, immunosuppressants", "immunosuppressants_proc", "steroid"),
#
#   exposure = list(simvastatin = list(atc = "C10AA01",
#                        maxdepot = 100*80*3,
#                        prescriptionwindow = 2,
#                        doses = list(
#                          value = c(5,10,20,30,40,80),
#                          min = c(5,5,10,15,20,40),
#                          max = c(10,20,40,60,80,80),
#                          def = c(5,10,20,30,40,80))),
#                   atorvastatin = list(atc = "C10AA01",
#                                       maxdepot = 100*80*3,
#                                       prescriptionwindow = 2,
#                                       doses = list(
#                                         value = c(5,10,20,30,40,80),
#                                         min = c(5,5,10,15,20,40),
#                                         max = c(10,20,40,60,80,80),
#                                         def = c(5,10,20,30,40,80))),
#                   rosuvastatin = list(atc = "C10AA01",
#                                       maxdepot = 100*80*3,
#                                       prescriptionwindow = 2,
#                                       doses = list(
#                                         value = c(5,10,20,30,40,80),
#                                         min = c(5,5,10,15,20,40),
#                                         max = c(10,20,40,60,80,80),
#                                         def = c(5,10,20,30,40,80)))),
#   lmdb = list(
#     tetracyclines = c("J01AA"),
#     fluoroquinolones = c("J01MA"),
#     sulfonamides = c("C03BA", "D06BA", "J01EB", "J01EE"),
#     macrolides = c("J01FA"),
#     voriconazole = c("J02AC03"),
#     griseofulvin = c("D01BA", "J02AC"),
#     hydrochlorothiazide = c("C03AA03"),
#     chlorthalidone = c("C03BA04"),
#     indapamide = c("C03BA11"),
#     furosemide = c("C03CA01", "C03EB01"),
#     amiodarone = c("C01BD01"),
#     calcium_channel_blockers = c("C08"),
#     quinidine = c("C01B"),
#     piroxicam = c("M01AC01"),
#     naproxen = c("M01AE02"),
#     diclofenac = c("M01AB05", "M01AB55"),
#     ketoprofen_gel = c("M02AA10"),
#     diclofenac_gel = c("M02AA15", "D11AX18"),
#     chlorpromazine = c("N05AA01"),
#     thioridazine = c("N05AC02"),
#     tricyclics = c("N06AA"),
#     isotretinoin = c("D10BA01"),
#     acitretin = c("D05BB02"),
#     topical_retinoids = c("D10AD"),
#     vemurafenib = c("L01EC01"),
#     dabrafenib = c("L01EC02"),
#     fluorouracil_5 = c("L01BC02"),
#     aminolevulinic_acid = c("L01XD04"),
#     methyl_ala = c("L01XD03"),
#     immunosuppressants = c("L04"),
#     antimalarials = c("P01B"),
#     steroid = "H02A"
#   ),
#
#   lpr = list(
#     heart_failure = c(
#       "I110", "I130", "I132", "I420", "I42[6-9]", "I50"
#     ),
#     hypertension = c(
#       "I10-I15", "I109", "I110", "I119", "I19A", "I120",
#       "I129", "I13[0-2]", "I139", "I15[0-2]", "I15[8-9]"
#     ),
#     ischemic_heart_disease = c(
#       "I2[03-5]"
#     ),
#     stroke = c(
#       "I6[0-1]", "I6[3-4]", "G45"
#     ),
#     cerebral_transient_ischemic_attack = c(
#       "G45"
#     ),
#     peripheral_vascular_disease = c(
#       "I70", "I739"
#     ),
#     renal_disease = c(
#       "N0[2-8]", "N1[1-2]", "N14", "N1[7-9]", "N26",
#       "N15[8-9]", "N160", "N16[2-4]", "N168",
#       "Q61[2-3]", "Q615", "Q619",
#       "E112", "E131", "E142", "I120", "R34"
#     ),
#     atrial_fibrillation = c(
#       "I48"
#     ),
#     non_skin_cancer = c(
#       "C[0-35-9]", "C4[0-25-9]"
#   ),
#   copd = c(
#     "J4[24]"
#     ),
#
#     myocardial_infarction = c(
#       "I2[1-2]"
#     ),
#
#     revascularization = c(
#       "O2[017]", "O3[47]", "O4", "O21", "O27", "O271",
#       "O37", "O37[1-2]", "O4[1-2]", "O34", "O34[1-2]"
#     ),
#
#     unstable_angina = c(
#       "I20"
#     ),
#
#     diabetes = c(
#       "E1[01]"
#     ),
#
#     organ_transplantation = c("DZ94"),
#
#     immunodeficiency = c("DD8")),
#
#   opr = list(
#     sotr = c("KKAS", "KFQA", "KJJC", "KGDG", "KJLE"),
#     immunosuppressants_proc = c("BOHJ")
#   )
# )
#
#
# codelist



decodR <- function(codelist,
                   regs = c("pop", "sc", "meta", "dsd"),
                   type = "matching") {

  if(type == "matching" & sum(str_detect(names(codelist), "case")) != 1) {
    return(cat("Error: Only one elment in the list can be named case"))
  }

  #Copy
  main <- codelist

  out.list <- list()

  #loadR registries
  regs.names <- as.character(na.omit(str_extract(names(main), "lpr|lmdb|opr|cancer|pato")))
  out.list[["loadR"]][["regs"]] <- c(regs.names, regs)

   for(i in regs.names) {

     out.list[["loadR"]][["pattern.list"]][[i]] <- unlist(main[i], use.names = F)
     out.list[["searchR"]][["search.list"]][[i]] <- main[[i]]

   }

  out.list[["design"]] <- main$design

  #Design specific decoding
  if(type == "matching") {

    out.list[["loadR"]][["regs"]] <- unique(c(out.list[["loadR"]][["regs"]], names(main$case)))

    for(i in names(main$case)) {

      out.list[["loadR"]][["pattern.list"]][[i]] <- c(out.list[["loadR"]][["pattern.list"]][[i]], unlist(main$case[[i]], use.names=F))

      if(length(names(main$case)) > 1) {
      out.list[["searchR"]][["search.list"]][[i]][[paste0("case_",i)]] <- main$case[[i]]
      } else {
        out.list[["searchR"]][["search.list"]][[i]][["case"]] <- main$case[[i]]
      }

    }

    out.list[["design"]]$exclusion.ex <- unlist(lapply(main[regs.names], function(i) {
                                          names(i)
                                        }), use.names = F)

    names(out.list[["design"]][["exclusion"]]) <- "exclusion.out"

    if(length(names(main$case)) > 1) {
      out.list[["design"]]$case <-  paste0("case_", names(main$case))
    } else {
      out.list[["design"]]$case <-  "case"
    }


  }

  if(type == "rtmle") {

    for(i in seq_along(main$exposure)) {

      main$exposure[[i]]$period <- main$design$period

      out.list[["exposure_atc"]] <- c(out.list[["exposure_atc"]], main$exposure[[i]]$atc)

    }

    out.list[["exposure"]] <- main$exposure

  }

 return(out.list)




















  if(type == "matching") {
  c_lab <- str_detect(names(codelist), "case")

  #Flatten list
  cl_melt <- rrapply::rrapply(codelist, how = "melt")

  #Fill
  codelist <-
    bind_cols(cl_melt[, 1:ncol(cl_melt)-1] %>%
                rowR(type = "fill", direction = "right"),
              cl_melt[ncol(cl_melt)]) %>%
    select(1, (ncol(.)-1), ncol(.)) %>%
    rrapply::rrapply(how = "unmelt")

  #Registies
  registries <- unique(str_extract(names(codelist)[str_detect(names(codelist), "lpr|opr|lmdb|cancer|pato")], "lpr|opr|lmdb|cancer|pato"))

  #LoadR + searchR
  loadlist <- list()
  searchlist <- list()

  for(i in registries) {

    loadlist[[i]] <- c(unlist(codelist[str_detect(names(codelist), i)]), use.names=F)

    for(j in names(codelist)[str_detect(names(codelist), i)]) {

      searchlist[[j]] <-c(unlist(codelist[[j]]), use.names=F)

    }

  }

  #Exclusions
  ex_list <- unique(str_extract(names(codelist)[str_detect(names(codelist), "_ex")], ".*_ex"))

  list <- list(
    main = main,
    loadR.regs = c(registries, regs),
    loadR.list = loadlist,
    searchR.list = searchlist,
    includR.exclude = ex_list
  )

  if("labels" %in% names(codelist)) {

    list <- append(list, list(searchR.keep = codelist[["labels"]]))

    lab_list <- list()

    for(n in names(codelist[["labels"]])) {

      lab_df <- cl_melt[cl_melt[[1]] == n,]

      levels <- codelist[["labels"]][[n]]

      for(l in seq_along(levels)) {

        sub_df <- lab_df[, c(l+1,ncol(lab_df))]

        lab_list[[levels[l]]] <-
          rrapply::rrapply(sub_df %>%
                             unnest(value) %>%
                             group_by(!!sym(colnames(sub_df)[1])) %>%
                             summarise(value = list(value)),
                           how = "unmelt")

      }

    }

    list <- append(list, list(recodR.labels = lab_list))

  }

  if("exclusion" %in% names(codelist)) {

    list <- append(list, list(searchR.exclusion = codelist[["exclusion"]]))
  }

  if("lpr_case" %in% names(codelist)) {

    if(any(unlist(codelist[["lpr_case"]]) %in% unlist(charlson.codes))) {

      cat("Cases have diagnoses codes as a part of the Charlson Comorbidity Index - remember to use the updatR() function")


      list <- append(list, list(updatR.exclusion = unlist(codelist[str_detect(names(codelist), "lpr_case")], use.names = F)))

    }

  }

 # print(viewR(main))

  return(list)

  }

  if(type == "rtmle") {

    out.list <- list()

    reg.list <- na.omit(unique(str_extract(names(main), "lpr|opr|lmdb|cancer|pato")))

    out.list[["loadR.regs"]] <- c(reg.list, regs)

    print(reg.list)

    #loadR pattern.list
    out.list[["loadR.list"]] <- lapply(reg.list, function(i) {

      unlist(main[[i]], use.names = F)

    }) %>% set_names(reg.list)

    out.list[["searchR.list"]] <-
      lapply(reg.list, function(i) {

        main[[i]]

    }) %>% set_names(reg.list)

    #Allocate study period to
    for(i in seq_along(main$exposure)) {

      main$exposure[[i]]$period <- main$setup$period

      out.list[["exposure_atc"]] <- c(out.list[["exposure_atc"]], main$exposure[[i]]$atc)

    }

    out.list[["exposure"]] <- main$exposure
    out.list[["exclusion"]] <- main$exclusion
    out.list[["setup"]] <- main$setup


    return(out.list)
  }



}

# c.list <- decodR(codelist_matching)
#
# str(c.list)
#decodR(codelist_rtmle, type = "rtmle")


