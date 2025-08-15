#' viewR
#'
#' @description
#' Graphical overview of the codelist hierarchy
#'
#'
#' @param codelist A codelist of max depth 3
#'
#' @return graphical overview of the levels and groups in the codelist
#' @export
#'
#'

# codelist <- decodR(list(
#
#   #Autoimmune/autoinflammatory diseases (cases)
#   lpr_case = list(
#     #Systemiske autoimmune/autoinflammatoriske sygdomme
#     "ra" = c("DM05", "DM06"), #reumatoid artrit (seropositiv og seronegativ)
#
#     "psa" = c("DL40", "DL400", "DL400B", "DL400C",
#               "DL400D", "DL401", "DL401B"), #psoriasis + alle undertyper
#
#     "spa" = c("DM45", "DM459", "DM46", "DM460",
#               "DM461", "DM468", "DM468A", "DM469"), #spondylitis ankylopoietica (Bechterews sygdom)
#
#     "sle" = c("DM32", "DM321", "DM328", "DM329", "DL93"), #systemisk lupus erythematosus + kutane manifestationer
#
#     "ss" = c("DM34"), #systemisk sklerodermi
#
#     "sjogren" = c("DM350"), #sjögrens/sicca syndrom og følgesykdommer (keratkonjuctivitis, xerostomi),
#
#     "vasc" = c("DM30", "DM300", "DM303", "DM308", "DM308A",
#                "DM315", "DM315A", "DM316", "DM316A", "DL95",
#                "DM31", "DM30", "DM300", "DM308", "DM313"), #vaskulitissygdomme inkl kæmpecellearteritis, nekrotiserende vaskulitis, polyartritis nodosa og wegeners granulomatose
#
#     "ctd" = c("DM351", "DM353"), #generelle bindevævssygdomme inkl polymyalgia reumatica
#
#     "mctd" = c("DM351A"), #blandet bindevævssygdom (MCTD)
#
#     "dm_pm" = c("DM33", "DM331", "DM332", "DM339"), #dermatomyositis og polymyositis
#
#     "behcet" = c("DM352"), #behcets sygdom
#
#     "ms" = c("DG35", "DG359", "DG359A", "DG359B", "DG359C"), #multipel sklerose
#
#     "nmosd" = c("DG360"), #neuromyelitis optica spectrum disorder / NMOSD
#
#     "dem_other" = c("DG36", "DG368", "DG369",
#                     "DG37", "DG373", "DG378", "DG379"), #andre autoimmune demyeliniserende CNS-sygdomme
#
#     "sarcoidose" = c("DB86", "DB860", "DB861", "DB862",
#                      "DB863", "DB863A", "DB868", "DB868A",
#                      "DB868G", "DB869", "DG532", "DM633"), #Sarkoidose, Sarkoidose i lunger, Sarkoidose i lymfeknuder, Sarkoidose i både lunger og lymfeknuder, Sarkoidose i hud, Lupus pernio, Sarkoidose med anden lokalisation eller flere lokalisationer, Febris uveoparotidea (Heerfordt), Akut sarkoidose UNS, Sarkoidose UNS, Parese af flere kranienerver ved sarkoidose, Myositis ved sarkoidose
#
#     "itp" = "DD693", #idiopatisk trombocytopenisk purpura
#
#
#     #Tarmrelaterede autoimmune sygdomme
#     "mb" = c("DK500", "DK500A", "DK500B",
#              "DK500C", "DK500D", "DK501",
#              "DK501D", "DK508", "DK508A",
#              "DK508C", "DK508D", "DK509"), #morbus crohn + all locations
#
#     "uc" = c("DK51", "DK510", "DK512", "DK513",
#              "DK514", "DK515", "DK515A",
#              "DK515B", "DK518", "DK518B",
#              "DK519"), #ulcerøs colitis + all locations
#
#     #Hudrelaterede autoinflammatoriske sygdomme
#     "hs" = "DL732", #hidradenitis suppurativa
#
#     "lp" = "DL43", #lichen planus
#
#     "pemph" = c("DL120", "DL121", "DL121A", "DL129", "DL128"), #pemfigoid-sygdomme
#
#     "ad" = c("DL20", "DL208", "DL208B", "DL208D", "DL209"), #atopisk dermatitis
#
#     #Lokale/organ-specifikke autoimmune sygdomme
#     "add" = c("DE271", "DE271A", "DE271B"), #addisons sygdom "DE271B" er autoimmun, men de fleste autoimmune er sikkert kodet med de uspecifikke DE271 og DE271A
#
#     "cd" = "DK900", #cøliaki
#
#     "t1d" = "DE10", #type 1 diabetes
#
#     "graves" = c("DE05", "DE050", "DE051",
#                  "DE052", "DE052A", "DE055",
#                  "DE058", "DE058A", "DE058B",
#                  "DE058C", "DE059"), #graves sygdom
#
#     "hashimoto" = c("DE063", "DE063A", "DE063B"), #hashimotos thyroiditis
#
#     "mg" = c("DG70", "DG700"), #myasthenia gravis
#
#     "pa" = c("DD51", "DD510", "DD511", "DD513", "DD519"), #perniciøs anæmi
#
#     "aih" = c("DK743", "DK754", "DK732",
#               "DK732B", "DK732C", "DK732D",
#               "DK732E", "DK732F", "DK732G") #autoimmun hepatitis og primær biliær cholangitis
#   ),
#
#   #Exclusion diagnoses
#   lpr_ex = list(
#     "hiv" = "DB2[0-4][0-9A-Z]", #HIV og AIDS – dækker hele blokken DB20 til DB24 + subkoder
#     "immundefekt" = "DD8[0-4][0-9A-Z]", #Primære immundefekter – hele blokken DD80–DD84 + subkoder
#
#     #Genetiske syndromer
#     "NF" = "DQ85", #Neurofibromatose type 1 (NF1), Von Hippel-Lindaus syndrom, Peutz-Jeghers' syndrom, Tuberøs sklerose, Sturge-Webers-Krabbes syndrom , Fakomatoser
#     "Cowdens" = "DQ878D", #Cowdens
#     "fap" = "DD126F", #Familiær Adenomatøs Polypose
#     "men" = "DD448", #Multipel Endokrin Neoplasi/Adenomatose
#     "aht" = "DI780", #arvelig hemoragisk telengaktesi
#     "xp" = "DQ821", #xeroderma pigmentosum
#     "fa" = "DD610B", #fanconis anemi
#     "wa" = "DD820" #Wiskott-Aldrichs syndrom
#   ),
#
#   #ATC exclusions
#   lmdb_ex = list(
#     "is" = "L04A", #Immunosuppressiva – dækker hele L04A-blokken og alle underkoder
#     "an" = "L01", #Antineoplastiske midler – hele L01 + subkoder
#     "gk" = "H02" #Systemiske glukokortikoider – hele H02 + subkoder
#   ),
#
#   #Op/procedure exclusions
#   opr_ex = list(
#     "tx_kidney" = "KKAS(00|10|20|40|41|50|60|61|70|96|97)$",   #nyretransplantationskoder
#     "tx_liver" = "KJJC(00|10|20|30|40|60|96|96A)$",            #levertransplantation
#     "tx_pancreas" = "KJLE(00|03|10|16|20|30|40|50|96)$",       #pancreastransplantation minus KJLE56
#     "tx_heart" = "KFQ(A|B|W)[0-9A-Z]$",                       #hjerte-, hjerte-lunge- og relaterede transplantationskoder
#     "tx_lung" = "KGDG[0-9A-Z]$",                              #lungetransplantationskoder
#     "tx_kmt" = "BOQF[0-9A-Z]$",                               #knoglemarvstransplantation
#     "tx_bohj" = "BOHJ"                                   #Behandling med antistoffer og immunmodulerende behandling
#   ),
#   labels = list("lpr_case" = "disease")
# ))
#
#
#
# viewR(codelist$codes)


viewR <- function(codelist) {

  codelist <- codelist[str_detect(names(codelist), "lpr|lmdb|opr|cancer|pato")]

  l2 <- map_depth(codelist, 2, function(x) paste(x, collapse=", "))

  l <- list()

  for(i in names(l2)) {

    l[[i]] <-
      data.frame(code=as.vector(unlist(l2[[i]])),
                 label = names(l2[[i]])) %>%
      mutate(list = i)

  }

  df <- bind_rows(l)


  df2 <-
    df %>%
    rollR(label, y.axis) %>%
    rollR(list, group) %>%
    pivot_longer(c("label", "code", "list"), names_to = "type", values_to = "text") %>%
    arrange(desc(type)) %>%
    rollR(type, x.axis) %>%
    mutate(y.label = ifelse(type %in% "list", group, y.axis)) %>%
    distinct(text, .keep_all = T) %>%
    group_by(group) %>%
    mutate(y.axis = ifelse(type %in% "list", median(y.axis), y.axis)) %>%
    ungroup() %>%
    factR(group)

  rect <- df2 %>% group_by(group) %>%
    summarise(min = min(y.axis),
              max = max(y.axis))

  grps <- df2 %>% filter(type == "list")
  rows <- df2 %>% filter(type != "list")

  row <- list()

  for(g in 1:nrow(grps)) {

    row_sub <- rows %>% filter(group == g)

    for(i in 1:nrow(row_sub)) {

      row <- append(row, list(data.frame(y.axis=c(as.numeric(unlist(grps[g, "y.axis"])),
                                                  rep(as.numeric(unlist(row_sub[i, "y.axis"])),3)),
                                         x.axis=c(1,1.5,2,3),
                                         group = g,
                                         i = i)))



    }

  }

  lines <- bind_rows(row) %>% mutate(gp = str_c(i, group, sep = "_")) %>%
    factR(gp) %>%
    factR(group)


  ggplot(df2, aes(x=x.axis, y = y.axis, group = group)) +
    geom_step(data=lines, aes(group = gp, color = group)) +
    geom_label(aes(label = text, fill = group), hjust = "left") +
    scale_fill_manual(values = cancR_palette) +
    scale_color_manual(values = cancR_palette) +
    coord_cartesian(xlim=c(1, pmax(5, (max(nchar(unlist(l2)))/15)))) +
    theme_classic() +
    theme(legend.position = "none",
          axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank())



}

