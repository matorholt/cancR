#' Autoformatting of redcap exports
#'
#' @description
#' Automatic grabbing of labels from data dictionary and conversion to correct date format for exported redcap datasets.
#' The data dictionary can be downloaded under: Project Home and Design -> Dictionary -> Download the Data Dictionary
#'
#'
#' @param data raw redcap dataset
#' @param dictionary data dictionary in semicolon-separated .csv format
#' @param namelist optional list for manual labelling e.g. list("observer" = list("John" = "1", "Me" = "2"))
#' @param autoformat whether all labels should be in lowercase and underscores as spaces
#' @param date.vars vector of variable names containing date if not automatically converted
#' @param formatlist optional list for recoding common values such as positiv -> 1/pos/yes e.g. list("pos" = "positive", "no" = "Not Present")
#' @param cprlist optional dataframe containing cpr numbers for extraction of birth and sex
#' @param index optional index date for calculation of age at index
#'
#' @returns relabelled redcap dataset with correctly formatted dates
#' @export
#'
#'
#

# cpr <- readR("../../Atypical fibroxanthoma/Surgical risk factors of AFX recurrence/Statistics/CPRLIST.csv") %>%
#   select(id, cpr)
# raw <- readR("../../Atypical fibroxanthoma/Surgical risk factors of AFX recurrence/Statistics/data/afx_data_16.09.2025.csv")
# raw <- raw %>%
#   filter(study_id %nin% raw$study_id[raw$study_id %nin% cpr$id])
# dict <- readR("../../Atypical fibroxanthoma/Surgical risk factors of AFX recurrence/Statistics/data/data_dict.csv")
#
# redcapR(raw,
#               dict,
#               namelist = list("name" = list("n1" = "1",
#                                             "n2" = "2",
#                                             "n3" = "3",
#                                             "n4" = "4")),
#               formatlist = list("pos" = "Positive",
#                                 "neg" = "Negativ"),
#               cprlist = cpr,
#               index = datesurg)

redcapR <- function (data, dictionary, namelist = list(), date.vars = NULL,
                     autoformat = T, formatlist = NULL, cprlist = NULL, index)
{
  dict <- dictionary %>% rename(var = "Variable / Field Name",
                                type = "Field Type", labels = "Choices, Calculations, OR Slider Labels",
                                format = "Text Validation Type OR Show Slider Number") %>%
    select(var, type, labels, format)
  d <- dict %>% filter(type %in% c("radio", "checkbox"))
  varlist <- list()
  for (v in seq_along(d[["var"]])) {
    if (d[["var"]][v] %in% names(namelist)) {
      varlist[[d[["var"]][v]]] <- namelist[[d[["var"]][v]]]
    }
    else {
      values <- str_extract_all(d[v, "labels"], "\\d+(?=(,))")[[1]]
      labels <- str_remove_all(str_split(d[v, "labels"],
                                         "\\s\\|\\s")[[1]], "\\d+,\\s")
      if (!is.null(formatlist)) {
        labels <- str_replace_all(labels, names(formatlist) %>%
                                    set_names(formatlist))
      }
      if (autoformat) {
        labels <- str_replace_all(str_to_lower(labels),
                                  "\\s", "_")
      }
      varlist[[d[["var"]][v]]] <- as.list(values) %>% set_names(labels)
    }
  }
  raw <- data %>% recodR(varlist, match = "boundary", replace = T) %>%
    rename(id = 1) %>% mutate(across(everything(), ~if_else(. %in%
                                                              c("", "NA", " "), NA, .)))
  date_c <- unique(c(dict$var[which(str_detect(dict$format,
                                               "date"))], date.vars, dict$var[which(str_detect(dict$var,
                                                                                               "date"))]))
  if (length(date_c) > 0) {
    raw <- raw %>% datR(date_c)
  }

  if(any(!is.null(cprlist) | str_detect(colnames(raw), "\\bcpr\\b"))) {

    if (!is.null(cprlist)) {
      raw <- left_join(raw, cprlist, by = "id")

    }

    raw <- raw %>% cpR(extract = T)

  }

  if(any(str_detect(colnames(raw), "\\bbirth\\b")) & !missing(index)) {

    index_c <- data %>% select({{index}}) %>% names()

    raw <- raw %>% mutate(age = round((as.numeric(!!sym(index_c) -
                                                    birth))/365.25, 1))

  }

  return(raw)
}

