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
#' @param id name of id in cprlist (should have the same name as in data)
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
#               cprlist = cpr)

redcapR <- function(data,
                    dictionary,
                    namelist = list(),
                    keep.raw = NULL,
                    date.vars = NULL,
                    autoformat = T,
                    formatlist = NULL,
                    cprlist = NULL,
                    id,
                    dt = F) {

  dat <- as.data.table(data)
  setDT(dictionary)

  keep_c <- defusR(keep.raw, data=dictionary)

  dict_labs <- c("var", "type", "labels", "format")

  setnames(dictionary,
           c("Variable / Field Name", "Field Type", "Choices, Calculations, OR Slider Labels", "Text Validation Type OR Show Slider Number"),
           dict_labs)

  if("record_id" %in% names(dat) && "id" %nin% names(dat)) setnames(dat, "record_id", "id")

  d <- dictionary[, c(dict_labs), with = FALSE] %>%
    .[type %in% c("radio", "checkbox")]

  if(nrow(d) == 0) return(cli::cli_alert_warning("No radio or checkbox labels present in dictionary - nothing changed"))

  vars <- d$var[d$var %nin% keep_c & d$var %in% names(dat)]

  varlist <- map(seq_along(vars), ~ {

    v <- vars[.x]

    if(v %in% names(namelist)) {
      namelist[[v]]
    } else {
      values <- unlist(str_extract_all(d[var == v]$labels, "\\d+(?=(,))"))
      labels <- str_remove_all(str_split(d[var == v]$labels,
                                         "\\s\\|\\s")[[1]], "\\d+,\\s")

      if (!is.null(formatlist)) {
        labels <- str_replace_all(labels, names(formatlist) %>%
                                    set_names(formatlist))
      }
      if (autoformat) {
        labels <- str_replace_all(str_to_lower(labels),
                                  "\\s", "_")
      }

      as.list(values) %>% set_names(labels)
    }



  }) %>% set_names(vars)

  dat <- recodR(dat,
                varlist,
                match = "boundary",
                replace = T)[, map(.SD, ~ if_else(.x %in% c("", "NA", " "), NA, .x)), .SDcols = names(dat)]

  date_c <- unique(c(dictionary$var[which(str_detect(dictionary$format, "IDate|date"))],
                     date.vars,
                     dictionary$var[which(str_detect(dictionary$var, "date"))]))

  if (length(date_c) > 0) {
    datR(dat, date_c[date_c %in% names(dat)])
  }

  if (!is.null(cprlist)) {
    id_c <- defusR(id)

    cpr <- setDT(cpr(cprlist))

    dat <- joinR(dat, unique(cpr, by = "cpr")[, c("id", "cpr")], by = id_c)

  }

  if(dt) return(dat) else(as.data.frame(dat))
}
