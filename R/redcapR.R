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
#' @param formatlist optional list for recoding common values such as positiv -> 1/pos/yes e.g. list("pos" = "positive", "no" = "Not Present")
#'
#' @returns relabelled redcap dataset with correctly formatted dates
#' @export
#'
#'

# redcapR(raw,
#         dict,
#         namelist = list("name" = list("n1" = "1",
#                                       "n2" = "2",
#                                       "n3" = "3",
#                                       "n4" = "4")),
#         formatlist = list("pos" = "Positive",
#                           "neg" = "Negativ"))

redcapR <- function(data,
                    dictionary,
                    namelist = list(),
                    autoformat = T,
                    formatlist = NULL) {

  dict <-
    dictionary %>%
    rename(var = "Variable / Field Name",
           type = "Field Type",
           labels = "Choices, Calculations, OR Slider Labels",
           format = "Text Validation Type OR Show Slider Number") %>%
    select(var, type, labels, format)

  d <- dict %>%
    filter(type %in% "radio")


  varlist <- list()

  for(v in seq_along(d[["var"]])) {

    if(d[["var"]][v] %in% names(namelist)) {

      varlist[[d[["var"]][v]]] <- namelist[[d[["var"]][v]]]

    } else {

      values <- str_extract_all(d[v, "labels"], "\\d+(?=(,))")[[1]]
      labels <- str_remove_all(str_split(d[v, "labels"], "\\s\\|\\s")[[1]], "\\d+,\\s")

      if(!is.null(formatlist)) {

       labels <- str_replace_all(labels,
                                 names(formatlist) %>% set_names(formatlist))

      }

      if(autoformat) {

        labels <- str_replace_all(str_to_lower(labels), "\\s", "_")

      }

    varlist[[d[["var"]][v]]] <-
      as.list(values) %>% set_names(labels)

    }


  }

  raw %>%
    datR(vars = dict$var[str_detect(dict$format, "date")]) %>%
    recodR(varlist)

}
