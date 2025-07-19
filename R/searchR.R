#' searchR
#'
#' @description
#' Function to obtain covariates or outcomes from the most common registers
#'
#'
#' @param data The full register as a data.frame
#' @param register Specify the register
#' @param pattern_list list with list(label = diagnosis code) structure. The pattern can be collected using c() which will be coupled with | as a regex.
#' @param keep list with list("label" = "column name") structure if the diagnosis code should be kept with specified column name.
#' @param index_df Add index-dates to the patients (optional)
#' @param match How specific the diagnosis codes should be used
#' @param remove Removal of specific diagnosis codes
#' @param format Whether NA/! or dates should be returned
#' @param interval Whether the var_search should be in intervals around the index date (e.g. index +/- interval)
#' @param slice Which occurence should be extracted
#' @param index Name of the index column
#' @param pnr Name of the pnr column
#'
#' @return returns a data frame with one or multiple columns based on the labels with either NA/1 or dates if a condition is present
#' @export
#'
#'
#'

# df <- simAdmissionData(n=100)
# df2 <- simPrescriptionData(n=100)
#
# df_index <- df %>% select(pnr, indexdate) %>% rename(index = indexdate) %>% distinct(pnr, .keep_all=T)
#
# (t <- searchR(df,
#               pattern_list= list("KOL" = "DQ",
#                                  "Astma" = c("DB", "DQ"),
#                                  "DM" = "DD"),
#               keep = list("KOL" = "subtypes",
#                           "Astma" = "diags"),
#               register = "lpr",
#               remove = c("DG", "DUA", "EG"),
#               format="date",
#               slice = "first"))

searchR <- function(data,
                    register,
                       pattern_list,
                    keep = list(),
                       index_df,
                       match = "contains",
                       remove = "*_*",
                       format = "date",
                       interval,
                       slice = "first",
                       index = index,
                       pnr = pnr) {

  register <- match.arg(register, c("lpr", "lmdb", "pato", "opr", "cancer", "dcr"))
  match <- match.arg(match, c("start", "end", "exact", "contains"))
  format <- match.arg(format, c("categorical", "date"))
  slice <- match.arg(slice, c("first", "last", "all"))

  pattern_list <- lapply(pattern_list, function(x) {
                               paste0("(",x,")", collapse="|")
                             })

  pattern <- unlist(pattern_list)
  labels <- names(pattern_list)


  if(missing(remove)) {
    remove <- rep("xxx", length(pattern))
  }

  if(length(pattern) != length(remove)) {
    remove <- c(remove, rep("xxx", length(pattern)-1))
    warning("Different lengths in pattern and remove - check positions")
  }

  if(sum(names(keep) %nin% names(pattern_list)) > 0) {
    warning(paste0(paste0(names(keep)[names(keep) %nin% names(pattern_list)], collapse=", "), " are not present in pattern_list - check for spelling errors"))
  }

  switch(match,
         "start" = {pattern <- paste0("^", "(", pattern, ")")},
         "end" = {pattern <- paste0("(", pattern, ")$")},
         "exact" = {pattern <- paste0("^(", pattern, ")$")}
        )

  switch(register,
         "lpr" = {data <- data %>% rename(code = diag,
                                          date = inddto)},
         "lmdb" = {data <- data %>% rename(code = atc,
                                           date = eksd)},
         "pato" = {data <- data %>% rename(code = snomed)},
         "opr" = {data <- data %>% rename(code = opr,
                                          date = inddto)},
         "cancer" = {data <- data %>% rename(code = c_morfo3,
                                          date = d_diagnosedato)}
         )

  if(!missing(index_df)) {
    data <- data %>%
      left_join(., index_df, by = data %>% select(pnr) %>% names()) %>%
      arrange({{pnr}}, date)
    if(!missing(interval)) {
      data <- data %>%
        filter(between(date, {{index}} + interval[1], {{index}} + interval[2])) %>%
        arrange({{pnr}}, date)
    } else {
      data <- data %>%
      filter(date <= {{index}}) %>%
        arrange({{pnr}}, date)
    }

  }

   l <-
    lapply(seq(1,length(pattern)), function(x) {

      data <- data %>%
        filter(str_detect(code, pattern[x]) & str_detect(code, remove[x], negate=T))

      if(format == "categorical") {
       data <- data %>%
          mutate(!!sym(labels[[x]]) := 1)
      } else {
      data <- data %>% mutate(!!sym(labels[[x]]) := date)
      }

      if(labels[[x]] %in% names(keep)) {
        data <- data %>% mutate(!!sym(unlist(keep[labels[[x]]])) := code)
      }

      if(slice == "first") {
        data <- data %>%
          group_by(pnr) %>%
          slice(1) %>%
          select(pnr, contains(c(labels[[x]], unlist(keep))))
      } else if(slice == "last") {
        data <- data %>%
          group_by(pnr) %>%
          slice(n()) %>%
          select(pnr, contains(c(labels[[x]], unlist(keep))))

      } else if(slice == "all") {
        data <- data %>%
          select(pnr, contains(c(labels[[x]], unlist(keep))))

      }

   })

  plyr::join_all(l, by = "pnr", type = "full") %>%
    arrange(pnr)

}
