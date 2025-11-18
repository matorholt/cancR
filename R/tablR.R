#' Create baseline table
#'
#' @description
#' Wrapper for the tableby function in the Arsenal package
#'
#' @param data dataframe
#' @param group Grouping variable. If omitted a single column tabel is provided
#' @param vars Variables that the grouping variable should be aggregated by
#' @param test Whether statistical tests should be performed (default = FALSE)
#' @param total Whether a total column should be included (default = FALSE)
#' @param numeric Selection of the type of stats for numerical variables (e.g. median, q1q3, range, mean, sd)
#' @param direction Direction for percentages (colwise or rowwise)
#' @param labels List specifying labels of the specific labels for each variable
#' @param reference List specifying reference group for each variable
#' @param labs.headings List specifying labels for variable names
#' @param reverse whether the order of groups should start with the highest level (default = T)
#' @param test.stats Vector of length 2 containing statistical tests that should be performed
#' @param show.na Whether NAs should be presented
#' @param censur whether counts <= 3 should be censored
#' @param digits number of digits
#'
#' @return Returns a table and exports a word-file (optional if filename is provided)
#' @export
#'
#'

# tab <- redcap_df %>%
#   mutate(margins = sample(c("0","1"), nrow(redcap_df), replace=TRUE)) %>%
#   factR(c(type, sex, localisation, cd10, sox10, ck, necrosis, margins)) %>%
#   tablR(
#     group = type,
#     vars=c(age, sex, localisation, cd10, sox10, ck, necrosis, margins),
#     labs.groups = list("Benign" = "0",
#                        "In situ" = "1",
#                        "Malignant" = "2"),
#     labs.headings = list("Age at Debut" = "age"),
#     labs.subheadings = list("sex" = list("Female" = "2",
#                                          "Male" = "1"),
#                             "localisation" = list("Neck" = "0",
#                                                   "Head" = "1",
#                                                   "Trunk" = "2",
#                                                   "Upper Extremity" = "3",
#                                                   "Lower Extremity" = "4",
#                                                   "Unspecified" = "5")),
#     reference = list("sex" = c("Male")),
#     levels = list("localisation" = c("Trunk", "Head")),
#     numeric = c("mean", "sd"),
#     censur = T,
#     simplify=list("Immunohistochemistry" = c("Cd10", "Sox10", "Ck"),
#                   "Tumor" = c("Necrosis", "Margins")),
#     flextable=F)

tablR <- function(data,
                  group,
                  vars,
                  test=FALSE,
                  total=FALSE,
                  numeric = c("median", "q1q3", "range"),
                  direction="colwise",
                  reference = list(),
                  levels = list(),
                  labs.groups = list(),
                  labs.headings = list(),
                  labs.subheadings= list(),
                  reverse = F,
                  test.stats = c("kwt", "chisq"),
                  show.na = FALSE,
                  censur=F,
                  digits = 1,
                  simplify = list(),
                  print = F,
                  flextable = T) {

  numeric_choices <- c("median", "q1q3", "iqr", "range", "mean", "sd", "min", "max")

  if(any(numeric %nin% numeric_choices)) {

    cat(paste0("Error: ", numeric[numeric %nin% numeric_choices], " is not a valid option. Valid choices are: \n"), paste0(c(numeric_choices), sep = "\n"))

  }

  direction <- match.arg(direction, c("colwise", "rowwise"))
  test.stats <- match.arg(test.stats, c("kwt", "chisq", "anova"), several.ok = T)

  data <- as.data.frame(data)

  if(direction == "rowwise") {
    categorical <- "countrowpct"
  } else {
      categorical <- "countpct"
  }

  if(show.na) {
    numeric <- c("Nmiss", numeric)
    categorical <- c("Nmiss", categorical)
  }


   vars_c <- data %>% select({{vars}}) %>% names()

   #Group formatting

   if(!missing(group)) {

    group_c <- data %>% select({{group}}) %>% names()

    if(all(class(data[, group_c]) %nin% c("factor", "character"))) {
      cat("Error: Group is not a factor or character")
    }

    if(length(labs.groups) == 0) {
      if(is.factor(data[[group_c]])) {

        labs.groups <- as.list(levels(data[[group_c]])) %>% set_names(str_to_title(str_replace_all(levels(data[[group_c]]), "_", " ")))

      } else {

        labs.groups <- as.list(unique(data[[group_c]])) %>% set_names(str_to_title(str_replace_all(unique(data[[group_c]]), "_", " ")))

      }
    }

    data <- data %>%
    factR(group_c,
          labels = list(labs.groups) %>% set_names(group_c),
          lab_to_lev = T,
          reverse=T)

    if(!reverse) {
      data <- data %>% mutate(!!sym(group_c) := fct_rev(!!sym(group_c)))
    }

   }

   #Autoformat levels
   r_list <- list()

   for(v in vars_c[vars_c %nin% names(labs.subheadings)]) {

     if(is.factor(data[[v]])) {

       r_list[[v]] <-
       as.list(levels(data[[v]])) %>% set_names(str_to_title(str_replace_all(levels(data[[v]]), "_", " ")))

     }

     if(is.character(data[[v]])) {

       r_list[[v]] <-
         as.list(unique(data[[v]])) %>% set_names(str_to_title(str_replace_all(unique(data[[v]]), "_", " ")))

     }

     }

   data <- data %>%
     recodR(append(r_list, labs.subheadings))




if(all(length(reference) > 0 | length(levels) > 0)) {

  data <- data %>%
    factR(c(names(reference), names(levels)),
          reference = reference,
          levels = levels)



}

  c <- tableby.control(test=test, total=total,
                       numeric.test=test.stats[1], cat.test=test.stats[2],
                       numeric.stats=numeric,
                       cat.stats=categorical,
                       stats.labels=list(median="Median", q1q3="Q1, Q3", iqr = "IQR", mean = "Mean", sd="SD", range = "Range", Nmiss = "Missing")
                       )


  if(missing(group)) {
    form <- paste0(" ~ ", paste0(vars_c, collapse="+"))
  } else {
  form <- paste0(group_c, " ~ ", paste0(vars_c, collapse="+"))
  }


  table <- tableby(as.formula(form), data=data, control=c)

  #Autoformatting (to_title and spacing)
  headings_default <- as.list(str_to_title(str_replace_all(vars_c, "_", " "))) %>% set_names(vars_c)
  if(length(labs.headings) > 0) {
  headings_rev <- list(names(labs.headings)) %>% set_names(labs.headings)

  labs.headings <- modifyList(headings_default, headings_rev)
  } else {
  labs.headings <- headings_default
}

tab <- summary(table,
        text=T,
        labelTranslations = labs.headings,
        digits = digits) %>%
  as.data.frame() %>%
  rename("var" = 1)

if(censur) {

  tab <- tab %>% mutate(across(everything(), ~ ifelse(str_detect(., "^(1|2|3)\\s\\("), "<=3", .)))

}

if(length(simplify) > 0) {

  for(i in seq_along(simplify)) {

    labels <- paste0("-  ", simplify[[i]])

    #Find variable names
    indices <- which(str_detect(tab[,"var"], paste0(simplify[[i]], collapse="|")))
    #Total range including levels
    range <- c(min(indices):(max(indices)+2))
    #Remove levels 0, No etc.
    tab <- tab %>% filter(!(str_detect(var, "\\b0|No|no\\b") & row_number() %in% range))
    #Update indices
    indices <- which(str_detect(tab[,"var"], paste0(simplify[[i]], collapse="|")))
    #Rename
    tab[min(indices),"var"] <- names(simplify[i])
    #Remove variable names
    tab <- tab[-indices[-1],]
    #Add labels
    tab[c((min(indices)+1):(min(indices)+length(simplify[[i]]))),"var"] <- paste0("-  ", simplify[[i]])
  }


}

tab[,1] <- str_pad(str_trim(tab[,1]), width = max(str_count(str_trim(tab[,1]))), side = "right")
tab[,1] <- str_replace(tab[,1], "-", " ")
colnames(tab)[1] <- " "

if(print) {
  print(tab)
}

if(flextable) {

  #Shift values to the left with padding
  padding <- paste0(c(labs.headings[labs.headings %nin% unlist(simplify)], names(simplify)), collapse = "|")
  padding

  rows <- which(str_detect(tab[,1], padding, negate=T))

  #Pad levels right for flextable
  return(tab %>% flextable %>%
    padding(i = c(rows), j=1, padding.left = 15))

} else {

  class(tab) <- c("data.frame", "tablR")

  return(tab)

}

}
