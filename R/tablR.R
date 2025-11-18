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
                  simplify = F) {

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

     if(any(class(data[[v]]) %in% c("factor", "character"))) {

     labs.levels <- unique(data[[v]])

     r_list[[v]] <- as.list(labs.levels) %>% set_names(str_to_title(str_replace_all(labs.levels, "_", " ")))
     }


   }

   data <- recodR(data,
                  append(r_list, labs.subheadings))

   append(r_list, labs.subheadings)

if(all(length(reference) > 0 | length(levels) > 0)) {

  data <- data %>%
    factR(c(names(reference), names(levels)),
          reference = reference,
          levels = levels)



}



   if(simplify) {

     data <- data %>%
       mutate(across(c({{vars}}), ~ if_else(. %in% c("0", 0, "No", "no"), NA, .)),
              across(c(where(is.factor)), ~ fct_drop(.)))

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

s <- summary(table,
        text=T,
        labelTranslations = labs.headings,
        digits = digits)

if(censur) {

  if(missing(group)) {
    loop_string <- "Total"

  } else {
  loop_string <- c(as.character(unique(data[, group_c])), "Total")
  }

  for(v in loop_string) {

    for(i in 1:length(s$object[[1]][v][[1]])) {

      if("tbstat_countpct" %in% class(s$object[[1]][v][[1]][[i]]) & s$object[[1]][v][[1]][[i]][1] != "" & s$object[[1]][v][[1]][[i]][1] <= 3) {

        s$object[[1]][v][[1]][[i]]<- "<=3"

      }

    }
  }
}

s


}

# tab <- redcap_df %>%
#   factR(c(type, sex, localisation, cd10, sox10, ck)) %>%
#   tablR(
#     group = type,
#     vars=c(age, sex, localisation, cd10, sox10, ck),
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
#     reference = list("sex" = c("Female")),
#     levels = list("localisation" = c("Trunk", "Head")),
#     numeric = c("mean", "sd"),
#     simplify=T)
#
# tab
#
# tab_df <- as.data.frame(tab)
#
# simplify.vars <- c("Cd10", "Sox10", "Ck")
#
# indices <- which(str_detect(unlist(tab_df[1]), paste0(simplify.vars, collapse="|")))
#
# tab_df[indices[1],1] <- simplify.
#
# tab_df <- tab_df[-indices[-1],]
#
# tab_df[c(seq(indices[1]+1, indices[1]+length(indices))), 1] <- simplify.vars
#
# tab_df
#
#
