#' Create frequency tables
#'
#' @description
#' Wrapper for the tableby function in the Arsenal package
#'
#' @param data dataframe
#' @param group Grouping variable. If omitted a single column tabel is provided
#' @param vars Variables that the grouping variable should be aggregated by
#' @param num.vars vector of variables that should be sorted based on numerical order
#' @param test Whether statistical tests should be performed (default = FALSE)
#' @param total Whether a total column should be included (default = FALSE)
#' @param numeric Selection of the type of stats for numerical variables.
#' Options include: median, q1q3, medianq1q3 (default), iqr, range, mean, sd, meansd, min, max)
#' @param direction Direction for percentages (colwise or rowwise)
#' @param labels List specifying labels of the specific labels for each variable
#' @param reference List specifying reference group for each variable
#' @param labs.headings List specifying labels for variable names
#' @param reverse whether the order of groups should start with the highest level (default = T)
#' @param test.stats Vector of length 2 containing statistical tests that should be performed
#' @param show.na Whether NAs should be presented
#' @param censur whether counts <= 3 should be censored
#' @param weights optional name of the column containing weights for weighted summaries
#' @param digits number of digits
#' @param ama whether percentages >10 should be without digits per AMA journal of style. Default = T.
#' @param simplify a list of column names that should be simplified by dropping specified levels (e.g. 0 or "no") for simple output.
#' Groups of size > 1 should be named such as: list("IHC" = c("cd10", "sox10", "ck"), "necrosis", "margins")
#' @param simplify.remove vector of labels that should be removed from the columns assigned in the "simplify" argument. Default = c("no", "0")
#' @param print whether the table should be printed in the console
#' @param flextable whether the table should be returned as a flextable
#'
#' @return Returns a table as a dataframe or flextable
#' @export
#'
#' @examples
#' tablR(population_denmark,
#'      group = sex,
#'      vars = c(age_group, population))

# redcap_df %>%
#   mutate(margins = sample(c("0","1"), nrow(redcap_df), replace=TRUE),
#          w = runif(nrow(redcap_df), 1,5)) %>%
#   #mutate(type = ifelse(row_number() == 1, NA, type)) %>%
#   factR(c(type, sex, localisation, cd10, sox10, ck, margins, necrosis)) %>%
#   tablR(group=type,
#         numeric = c("meansd","range"),
#         vars = c(age, sex, localisation, cd10, sox10, ck, necrosis, margins),
#         labs.groups = list("type" = list("Benign" = "0",
#                                          "In situ" = "1",
#                                          "Malignant" = "2")),
#         reverse = T,
#         labs.headings = list("Age at Debut" = "age",
#                              "gender" = "sex",
#                              "Cluster of diff 10" = "cd10",
#                              "SOX10" = "sox10"),
#         labs.subheadings = list("sex" = list("Female" = "2",
#                                              "Male" = "1"),
#                                 "localisation" = list("Neck" = "0",
#                                                       "Head" = "1",
#                                                       "Trunk" = "2",
#                                                       "Upper Extremity" = "3",
#                                                       "Lower Extremity" = "4",
#                                                       "Unspecified" = "5")),
#         reference = list("sex" = c("male")),
#         #simplify = c("necrosis", "margins"),
#         # simplify=list("Immunohistochemistry" = c("cd10", "sox10", "ck"),
#         #               "Tumor" = c("necrosis", "margins")),
#         simplify=list("Immunohistochemistry" = c("cd10", "sox10", "ck"),
#                       "necrosis",
#                       "margins"),
#         print=F,
#         weights = w)


tablR <- function(data,
                  group,
                  vars,
                  num.vars,
                  test=FALSE,
                  total=FALSE,
                  numeric = c("medianq1q3", "range"),
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
                  weights,
                  digits = 1,
                  ama = T,
                  simplify = list(),
                  simplify.remove = c("no", "0"),
                  print = F,
                  flextable = F) {

  numeric_choices <- c("median", "q1q3", "medianq1q3", "iqr", "range", "mean", "sd", "meansd", "min", "max")

  if(any(numeric %nin% numeric_choices)) {

    return(cat(paste0("Error: ", numeric[numeric %nin% numeric_choices], " is not a valid option. Valid choices are: \n"), paste0(c(numeric_choices), sep = "\n")))

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


  vars_c <- data %>% select({{vars}}) %>% names
  #In case you forgot
  vars_c <- unique(c(vars_c, names(labs.subheadings)))
  vars_cat <- data %>% select(where(is.factor) | where(is.character)) %>% names
  num_c <- data %>% select({{num.vars}}) %>% names

  if(!missing(weights)) {
    weights_c <- data %>% select({{weights}}) %>% names
  } else {
    weights_c <- ""
  }

  #Group formatting
  if(!missing(group)) {

    group_c <- data %>% select({{group}}) %>% names

    if(all(class(data[, group_c]) %nin% c("factor", "character"))) {
      return(cat("Error: Group is not a factor or character"))
    }

    #Group name inserted if omitted from argument
    if(length(labs.groups) == 0) {
     # return(data[[group_c]])
      if(is.factor(data[[group_c]])) {
        labs.groups <- listR(levels(data[[group_c]]), type = "vec2list")
      } else {
        labs.groups <- as.list(as.character(na.omit(unique(data[[group_c]])))) %>% set_names(str_to_title(na.omit(unique(data[[group_c]]))))
      }

    }



       if(pluck_depth(labs.groups) == 2) {

         labs.groups <-
           list(labs.groups) %>% set_names(group_c)

       }

    data <- data %>%
      factR(vars=group_c,
            labels = labs.groups,
            lab_to_lev = T,
            reverse = reverse)

  }

  #Format levels
  data <- data %>%
    factR(vars=c(vars_c[vars_c %in% vars_cat]),
          num.vars = num_c,
          labels = labs.subheadings,
          levels = levels,
          reference = reference,
          lab_to_lev=T)
  #Set reference
  data <- data %>%
    factR(vars = names(reference),
          reference = reference)



  #Table controls
  c <- tableby.control(test=test, total=total,
                       numeric.test=test.stats[1], cat.test=test.stats[2],
                       numeric.stats=numeric,
                       cat.stats=categorical,
                       numeric.simplify = T,
                       stats.labels=list(median="Median",
                                         medianq1q3 = "Median (Q1, Q3)",
                                         meansd = "Mean (SD)",
                                         q1q3="Q1, Q3",
                                         iqr = "IQR",
                                         mean = "Mean",
                                         sd="SD",
                                         range = "Range",
                                         Nmiss = "Missing")
  )


  if(missing(group)) {
    form <- paste0(" ~ ", paste0(vars_c, collapse="+"))
  } else {
    form <- paste0(group_c, " ~ ", paste0(vars_c, collapse="+"))
  }


  table <- tableby(as.formula(form),
                   data=data,
                   control=c,
                   weights = eval(parse(text = weights_c)))

  if(length(labs.headings) > 0) {
  headings_reverse <- names(labs.headings) %>% set_names(labs.headings)
  } else {
    headings_reverse <- labs.headings
  }

  tab <- summary(table,
                 text=T,
                 labelTranslations = headings_reverse,
                 digits = digits) %>%
    as.data.frame() %>%
    rename("var" = 1)

  if(censur) {

    tab <- tab %>% mutate(across(everything(), ~ ifelse(str_detect(., "^(1|2|3)\\s\\("), "<=3", .)))

  }

  if(ama) {

    tab <- tab %>% mutate(across(everything(), ~ ifelse(str_detect(., "%"),
                                                        ifelse(as.numeric(str_extract(., "\\d+\\.\\d*(?=(%))")) >= 10,
                                                               str_replace(., "\\d+\\.\\d*(?=(%))", as.character(round(as.numeric(str_extract(., "\\d+\\.\\d*(?=(%))")),0))),
                                                               .)

                                                        , .)))

  }

  #Update names from labs.headings
  if(length(labs.headings) > 0 & class(simplify) == "list") {
    simplify <- lapply(simplify, function(i) {
      str_replace_all(unlist(i), names(labs.headings) %>% set_names(labs.headings))
    })
  }

  #SIMPLIFICATION

    if(length(simplify) > 0) {

      #Remove pattern
      remove <- paste0("\\b(", paste0(simplify.remove, collapse = "|"), ")\\b", collapse="")

      #Convert simplify to named list if vector
    if(class(simplify) != "list") {
      simplify <- as.list(simplify) %>% set_names(simplify)

    }

    for(i in seq_along(simplify)) {

      #Only multiple categories gets "-" for right-shifting
      if(length(simplify[[i]]) > 1) {
         labels <- paste0("-  ", simplify[[i]])
      } else {
        labels <- simplify[[i]]
      }

      #Find variable names
      indices <- which(str_detect(tab[,"var"], paste0("\\b", simplify[[i]], "\\b", collapse="|")))

      #Total range including levels
      range <- c(min(indices):(max(indices)+2))

      #Remove levels 0, No etc.
      tab <- tab %>% filter(!(str_detect(var, remove) & row_number() %in% range))

      #Update indices
      indices <- which(str_detect(tab[,"var"], paste0("\\b", simplify[[i]], "\\b", collapse="|")))

      #Rename
      tab[min(indices),"var"] <- names(simplify[i])


      #Remove variable names (remove first index but keep first if length(index) == 1)
      tab <- tab[-indices[1-(length(indices) > 1) * 2],]


      # #Add labels
      tab[c((min(indices)+(length(indices)>1)):(min(indices)+length(simplify[[i]]) * (length(indices) > 1))),"var"] <- labels

    }



  }

  headings_index <- which(str_detect(tab[, "var"], "-"))

  if(test) {

    print(tab)
    tab <- tab %>%
    rename("P-value" = `p value`) %>%
      mutate(`P-value` = pvertR(`P-value`, na= " "))
  }


  #Formatting
  tab <- tab %>%
    #Pad all cells
    mutate(across(c(2:ncol(tab)), ~ str_pad(., width = max(str_count(.)), side ="both")),
           #Fix column 1
           #Remove -
           var = str_remove(var, "-\\s{2}"),
           #Autoformat to upper case
           var = ifelse(str_detect(var, paste0("\\b", c(names(labs.headings), "xzx"), "\\b", collapse="|")), var, str_to_title(str_replace_all(var, "_", " "))),
           var = case_when(str_detect(var, "\\bSd\\b") ~ str_replace(var, "\\bSd\\b", "SD"),
                           str_detect(var, "\\bIqr\\b") ~ str_replace(var, "\\bIqr\\b", "IQR"),
                           T ~ var),
           var = ifelse(row_number() %in% headings_index, paste0("xzx", var), var),
           var = str_pad(str_trim(var), width = max(str_count(str_trim(var))), side = "right"),
           var = str_remove(var, "xzx")) %>%
    rename(" " = 1)

  if(print) {
    print(tab)
  }

  if(flextable) {

    #Pad levels right for flextable
    return(tab %>% flextable %>%
             padding(i = c(headings_index), j=1, padding.left = 15) %>%
             align(j=c(2:ncol(tab)), align = "center", part = "all") %>%
             width(width=2))

  } else {

    class(tab) <- c("data.frame", "tablR")

    return(tab)

  }


}
