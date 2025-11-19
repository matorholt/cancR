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

# redcap_df %>%
#   mutate(margins = sample(c("0","1"), nrow(redcap_df), replace=TRUE)) %>%
#   factR(c(type, sex, localisation, cd10, sox10, ck, margins, necrosis)) %>%
#   tablR(group=type,
#          vars = c(age, sex, localisation, cd10, sox10, ck, necrosis, margins),
#          labs.groups = list("type" = list("Benign" = "0",
#                                           "In situ" = "1",
#                                           "Malignant" = "2")),
#          reverse = T,
#          labs.headings = list("Age at Debut" = "age",
#                               "Gender" = "sex",
#                               "CD10" = "cd10"),
#          labs.subheadings = list("sex" = list("Female" = "2",
#                                               "Male" = "1"),
#                                  "localisation" = list("Neck" = "0",
#                                                        "Head" = "1",
#                                                        "Trunk" = "2",
#                                                        "Upper Extremity" = "3",
#                                                        "Lower Extremity" = "4",
#                                                        "Unspecified" = "5")),
#          reference = list("sex" = c("Male")),
#          simplify=list("Immunohistochemistry" = c("cd10", "sox10", "ck"),
#                        "Tumor" = c("necrosis", "margins")),
#          censur=F,
#          print=F,
#          numeric = c("mean", "sd", "q1q3", "iqr"),
#          flextable = F,
#          test=T,
#          total=T)

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
                   flextable = F) {

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


  vars_c <- data %>% select({{vars}}) %>% names
  #In case you forgot
  vars_c <- unique(c(vars_c, names(labs.subheadings)))
  vars_cat <- data %>% select(where(is.factor) | where(is.character)) %>% names

  #Group formatting
  if(!missing(group)) {

    group_c <- data %>% select({{group}}) %>% names

    if(all(class(data[, group_c]) %nin% c("factor", "character"))) {
      cat("Error: Group is not a factor or character")
    }

    #Group name inserted if omitted from argument
    if(pluck_depth(labs.groups) == 2) labs.groups <- list(labs.groups) %>% set_names(group_c)

    data <- data %>%
      factR(group_c,
            labels = labs.groups,
            lab_to_lev = T,
            reverse = reverse)

  }

  #Format levels
  data <- data %>%
    factR(vars=c(vars_c[vars_c %in% vars_cat]),
          labels = labs.subheadings,
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
                       stats.labels=list(median="Median", q1q3="Q1, Q3", iqr = "IQR", mean = "Mean", sd="SD", range = "Range", Nmiss = "Missing")
  )


  if(missing(group)) {
    form <- paste0(" ~ ", paste0(vars_c, collapse="+"))
  } else {
    form <- paste0(group_c, " ~ ", paste0(vars_c, collapse="+"))
  }


  table <- tableby(as.formula(form), data=data, control=c)

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
      indices <- which(str_detect(tab[,"var"], paste0("\\b", simplify[[i]], "\\b", collapse="|")))
      #Total range including levels
      range <- c(min(indices):(max(indices)+2))
      #Remove levels 0, No etc.
      tab <- tab %>% filter(!(str_detect(var, "\\b0|No|no\\b") & row_number() %in% range))
      #Update indices
      indices <- which(str_detect(tab[,"var"], paste0("\\b", simplify[[i]], "\\b", collapse="|")))
      #Rename
      tab[min(indices),"var"] <- names(simplify[i])
      #Remove variable names
      tab <- tab[-indices[-1],]
      #Add labels
      tab[c((min(indices)+1):(min(indices)+length(simplify[[i]]))),"var"] <- paste0("-  ", simplify[[i]])
    }



  }

  headings <- c(vars_c[vars_c %nin% unlist(simplify)], names(simplify))
  headings_index <- which(tab[, 1] %nin% headings)

  if(length(labs.headings) > 0) {

    tab <- tab %>%
      mutate(var = str_replace_all(var, names(labs.headings) %>% set_names(labs.headings)))

  }

  #Formatting
  tab <- tab %>%
    #Pad all cells
    mutate(across(c(2:ncol(tab)), ~ str_pad(., width = max(str_count(.)), side ="both")),
           #Fix column 1
           #Remove -
           var = str_remove(var, "-\\s{2}"),
           #Autoformat to upper case
           var = ifelse(str_detect(var, paste0("\\b", names(labs.headings), "\\b", collapse="|")), var, str_to_title(var)),
           var = case_when(str_detect(var, "\\bSd\\b") ~ "SD",
                           str_detect(var, "\\bIqr\\b") ~ "IQR",
                           T ~ var),
           var = ifelse(row_number() %in% headings_index, paste0("xzx", var), var),
           var = str_pad(str_trim(var), width = max(str_count(str_trim(var))), side = "right"),
           var = str_remove(var, "xzx")) %>%
    rename(" " = 1)

  if(test) {

    tab <- tab %>%
      rename("p-value" = `p value`) %>%
      mutate("p-value" := ifelse(`p-value` != "     ", pvertR(as.numeric(`p-value`)), " "))

  }

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

