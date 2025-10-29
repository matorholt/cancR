#' Extraction of key results from the estimatR function
#'
#'
#' @param list estimatR object
#' @param outcome Which outcome measures to provide (counts, risks, differneces and/or ratios)
#' @param format Format of the table (long or wide).
#' @param ci The format of the confidence intervals (default = 95%CI)
#' @param nsep The separator between event counts (default = /)
#' @param sep The separator between all other ranges (default = to)
#' @param censur Whether counts <= 3 should be censured (default = FALSE)
#' @param risk.digits number of digits on risk estimates
#' @param diff.digits number of digits on risk differences
#' @param ratio.digits number of digits on risk ratios
#'
#' @return Returns a data frame compatible with flextable
#' @export
#'
#'

# df2 <- analysis_df %>%
#   mutate(X2 = ifelse(X2 == 1, "No CLL", "CLL"))
#
# t2 <- estimatR(df2, ttt, event2, X2, type = "select", vars = c(X6,X7))
# t3 <- estimatR(df2, ttt, event2, X3, type = "select", vars = c(X6,X7))
#
# extractR(t3)
# extractR(t2, format = "wide")
# (tab <- extractR(t2,
#                  format = "wide",
#                  outcome = c("counts", "risks", "diff", "ratio"),
#                  total.count = T,
#                  flextable = T,
#                  sep.ci = F,
#                  reverse = F,
#                  labs = list("diff" = "Absolute Risk Difference (95%CI)"),
#                  headings = list("No CLL" = "No CLL (n=123)",
#                                  "CLL" = "CLL (n=345)"),
#                  ci = ""))
#
# tab %>% flextable() %>%
#   separate_header()


extractR <- function(list,
                     outcome = c("counts", "risks", "diff"),
                     format ="long",
                     ci = "95%CI ",
                     nsep = " / ",
                     sep = " to ",
                     total.count = T,
                     labs = NULL,
                     headings = NULL,
                     censur = F,
                     risk.digits = 1,
                     diff.digits = 1,
                     ratio.digits = 1,
                     sep.ci = F,
                     flextable = F,
                     reverse = T) {

  outcome <- match.arg(outcome, c("counts", "risks", "diff", "ratio"), several.ok=TRUE)

  if(class(list) %in% c("incidencR", "clustR")) outcome <- c("counts", "risks")

  grps <- list$info$group_levels

  if(censur) {
    list$counts <- list$counts %>%
      mutate(n.events = ifelse(n.events <= 3, "<=3", n.events))
  }

  counts <-
    list$counts %>%
    rowwise() %>%
    mutate(counts = ifelse(total.count, str_c(n.events, nsep, total), n.events)) %>%
    select(counts) %>%
    as.data.frame()



  risks <-
    list$risks %>% filter(time %in% list$info$time) %>% mutate(across(c(est, lower, upper), ~ numbR(.*100, risk.digits)),
                                                    risks = str_c(est, "%x(", ci, lower, sep, upper, ")")) %>%
    select(risks)

  if(class(list) == "incidencR") {
    diff <- data.frame(diff = rep("NA", length(grps)))
  } else {
   diff <-
    list$difference %>%
     tibble::remove_rownames() %>%
     slice(1,1:(length(grps)-1)) %>%
    mutate(diff = ifelse(row_number() > 1, str_c(numbR(diff, diff.digits), "%x(", ci, numbR(lower, diff.digits), sep, numbR(upper, diff.digits), ")xz", p.value), "reference")) %>%
    select(diff)
  }

   if(class(list) %in% c("incidencR", "clustR")) {
     ratio <- data.frame(ratio = rep("NA", length(grps)))
   } else {
  ratio <-
    list$ratio %>% slice(1,1:(length(grps)-1)) %>%
    rename(rr = ratio) %>%
    mutate(ratio = ifelse(row_number() > 1, str_c(rr, "x(", ci, numbR(lower, ratio.digits), sep, numbR(upper, ratio.digits), ")xz", p.value), "reference")) %>%
    select(ratio)

   }

  total <- bind_cols(counts, risks, diff, ratio)


  if(format == "long") {
    tab <- total %>%
      mutate(!!sym(list$info$group) := grps) %>%
      select(!!sym(list$info$group), {{outcome}})


    for(i in names(tab)[str_detect(names(tab), "diff|ratio")]) {

    #Separate p-values

           tab <- tab %>%
            separate(i, into = c(i, paste0(i, "_p.value")), sep = "xz", fill = "right")

    }

    #Separate confidence intervals

for(i in names(tab)[str_detect(names(tab), "(risks|diff|ratio)(?!(_))")]) {

      if(sep.ci) {
        tab <- tab %>%
        separate(i, into = c(i, paste0(i, "_ci")), sep = "x", fill = "right")

       } else {


      tab <- tab %>%
        mutate(across(everything(), ~ str_replace_all(., "x", " ")))


       }

}



    tab <- tab %>% mutate(across(everything(), ~ ifelse(is.na(.), "reference", .)))



  }


  if(format == "wide")
  {
    frames <- list()
    names <- c()

    #Pivot wider

    for(i in 1:length(grps)) {

      frames[[i]] <- lapply(outcome, function(j) {

        total[i, j]

      }) %>% set_names(paste0(grps[i], "_", outcome))

      names <- c(names, names(frames[[i]]))

    }

    tab <- as.data.frame(frames)
    colnames(tab) <- names



    tab <- tab[-which(outcome %in% c("diff", "ratio"))]





for(i in names(tab)[str_detect(names(tab), "diff|ratio")]) {

#Separate p-values

  tab <- tab %>%
    separate(i, into = c(i, paste0(i, "_p.value")), sep = "xz", fill = "right")

}

    #Separate confidence intervals

for(i in names(tab)[str_detect(names(tab), "(risks|diff|ratio)(?!(_))")]) {

  if(sep.ci) {
    tab <- tab %>%
      separate(i, into = c(i, paste0(i, "_ci")), sep = "x", fill = "right")

  } else {


    tab <- tab %>%
      mutate(across(everything(), ~ str_replace_all(., "x", " ")))


  }

}



    if(reverse) {

      order_frame <-
        data.frame(v = names(tab)) %>%
        mutate(grp = str_extract(v, ".*?(?=(_))"),
               index = match(grp, grps),
               suffix = str_extract(v, "(?<=(_)).*")) %>%
        group_by(grp) %>%
        mutate(order = row_number(),
               prio = ifelse(str_detect(v, "count|risk"), 1, 2))
      if(length(grps) == 2) {
        order_frame <-
          order_frame %>%
          arrange(prio, desc(index), order)



      } else {
        order_frame <-
          order_frame %>%
          arrange(desc(index), order)

      }



    tab <- tab %>%
      select(order_frame$v)


    if(length(grps) == 2) names(tab) <- str_replace(names(tab), ".*(?=((diff|ratio)))", " ")

    }


}

      if(flextable) {

        labs.default <- list(
          "counts" = "Events/Total",
          "risk" = "Absolute Risk (95%CI)",
          "diff" = "Risk Difference (95%CI)",
          "ratio" = "Risk Ratio (95%CI)",
          "ci" = "(95%CI)")

        if(!is.null(labs)) {
          labs <- modifyList(labs.default, labs)
        } else {
          labs <- labs.default
        }

        for(x in seq_along(labs)) {

          if(names(labs)[[x]] != "ci" & sep.ci) {
            labs[[x]] <- str_remove_all(labs[[x]], "\\s\\(95%CI\\)")
          }

          if(!total.count) {
            labs[[x]] <- str_remove_all(labs[[x]], "/Total")
          }

        }


          names(tab) <- str_replace_all(names(tab),
                                        c("counts" = labs[["counts"]],
                                          "(.*)diff_p.value" = "\\1P-Value",
                                          "(.*)ratio_p.value" = "\\1P-Value ",
                                          "risks" = labs[["risk"]],
                                          "diff" = labs[["diff"]],
                                          "ratio" = labs[["ratio"]],
                                          "ci" = labs[["ci"]]))

          if(!is.null(headings)) {

            for(x in seq_along(headings)) {

              names(tab) <-
                str_replace(names(tab), paste0("^",names(headings)[[x]]), headings[[x]])
            }

          }


      }


class(tab) <- c("extractR", "data.frame")
tab

}

