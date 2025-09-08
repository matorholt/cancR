#' extractR
#'
#' @description
#' Automatic extraction of key results from the estimatR function
#'
#'
#' @param list estimatR object
#' @param vars Which outcome measures to provide (counts, risks, differneces and/or ratios)
#' @param format Format of the table (long or wide).
#' @param ci The format of the confidence intervals (default = 95%CI)
#' @param nsep The separator between event counts (default = /)
#' @param sep The separator between all other ranges (default = to)
#' @param censur Whether counts <= 3 should be censured (default = FALSE)
#' @param risk_digits number of digits on risk estimates
#' @param diff_digits number of digits on risk differences
#' @param ratio_digits number of digits on risk ratios
#'
#' @return Returns a data frame compatible with flextable
#' @export
#'
#'

# n <- 3000
# set.seed(1)
# df <- riskRegression::sampleData(n, outcome="survival")
# df$time <- round(df$time,1)*12
# df$time2 <- df$time + rnorm(n)
# df$X1 <- factor(rbinom(n, prob = c(0.3,0.4) , size = 2), labels = paste0("T",0:2))
# df$X3 <- factor(rbinom(n, prob = c(0.3,0.4,0.3) , size = 3), labels = paste0("T",0:3))
# df$event2 <- rbinom(n, 2, prob=.3)
# df <- as.data.frame(df)
#
# df2 <- df %>% mutate(X2 = ifelse(row_number()==1, NA, X2),
#                      event = as.factor(event)) %>%
#   rename(ttt = time) %>%
#   mutate(X2 = ifelse(X2 == 1, "No CLL", "CLL"))
#
# t2 <- estimatR(df2, ttt, event2, X2, type = "select", vars = c(X6,X7))
# t3 <- estimatR(df2, ttt, event2, X3, type = "select", vars = c(X6,X7))

# extractR(t3)
# extractR(t2, format = "wide")


# extractR(t2, format = "wide", vars = c("counts", "risks", "diff", "ratio"), flextable = T, sep.ci = F, reverse = F) %>%
#   flextable() %>%
#   separate_header()





extractR <- function(list,
                     vars = c("counts", "risks", "diff"),
                     format ="long",
                     ci = "95%CI ",
                     nsep = " / ",
                     sep = " to ",
                     censur = F,
                     risk_digits = 1,
                     diff_digits = 1,
                     ratio_digits = 1,
                     sep.ci = F,
                     flextable = F,
                     reverse = T) {

  vars <- match.arg(vars, c("counts", "risks", "diff", "ratio"), several.ok=TRUE)

  grps <- list$info$group_levels

  if(censur) {
    list$counts <- list$counts %>%
      mutate(n.events = ifelse(n.events <= 3, "<=3", n.events))
  }

  counts <-
    list$counts %>% mutate(counts = str_c(n.events, nsep, total)) %>%
    select(counts) %>%
    as.data.frame()

  risks <-
    list$risks %>% filter(time %in% list$info$time) %>% mutate(across(c(est, lower, upper), ~ numbR(.*100, risk_digits)),
                                                    risks = str_c(est, "%x(", ci, lower, sep, upper, ")")) %>%
    select(risks)
  diff <-
    list$difference %>% slice(1,1:(length(grps)-1)) %>%
    mutate(diff = ifelse(row_number() > 1, str_c(numbR(diff, diff_digits), "%x(", ci, numbR(lower, diff_digits), sep, numbR(upper, diff_digits), ")xz", p.value), "reference")) %>%
    select(diff)

  ratio <-
    list$ratio %>% slice(1,1:(length(grps)-1)) %>%
    rename(rr = ratio) %>%
    mutate(ratio = ifelse(row_number() > 1, str_c(rr, "x(", ci, numbR(lower, ratio_digits), sep, numbR(upper, ratio_digits), ")xz", p.value), "reference")) %>%
    select(ratio)

  total <- bind_cols(counts, risks, diff, ratio)


  if(format == "long") {
    tab <- total %>%
      mutate(!!sym(list$info$group) := grps) %>%
      select(!!sym(list$info$group), {{vars}})


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

      frames[[i]] <- lapply(vars, function(j) {

        total[i, j]

      }) %>% set_names(paste0(grps[i], "_", vars))

      names <- c(names, names(frames[[i]]))

    }

    tab <- as.data.frame(frames)
    colnames(tab) <- names



    tab <- tab[-which(vars %in% c("diff", "ratio"))]





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

        labs <- c("Absolute Risk (95%CI)",
                  "Risk Difference (95%CI)",
                  "Risk Ratio (95%CI)")


        if(sep.ci) {

          labs <- str_remove_all(labs, "\\s\\(95%CI\\)")

        }

          names(tab) <- str_replace_all(names(tab),
                                        c("counts" = "Counts (events/total)",
                                          "(.*)diff_p.value" = "\\1P-Value",
                                          "(.*)ratio_p.value" = "\\1P-Value ",
                                          "risks" = labs[1],
                                          "diff" = labs[2],
                                          "ratio" = labs[3],
                                          "ci" = "(95%CI)"))


      }


tab


}


