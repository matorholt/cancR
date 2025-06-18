#' extractR
#'
#' @description
#' Automatic extraction of key results from the estimatR function
#'
#'
#' @param list estimatR object
#' @param vars Which outcome measures to provide (counts, risks, differneces and/or ratios)
#' @param shape Format of the table (long or wide).
#' @param ci The format of the confidence intervals (default = 95%CI)
#' @param nsep The separator between event counts (default = /)
#' @param sep The separator between all other ranges (default = to)
#' @param censur Whether counts <= 3 should be censured (default = FALSE)
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
#   rename(ttt = time)
#
# t2 <- estimatR(df2, ttt, event2, X2, type = "select", vars = c(X6,X7))
#
# (t <- extractR(t2, vars=c("counts", "risks", "ratio"), shape = "long", ci = "", nsep = " of ", sep =" to ", censur=T))

extractR <- function(list,
                     vars = c("counts", "risks", "diff"),
                     shape="long",
                     ci = "95%CI ",
                     nsep = " / ",
                     sep = " to ",
                     censur = F) {

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
    list$risks %>% filter(time %in% 120) %>% mutate(across(c(est, lower, upper), ~ numbR(.*100)),
                                                    risks = str_c(est, "%x(", ci, lower, sep, upper, ")")) %>%
    select(risks)
  diff <-
    list$difference %>% slice(1,1:(length(grps)-1)) %>%
    mutate(diff = ifelse(row_number() > 1, str_c(numbR(diff), "%x(", ci, numbR(lower), sep, numbR(upper), ")x", p.value), "reference")) %>%
    select(diff)

  ratio <-
    list$ratio %>% slice(1,1:(length(grps)-1)) %>%
    rename(rr = ratio) %>%
    mutate(ratio = ifelse(row_number() > 1, str_c(rr, "%x(", ci, numbR(lower), sep, numbR(upper), ")x", p.value), "reference")) %>%
    select(ratio)

  total <- bind_cols(counts, risks, diff, ratio)

  if(shape == "long") {
    tab <- total %>%
      mutate(!!sym(list$info$group) := grps) %>%
      select(!!sym(list$info$group), {{vars}}) %>%
      separate("risks", into=c("risks", paste0("risks", "_ci")), sep = "x", fill="right")


    suppressWarnings(for(s in vars[vars != c("counts", "risks")]) {
      tab <- tab %>% separate(s, into=c(s, paste0(s, "_ci"), paste0(s, "_p.value")), sep = "x", fill="right")
    })

    tab <- tab %>% mutate(across(everything(), ~ ifelse(is.na(.), "reference", .)))
  }

  if(shape == "wide")
  {
    frames <- list()
    names <- c()

    for(i in 1:length(grps)) {

      frames[[i]] <- lapply(vars, function(j) {

        total[i, j]

      }) %>% set_names(paste0(grps[i], "_", vars))

      names <- c(names, names(frames[[i]]))

    }

    tab <- as.data.frame(frames)
    colnames(tab) <- names

    tab <- tab[-which(vars %in% c("diff", "ratio"))]

    for(i in names[str_detect(names, "risks")]) {
      tab <- tab %>%
        separate(i, into = c(i, paste0(i, "_ci")), sep = "x", fill = "right")
    }

    for(i in names[str_detect(names, "diff|ratio") & str_detect(names, grps[1], negate=T)]) {
      tab <- tab %>%
        separate(i, into = c(i, paste0(i, "_ci"), paste0(i, "_p.value")), sep = "x", fill = "right")
    }

  }
tab

}
