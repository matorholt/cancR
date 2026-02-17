#' Find covariates or outcomes from the registers
#'
#'
#' @param reglist list of dataframe(s)
#' @param search.list list with list(label = diagnosis code) structure.
#' @param sub.list list of variables where the diagnosis code should be kept (e.g. list("KOL" = "subtype"))
#' @param sub.labels list of labels for the diagnosis code colum (e.g. list("subtype" = list("a" = c("DC1", "DC2"))))
#' @param name.list list of labels for the main columns (e.g. list("case" = "diabetes))
#' @param exclusion vector of diagnosis codes for exclusion
#' @param slice which rows should be selected (first(default)/last/all)
#' @param format whether selected rows should contain date or 1
#' @param match the match of the regex code (match, start, end or contains(default))
#' @param casename the name of the case variable (default = "index")
#' @param pnr name of the pnr column
#'
#' @return A dataframe with the selected columns with corresponding diagnosis codes
#' @export
#'
#'
#
# set.seed(1)
#
# n=500
#
# reglist <- list(lpr = simulatR("lpr", n=n,lpr.diag.count = 10),
#                 opr = simulatR("opr", n=n, opr.diag.count = 10),
#                 lmdb = simulatR("lmdb", n = n),
#                 pop = simulatR("pop", n=n*10),
#                 sc = data.frame(pnr = sample(seq(1,n*10), n*10*0.1, replace=F),
#                                 sc_date = sample(c(sample(seq(as.Date('1990/01/01'), as.Date('2020/01/01'), by="day"))), n*10*0.1, replace=TRUE),
#                                 meta_date = sample(c(sample(seq(as.Date('1990/01/01'), as.Date('2020/01/01'), by="day"))), n*10*0.1, replace=TRUE),
#                                 pato_supp = sample(c(sample(seq(as.Date('1990/01/01'), as.Date('2020/01/01'), by="day"))), n*10*0.1, replace=TRUE)))
#

#
# reglist <- lapply(reglist, as.data.frame)
#
# clist <- decodR(list("lpr_case" =
#                        list(supergroup_a =
#                               list(group_a1 = list("sg1" = c("DB6", "DB7"),
#                                                   "sg2" = c("DD22", "DD23")),
#                                    group_b1 = list("sg3" = c("DD4"))),
#
#                             supergroup_b =
#                               list(group_a2 = list("sg4" = c("DE5", "DF"),
#                                                    "sg5" = c("DF", "DG"))),
#
#                             supergroup_c =
#                               list(group_a3 = list("sg6" = c("DJ", "DK"),
#                                                    "sg7" = c("DL", "DM")),
#                                    group_b3 = list("sg8" = c("DN")))),
#
#                      "lpr_ex" = list("e1" = c("DO", "DP"),
#                                      "c2" = c("DR", "DQ")),
#                      "lmdb_ex" = list("immune" = "C0"),
#                      "opr_ex" = list("sotr" = c("DT", "DG", "DK")),
#                      "labels" = list("lpr_case" = c("sg_level", "g_level", "sub_level"),
#                                      "lpr_ex" = "immsup"),
#                      "exclusion" = c("DQ","ZZ2")))
#
#
# t <- searchR(dfs,
#         clist$searchR.list,
#         sub.list = clist$searchR.keep,
#         sub.labels = clist$recodR.labels,
#         exclusion = clist$searchR.exclusion)
#
# Simple example
# t <- searchR(dfs$lpr,
#         list("t_var" = c("DP"),
#              "d_var" = c("DQ")),
#         format = "date",
#         date.filter = "1995-12-31")
#
# yframe <- data.frame()
#
# for(i in c("1996-01-01","2006-01-01","2010-01-01")) {
#
#   yframe <- bind_rows(yframe, as.data.table(searchR(dfs$lpr,
#           list("t_var" = c("DP"),
#                "d_var" = c("DQ")),
#           format = "categorical",
#           date.filter = i))[, year := i])
#
#
# }
#
# yframe %>% arrange(pnr)




searchR <- function(reglist,
                    search.list,
                    name.list = NULL,
                    sub.list = list(),
                    sub.labels = NULL,
                    exclusion = "NULL",
                    slice = "first",
                    format = "date",
                    date.filter = NULL,
                    match = "start",
                    casename = "index",
                    pnr = "pnr",
                    cores = 4) {

  tickR()

  start <- tickR.start

  cat(paste0("\nInitializing searchR algorithm: ", tockR("time"), "\n\n"))

  match <- match.arg(match, c("start", "end", "exact", "contains"))
  format <- match.arg(format, c("categorical", "date", "code"))
  slice <- match.arg(slice, c("first", "last", "all"))



  if(class(reglist) %in% "data.frame") {

    reglist <- lst(" " = reglist)
    names(search.list) <- " "
  } else {

    reglist <- reglist[names(search.list)]


  }

  pnr_c <- reglist[[1]] %>% select({{pnr}}) %>% names



  reglist <- lapply(reglist, function(d) {
    colnames(d)[which(str_detect(colnames(d), "eksd|inddto"))] <- "date"
    colnames(d)[which(str_detect(colnames(d), "diag|opr|atc|snomed"))] <- "code"
    d
  })


    cl <- makeCluster(cores)
    doSNOW::registerDoSNOW(cl)

    #Loop through reglist
  joined_data <-
    Reduce(function(x,y)
    merge(x, y, all = TRUE),
    foreach(x = seq_along(reglist)) %do% {

    cat(paste0("\n\nLoading ", str_to_upper(names(reglist)[x]), ": \n\n"))

    varlist <- search.list[[names(reglist)[[x]]]]
    reg <- names(reglist)[x]

    if(names(reglist)[[x]] == "pato") {
      regex <- c("\\b(", ")")
    } else {

      switch(match,
             "start" = {regex <- c("^(", ")")},
             "end" = {regex <- c("(", ")$")},
             "exact" = {regex <- c("^(", ")$")},
             "contains" = {regex <- c("(", ")")}
      )
    }

    pb <- txtProgressBar(max = length(varlist), style = 3, width = getOption("width") - 6)
    progress <- function(n) {

      setTxtProgressBar(pb, n)
      cat(paste0(" Completed: ",names(varlist)[n], " - Time: ", tockR("time"), " - runtime: ", tockR("diff"), "   "))

    }
    opts <- list(progress = progress)

    #Loop through variables
    Reduce(function(x,y) #Running full-join merge
      merge(x, y, by = pnr_c, all = TRUE),
     foreach(k = seq_along(varlist),
              .packages = c("tidyverse", "data.table"),
             .options.snow = opts) %dopar% {

      i <- names(varlist)[k]



      pattern <- paste0(regex[1], paste0(varlist[[i]], collapse="|"), regex[2])

      exclude <- paste0(regex[1], paste0(exclusion, collapse="|"), regex[2])

      data <- as.data.table(reglist[[reg]])[str_detect(code, pattern) & str_detect(code, exclude, negate=T)]

      if(!is.null(date.filter)) {

        data <- data[date <= as.Date(date.filter), ]

      }

      if(format == "categorical") {
        data <- data[, c(i) := 1]
      } else if(format == "code") {
        data <- data[, c(i) := code]
      } else {
        data <- data[, c(i) := date]
      }

      if(i %in% names(sub.list)) {

        for(j in sub.list[[i]]) {

          data <- data[, c(j) := code]
        }
      }

      switch(slice,
             "first" = {range <- 1},
             "last" = {range <- ".N"},
             "all" = {range <- "1:.N"})

      data <- data[, .SD[eval(parse(text=range))], by=c(pnr_c), .SDcols = c(i, sub.list[[i]])]

      setkeyv(data, pnr_c)
      data


    })

  })[order(get(pnr_c))]

  stopCluster(cl)

  if(!is.null(name.list)) {
  setnames(joined_data,
           unlist(name.list),
           names(name.list))

  }

  if(!is.null(sub.labels)) {

    joined_data <- recodR(joined_data, sub.labels)

  }

  cat("\nSearching complete!\n")
  cat("Total runtime: \n")
  cat(tockR("diff", start), "\n\n")

  joined_data


}

