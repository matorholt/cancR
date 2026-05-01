#' Find covariates or outcomes from the registers
#'
#'
#' @param reglist list of dataframe(s)
#' @param search.list list with list(label = diagnosis code) structure.
#' @param sub.list list of variables where the diagnosis code should be kept (e.g. list("KOL" = "subtype"))
#' @param sub.labels list of labels for the diagnosis code colum (e.g. list("subtype" = list("a" = c("DC1", "DC2"))))
#' @param name.list list of labels for the main columns (e.g. list("case" = "diabetes))
#' @param exclusion.list list with same structure as search.list for exclusion codes
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
#                 pato = simulatR("pato", n=n),
#                 sc = data.frame(pnr = sample(seq(1,n*10), n*10*0.1, replace=F),
#                                 sc_date = sample(c(sample(seq(as.Date('1990/01/01'), as.Date('2020/01/01'), by="day"))), n*10*0.1, replace=TRUE),
#                                 meta_date = sample(c(sample(seq(as.Date('1990/01/01'), as.Date('2020/01/01'), by="day"))), n*10*0.1, replace=TRUE),
#                                 pato_supp = sample(c(sample(seq(as.Date('1990/01/01'), as.Date('2020/01/01'), by="day"))), n*10*0.1, replace=TRUE)))


#Simple example
# searchR(reglist, list(lpr = list(test = c("DF", "DB")),
#                       opr = list(test2 = c("KF", "KB"))))

searchR <- function(reglist,
                    search.list,
                    name.list = NULL,
                    sub.list = list(),
                    sub.labels = NULL,
                    exclusion.list = list(),
                    slice = "first",
                    format = "date",
                    date.filter = NULL,
                    match = "start",
                    casename = "index",
                    pnr = "pnr",
                    cores = 4,
                    dt = F) {

  tickR()

  start <- tickR.start

  cli::cli_h2("Initializing searchR algorithm: {tockR(\'time\')}")

  match <- match.arg(match, c("start", "end", "exact", "contains"))
  format <- match.arg(format, c("categorical", "date", "code"))
  slice <- match.arg(slice, c("first", "last", "all"))


  if(class(reglist) %in% "data.frame") {

    reglist <- lst(" " = reglist)
    names(search.list) <- " "
  } else {

    if(pluck_depth(search.list) != 3) {
      return(cli::cli_alert_danger("Error: search.list needs to have a depth of 3. Maybe the list is unnamed"))
    }

    if(any(names(search.list) %nin% names(reglist))) {

      return(cli::cli_alert_danger("Error: {names(search.list)[names(search.list) %nin% names(reglist)]} not present in reglist"))

    }


    reglist <- reglist[names(search.list)]

  }

  pnr_c <- reglist[[1]] %>% select({{pnr}}) %>% names

  reg.labels <- list(lpr = c("inddto", "diag"),
                     opr = c("odto", "opr"),
                     lmdb = c("eksd", "atc"),
                     immune = c("date", "atc"),
                     pato = c("date", "snomed"))


 if(!is.null(cores)) multitaskR(cores = cores)

  progressr::handlers(global = TRUE)
  progressr::handlers("cli")
  options(cli.progress_bar_style = "fillsquares")

  #Loop through reglist
  joined_data <-
    joinR(
      map(names(reglist), function(x) {

        tickR()

        cli::cli_h3("Loading {str_to_upper(x)}")

        setDT(reglist[[x]])

        setnames(reglist[[x]], reg.labels[[x]], c("date", "code"))

        varlist <- search.list[[x]]
        exlist <- exclusion.list[[x]]

        if(x == "pato") {
          regex <- c("\\b(", ")")
        } else {

          switch(match,
                 "start" = {regex <- c("^(", ")")},
                 "end" = {regex <- c("(", ")$")},
                 "exact" = {regex <- c("^(", ")$")},
                 "contains" = {regex <- c("(", ")")}
          )
        }

       p <- progressr::progressor(along = varlist)

        #Loop through variables
        out <-
          joinR(
          future_map(names(varlist), function(i) {

                    tickR()

                    pattern <- paste0(regex[1], paste0(varlist[[i]], collapse="|"), regex[2])

                    if(i %in% names(exlist)) {
                    exclude <- paste0(regex[1], paste0(exlist[[i]], collapse="|"), regex[2])
                    } else {
                      exclude <- "^$"
                    }

                    data <- reglist[[x]][code %like% pattern & !code %like% exclude]

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

                    p(paste0(i, " complete: ", tockR("time"), " - Runtime: ", tockR()))

                    data


                  }), by = pnr_c, type = "full", dt=T)


        cli::cli_alert_success("Completed - {tockR(\'time\')}, runtime: {tockR()}")

        out

      }), by = pnr_c, type = "full", dt=T)[order(get(pnr_c))]


  if(!is.null(sub.labels)) {

    joined_data <- recodR(joined_data, sub.labels, match = "start")

  }

  if(!is.null(name.list)) {
    setnames(joined_data,
             unlist(name.list),
             names(name.list))

  }

  cli::cli_h3("Search complete!")
  cli::cli_text("Total runtime:")
  cli::cli_text(tockR("diff", start))

  if(!dt) return(as.data.frame(joined_data))
  joined_data

}
