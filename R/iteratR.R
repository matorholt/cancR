#' Perform multiple estimatR analyses
#'
#'
#' @param data list of dataframe(s)
#' @param names vector of names for the returned list of models. If missing the "events" names are used.
#' @param method which cancR function to use. Choose between "estimatR", "incidencR", "clustR" and "inferencR".
#' @param ... See arguments in the specific function documentations. Multiple arguments should be inputted as lists (e.g. time = list(60,60,120))

#'
#' @return A named list of models of the type specified in "method"
#' @export
#'
#'

# inf <- iteratR(analysis_df,
#                timevar = "ttt",
#                event = c("event", "event", "event2"),
#                treatment = "X2",
#                vars = c("X1", "X3", "X6", "X7"),
#                outcome.vars = "X10",
#                names = c("m1", "m2", "m3"),
#                method = "inferencR")
#
# plotR(inf$m1)
#
# ii <- inferencR(analysis_df,
#                 treatment = X2,
#                 timevar = ttt,
#                 event = event2,
#                 vars = c(X1, X3, X6, X7),
#                 outcome.vars = X10)
#
# plotR(t1)
#
#
# clu <-
#   iteratR(analysis_df,
#           timevar = "ttt",
#           event = c("event", "event", "event2"),
#           treatment = "X1",
#           cluster = "id",
#           vars = c("X3", "X6_bin", "X7_bin"),
#           outcome.vars = "X8_bin",
#           censoring.vars = "X8_bin",
#           names = c("m1", "m2", "m3"),
#           event.digits=0,
#           method = "clustR")
#
# plotR(clu$m1)
#
#
# inc <-
#   iteratR(analysis_df,
#           timevar = "ttt",
#           event = c("event", "event", "event2"),
#           group = "X3",
#           time = 60,
#           names = c("m1", "m2", "m3"),
#           method = "incidencR")
# plotR(inc$m1)
#
# est <-
#   iteratR(analysis_df,
#           timevar = "ttt",
#           event = c("event", "event", "event2"),
#           group = "X3",
#           survscale = "OS",
#           time = list(60,60,120),
#           names = c("m1", "m2", "m3"),
#           type = "select",
#           vars = c("X6", "X7"),
#           method = "estimatR")
#
# plotR(est$m1)


iteratR <- function(data,
                    names,
                    method,
                    ...) {

  cat("\niteratR initialized: ", tickR(), "\n")

  start <- tickR.start

  #if(class(data) != "list") return(cat("ERROR: Please input the dataframe(s) as a list"))

  if(missing(names)) {
    names <- paste(event, group)
  }

  arg.list <- append(as.list(environment()), list(...))

  arg.list[c("names")] <- NULL
  arg.list[c("start")] <- NULL
  arg.list[c("method")] <- NULL
  arg.list[["data"]] <- list(data)

if("vars" %in% names(arg.list)) {

    if(class(arg.list[["vars"]]) != "list") {

  arg.list$vars <- list(arg.list$vars)
    }
}

  if("timevar" %nin% names(arg.list)) {

      arg.list[["timevar"]] <- paste("t_", arg.list[["event"]], sep="")
    }


  out <- pmap(arg.list,
       function(...) {

         if(method == "estimatR") {
           estimatR(...)
         } else if(method == "incidencR") {
           incidencR(...)
         } else if(method == "clustR") {
           clustR(...)
         } else if(method == "inferencR") {
           inferencR(...)
         }

       })  %>% set_names(names)

  cat(paste0("\nTotal runtime: \n"))
  cat(tockR("diff", start))

  return(out)

}




