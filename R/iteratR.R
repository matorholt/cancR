#' Perform multiple estimatR analyses
#'
#'
#' @param data data frame, list of data frames or object of class "iteratR"
#' @param names vector of names for the returned list of models. If missing the "events" names or names in the iteratR object are used.
#' @param method which cancR function to use. Choose between "estimatR", "incidencR", "clustR" and "inferencR". The previous can be passed to "extractR" and "plotR"
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
# inc <-
#   iteratR(analysis_df,
#           timevar = "ttt",
#           event = c("event", "event", "event2"),
#           group = "X3",
#           time = 60,
#           names = c("m1", "m2", "m3"),
#           method = "incidencR")
#
# est <-
#   iteratR(analysis_df,
#           timevar = "ttt",
#           event = c("event", "event", "event2"),
#           group = "X3",
#           survscale = c("OS", "OS", "AM"),
#           time = list(60,60,120),
#           names = c("m1", "m2", "m3"),
#           type = "select",
#           vars = c("X6", "X7"),
#           method = "estimatR")
# #plotR
# iteratR(est, method = "plotR",
#         title = names(est),
#         y = c(100,100,90))
# #extractR
# iteratR(est, method = "extractR", format = "wide")
#
# iteratR(inc, method = "extractR")


iteratR <- function(data,
                    names,
                    method,
                    ...) {

  cat("\niteratR initialized: ", tickR(), "\n")

  method_choices <- c("extractR", "plotR", "estimatR", "incidencR", "inferencR", "clustR")

  if(method %nin% method_choices) {
    return(cat(paste0("Error: Invalid choice of method. Choose between:\n", paste0(method_choices, collapse="\n"))))
  }

  start <- tickR.start

    if(method == "extractR") {

      names <- NULL

      out <- bind_rows(lapply(seq_along(data), function(x) {

        extractR(data[[x]], ...) %>%
        mutate(model = names(data)[[x]])

      }))

  } else if(method == "plotR") {

    arg.list <- append(list(list = data), list(...))

    out <- pmap(arg.list,
                function(...) {

                  plotR(...)

                })


  } else {

  if(missing(names)) {
    names <- paste(event, group)
  }

  arg.list <- append(as.list(environment()), list(...))

  arg.list[c("names")] <- NULL
  arg.list[c("start")] <- NULL
  arg.list[c("method")] <- NULL

  if(class(data) != "list") {
  arg.list[["data"]] <- list(data)
  }

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

  class(out) <- "iteratR"

  }


  cat(paste0("\nTotal runtime: \n"))
  cat(tockR("diff", start))

  return(out)

}

