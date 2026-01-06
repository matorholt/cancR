#' Perform multiple estimatR analyses
#'
#'
#' @param data data frame, list of data frames or object of class "iteratR"
#' @param labels vector of names for the returned list of models. If missing the "events" names or names in the iteratR object are used.
#' @param method which cancR function to use. Choose between "estimatR", "incidencR", "clustR" and "inferencR". The previous can be passed to "extractR" and "plotR"
#' @param multivariable multivariable analysis for estimatR
#' @param ... See arguments in the specific function documentations. Multiple arguments should be inputted as lists (e.g. time = list(60,60,120))

#'
#' @return A named list of models of the type specified in "method"
#' @export
#'
#'

# #inferencR
# inf <- iteratR(analysis_df,
#                timevar = "ttt",
#                event = c("event", "event", "event2"),
#                treatment = "X2",
#                vars = c("X1", "X3", "X6", "X7"),
#                outcome.vars = "X10",
#                names = c("m1", "m2", "m3"),
#                method = "inferencR")
#
# #clustR
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
# #incidencR
# inc <-
#   iteratR(
#     data=split(analysis_df, ~ X1),
#     #data=analysis_df,
#           timevar = "ttt",
#           event = c("event", "event", "event2"),
#           group = "X3",
#           time = 60,
#           names = c("m1", "m2", "m3"),
#           method = "incidencR")
#
# #estimatR
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
#
# Est as multivariable
# est <-
#   iteratR(analysis_df,
#           timevar = "ttt",
#           event = "event",
#           group = c("X1", "X2", "X3", "X4", "X5"),
#           method = "estimatR",
#           multivariable = T)
# #plotR
# iteratR(est, method = "plotR",
#         title = names(est),
#         y = c(100,100,90))
# #extractR
# iteratR(est, method = "extractR", format = "wide")
#
# iteratR(inc, method = "extractR")
#
# savR
# iteratR(plots,
#         format = c("png", "pdf"),
#         method = "savR")
#
# tablR
#
# tab <-
#   iteratR(split(analysis_df, ~ X1),
#           group = "X2",
#           vars = c("X6", "X7"),
#           method = "tablR",
#           labels = c("G1", "G2", "G3"))
#
# tab

iteratR <- function(data,
                    labels,
                    method,
                    multivariable = F,
                    ...) {

  cat("\niteratR initialized: ", tickR(), "\n")

  method_choices <- c("tablR", "extractR", "plotR", "estimatR", "incidencR", "inferencR", "clustR", "savR")

  if(method %nin% method_choices) {
    return(cat(paste0("Error: Invalid choice of method. Choose between:\n", paste0(method_choices, collapse="\n"))))
  }

  start <- tickR.start

  if(method == "tablRt") {

    arg.list <- list(...)
    if(any(class(data) %nin% "list")) {
      arg.list[["data"]] <- list(data)
    } else {
      arg.list[["data"]] <- data
    }

    #return(arg.list)

    out <- pmap(arg.list,
         function(...) {
           tablR(...)
         })

  } else if(method == "extractR") {

      labels <- NULL

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


  } else if(method == "savR") {

    labels <- NULL

    cat(paste0("Exporting: ", length(data), " objects"))

    for(p in seq_along(data)) {

      savR(data[[p]], names(data)[p], ...)

    }

    return(cat(""))

  } else {

  arg.list <- list(...)

  if(missing(labels)) {

    if("group" %nin% names(arg.list)) {

      labels <- arg.list[["event"]]


    } else {

      labels <- paste0(arg.list[["event"]], "_", arg.list[["group"]])
    }
  }



  if(any(class(data) %nin% "list")) {
  arg.list[["data"]] <- list(data)
  } else {
    arg.list[["data"]] <- data
  }

if("vars" %in% names(arg.list)) {

    if(class(arg.list[["vars"]]) != "list") {

  arg.list$vars <- list(arg.list$vars)
    }
}

  if(multivariable) {

    arg.list$type <- "select"

    multi_list <- list()

    for(i in seq_along(arg.list$group)) {

      multi_list[[i]] <- arg.list$group[arg.list$group != arg.list$group[i]]

    }

    arg.list$vars <- multi_list

  }



  if("timevar" %nin% names(arg.list) & method != "tablR") {

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
         } else if(method == "tablR") {
           tablR(...)
         }

       })  %>% set_names(labels)

  class(out) <- "iteratR"

   }


  cat(paste0("\nTotal runtime: \n"))
  cat(tockR("diff", start))

  return(out)
}
