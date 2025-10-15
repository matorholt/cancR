#' Perform multiple estimatR analyses
#'
#'
#' @param data list of dataframe(s)
#' @param timevar Character vector of time variables. If missing "t_" is assumed to be prefix for all names in the "events" vector
#' @param event Character vector of event variables
#' @param group Character vector of grouping variables
#' @param names Element names of the returned list of models. If missing the "events" names are used.
#' @param ... See arguments in estimatR(). Multiple arguments should be inputted as lists (e.g. time = list(60,60,120))

#'
#' @return A named list of models with the estimatR function
#' @export
#'
#'
# n <- 2000
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
# t <-
#   estimatR.multi(list(df2),
#                  timevar = "ttt",
#                  event = c("event", "event", "event2"),
#                  group = "X3",
#                  survscale = "OS",
#                  time = list(60,60,120),
#                  names = c("m1", "m2", "m3"),
#                  type = "select",
#                  vars = c("X6", "X7"))
#
# vars <- c("X1", "X2", "X3")
#
# var_list <- list()
#
# for(i in seq_along(vars)) {
#
#   var_list[[i]] <- vars[vars != vars[i]]
#
# }
#
# var_list
#
#
# test <- estimatR.multi(
#   list(df2),
#   timevar = "ttt",
#   event = "event",
#   group = vars,
#   type = "select",
#   vars = var_list
# )

estimatR.multi <- function(data,
                           timevar,
                           event,
                           group,
                           names,
                           ...) {

  cat("\nestimatR.multi initialized: ", tickR(), "\n")

  start <- tickR.start

  if(class(data) != "list") return(cat("ERROR: Please input the dataframe(s) as a list"))

  if(missing(names)) {
    names <- paste(event, group)
  }

  if(missing(timevar)) {
    timevar <- paste("t_", event, sep="")
  }

  arg.list <- append(as.list(environment()), list(...))

  arg.list[c("names")] <- NULL
  arg.list[c("start")] <- NULL

if("vars" %in% names(arg.list)) {

    if(class(arg.list[["vars"]]) != "list") {

  arg.list$vars <- list(arg.list$vars)
    }
}

  out <- pmap(arg.list,
       function(...) {

         estimatR(...)

       })  %>% set_names(names)

  cat(paste0("\nTotal runtime: \n"))
  cat(tockR("diff", start))

  return(out)





}

