#' estimatR.multi
#'
#' @description
#' Convenience function for performing multiple estimatR functions (e.g. multiple outcomes or risk factor). Each argument is recycled to avoid repeated names.
#'
#'
#' @param data dataset
#' @param timevars Character vector of time variables. If missing "t_" is assumed to be prefix for all names in the "events" vector
#' @param events Character vector of event variables
#' @param groups Character vector of grouping variables
#' @param names Element names of the returned list of models. If missing the "events" names are used.
#' @param survtime Whether median time to event should be calculated (default = TRUE)
#' @param survscale Whether overall survial should be estimated as survival or all-cause mortality (1-survival)
#' @param type Model specification, Can be univariate ("uni"), Age- and sex standardized ("age-sex"), Multivariate with variable selection ("select"). In this case vars should be a vector of the covariates. "custom" allows for free modelling where the form-argument contains the formula.
#' @param vars Only applicable when "select" is chosen as type. The variables are added to the model as an additive model
#' @param form Only applicable when "custom" is chosen as type. Free specification of the model as the right-hand side of the formula.
#' @param time Time horizon of interest. Defaults to 60 (e.g. 5-years)
#' @param breaks Interim time points of interest. Defaults to 12 months (1-year gaps)
#' @param cores Number of cores for parallel processing
#'
#' @return A named list of models with the estimatR function
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
# t2 <- estimatR.multi(df2,
#              timevars = "ttt",
#              events = c("event", "event", "event2"),
#              groups = "X1",
#              names = c("m1", "m2", "m3"))

estimatR.multi <- function(data,
                           timevar,
                           event,
                           group,
                           names,
                           survscale = "AM",
                           type = "uni",
                           vars = NULL,
                           form = NULL,
                           time = 120,
                           breaks = 12,
                           survtime = T,
                           proportions = F,
                           conditional = F,
                           cores = pmin(detectCores(), 4)) {


  if(missing(names)) {
    names <- paste(event, group)
  }

  if(missing(timevar)) {
    timevar <- paste("t_", event, sep="")
  }

  if(!is.null(vars)) {
  vars <- list(vars)
  }

  arg.list <- as.list(environment())
  arg.list[c("data", "names")] <- NULL
  arg.list <- discard(arg.list, is.null)

  pmap(arg.list,
       function(...) {

         estimatR(data=data, ...)
       })  %>% set_names(names)

}

