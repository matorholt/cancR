#' Data reduction, updating and insertion of time-dependent covariates
#'
#' @param td.frame data frame of time-dependent covariates with dates in from/to format
#' @param update.frame data.frame of index dates of covariates that should be updated/inserted
#' @param vars vector of variables to update/insert
#' @param indices vector of index dates for data reduction
#' @param pnrs vector of pnrs for data reduction
#' @param dt whether the dataframe should be returned as a data.table
#'
#' @return A reduced and updated time-dependent data frame
#' @export
#'

# n=2000
# c=10
#
#
# pop <- simulatR("match",
#                n=n,
#                match.cases = c) %>%
#   mutate(byear = round(runif(n, 1955,1965),0),
#          ethnic = sample(c("euro", "africa", "asia"), n, replace=TRUE))
#
# covariates_df
# set.seed(1)
# update.frame <- data.frame(pnr = seq(0,100),
#                            connective = sample(c(as.Date("2000-01-01") + sample(0:365*10, n, replace = TRUE), as.Date(NA)), 101, replace=TRUE),
#                            new = sample(c(as.Date("2000-01-01") + sample(0:365*10, n, replace = TRUE), as.Date(NA)), 101, replace=TRUE))
#
#
#
#
#
# split(unique(covariates_df$pnr), cut(seq_along(unique(covariates_df$pnr)), 10, labels = FALSE))
#
# tdf <- updatR(covariates_df,
#               update.frame,
#               vars = c(connective, new),
#               indices = pop[pop$case == 1,"index"],
#               pnrs = pop$pnr)



updatR <- function(td.frame,
                   update.frame,
                   vars,
                   indices = NULL,
                   pnrs = NULL,
                   dt = F) {

  tickR()

  start <- tickR.start

  cli::cli_h2("Initializing updatR algorithm: {tockR(\'time\')}")

  setDT(td.frame)

  if(!is.null(pnrs)) {
    tickR()
    cli::cli_h3("Filtering PNRs")

    td.frame <- td.frame[pnr %in% pnrs]

    cli::cli_alert_success("Completed - {tockR(\'time\')}, runtime: {tockR()}")

  }

  if(!is.null(indices)) {

    tickR()
    cli::cli_h3("Index reduction")

    all_indices <- sort(indices)

    td.frame <- td.frame[findInterval(from, all_indices) < findInterval(to, all_indices)]

    cli::cli_alert_success("Completed - {tockR(\'time\')}, runtime: {tockR()}")

  }

  cli::cli_h3("Updating and Inserting")
  tickR()

  if(!missing(vars)) {
    vars <- update.frame %>% select({{vars}}) %>% names

    setDT(update.frame)

    progressr::handlers(global = TRUE)
    progressr::handlers("cli")
    options(cli.progress_bar_style = "fillsquares")

    p <- progressr::progressor(along = seq_along(vars))

    for(v in vars) {

      if(v %in% names(td.frame)) {

        td.frame <- joinR(td.frame, update.frame[, .SD, .SDcols = c("pnr", v)], by = "pnr", dt=T)

        #Split into:
        #1) Everything before index is pmax(0 and current status)
        #2) Everything after index is 1
        #3a) If between from/to, to = index-1 and pmax(0 and current status)
        #3b) If between from/to, from = index and 1
        #4) Keep NAs and let .x be .x for update and 0 for insert

       td.frame <-
         rbindlist(list(
           td.frame[to <= get(paste0(v, ".y"))][, substitute(paste0(v, ".x")) := pmax(0, get(paste0(v, ".x")))],
           td.frame[from > get(paste0(v, ".y"))][, substitute(paste0(v, ".x")) := 1],
           td.frame[from <= get(paste0(v, ".y")) & get(paste0(v, ".y")) < to][, c("to", paste0(v, ".x")) := list(get(paste0(v, ".y")) - 1, pmax(0, get(paste0(v, ".x"))))],
           td.frame[from <= get(paste0(v, ".y")) & get(paste0(v, ".y")) < to][, c("from", paste0(v, ".x")) := list(get(paste0(v, ".y")), 1)],
           td.frame[is.na(get(paste0(v, ".y")))]))[order(pnr, from)][,c(paste0(v, ".y")) := NULL]

       setnames(td.frame, paste0(v, ".x"), v)


        } else {


          td.frame <- joinR(td.frame, update.frame[, .SD, .SDcols = c("pnr", v)], by = "pnr", dt=T)

          td.frame <-
            rbindlist(list(
            td.frame[to <= get(v)][, c(paste0(v, ".x")) := 0],
            td.frame[from > get(v)][, c(paste0(v, ".x")) := 1],
            td.frame[from <= get(v) & get(v) < to][, c("to", c(paste0(v, ".x"))) := list(get(v) - 1, 0)],
            td.frame[from <= get(v) & get(v) < to][, c("from", c(paste0(v, ".x"))) := list(get(v), 1)],
            td.frame[is.na(get(v))][,c(paste0(v, ".x")) := 0]), fill = TRUE)[order(pnr, from)][,c(v) := NULL]

      setnames(td.frame, paste0(v, ".x"), v)

      }
      p(paste0("Updating: ", v, " complete: ", tockR("time"), " - Runtime: ", tockR()))


    }
  }
  cli::cli_alert_success("Completed - {tockR(\'time\')}, runtime: {tockR()}")

  cli::cli_h3("Updating complete!")
  cli::cli_text("Total runtime:")
  cli::cli_text(tockR("diff", start))

  if(dt) return(td.frame) else return(as.data.frame(td.frame))


}

# tdf <-
#   updatR(covariates_df,
#               update.frame,
#               vars = c(connective, new),
#               indices = pop[pop$case == 1,"index"],
#               pnrs = pop$pnr)
