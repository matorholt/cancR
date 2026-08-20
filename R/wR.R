#' Win ratio/Win difference analysis
#'
#' @description
#' Win ratio/Win difference analysis as described by Pocock et al. with the Finkelstein-Schoenfeld test.
#'
#' @format A data frame with four columns:
#' \describe{
#'   \item{id}{ID column with multiple rows per subject}
#'   \item{event}{Event type; \code{0} = censoring}
#'   \item{event_time}{Time to event}
#'   \item{allocation}{Treatment arm; \code{"trt"} = treatment, \code{"ctrl"} = control}
#' }
#' The last row per \code{id} must include either a terminal event or censoring,
#' with the corresponding \code{event_time} representing the maximum follow-up date.
#'
#' @param data Dataset; see \code{format}.
#' @param hierarchy Named list of outcomes with corresponding event numbers
#'   (e.g. \code{list("death" = 1, "recurrence" = 2)}). The order determines
#'   the hierarchy, with the first element being the most important outcome.
#' @param max.time Maximum follow-up time; \code{event_time} values beyond this
#'   will be truncated.
#' @param digits Number of digits used for rounding shared follow-up time,
#'   allowing for slightly faster computation.
#' @param alpha Alpha level (default: \code{0.05}).
#' @param verbose Logical; whether objects should be printed for debugging
#'   (default: \code{FALSE}).
#'
#' @returns A list containing the following elements:
#' \describe{
#'   \item{win_counts}{Wins, losses, ties and proportions — overall and per component}
#'   \item{win_ratio}{Win ratio with 95\% CI, SE, Z-statistic and p-value — overall and per component}
#'   \item{win_difference}{Win difference with 95\% CI, SE, Z-statistic and p-value — overall and per component}
#' }
#' @export
#'

# sim_dat <-
#   tribble(
#     ~ id, ~event, ~ event_time, ~allocation,
#     1, 4, 10, "trt",
#     1, 3, 20,"trt",
#     1, 2, 30,"trt",
#     1, 1, 40,"trt",
#     2, 2, 20,"ctrl",
#     2, 0, 30,"ctrl",
#     3, 0, 10,"trt",
#     4, 0, 50,"ctrl",
#     5, 2, 20, "ctrl",
#     5, 2, 30, "ctrl",
#     5, 0, 40, "ctrl",
#     6, 2, 20, "trt",
#     6, 0, 60, "trt",
#     7, 1, 5, "ctrl",
#     8, 3, 5, "trt",
#     9, 4, 10, "ctrl",
#     10, 4, 5, "trt",
#     11, 3, 5, "ctrl") %>%
#   mutate(event_time = pmax(0, event_time + rnorm(n(), 0, 0.05)))

# wR(sim_dat,
#    hierarchy = list("dsd" = 1,
#                     "distant" = 2,
#                     "nodal" = 3,
#                     "local" = 4),
#    verbose = F)

wR <- function(data,
               hierarchy,
               max.time = 60,
               digits = 4,
               alpha = 0.05,
               verbose = T) {

  verbosR <- function(obj) {

    if(verbose) {
      obj_c <- defusR(obj)

      cli::cli_h1(obj_c)
      print(obj)
    }

  }

  dat <- as.data.table(data)

  setorder(dat, id, event_time)

  verbosR(dat)

  trt_ids <- unique(dat$id[dat$allocation == "trt"])
  ctrl_ids <- unique(dat$id[dat$allocation == "ctrl"])
  all_ids <- c(trt_ids, ctrl_ids)
  n_trt <- length(trt_ids)
  n_ctrl <- length(ctrl_ids)
  n_all <- n_trt + n_ctrl


  verbosR(trt_ids)

  follow_dt <- dat[, .(max_time = max(event_time)), by = id]

  verbosR(follow_dt)

  #Pairs
  grid <- CJ(idx = all_ids, idy = all_ids) %>%
    joinR(., follow_dt, by = list(c("idx", "id"))) %>%
    joinR(., follow_dt, by = list(c("idy", "id"))) %>%
    .[idx != idy,] %>%
    .[, shared := round(pmin(pmin(max_time.x, max_time.y), max.time),digits)] %>%
    .[, c("max_time.x", "max_time.y") := NULL]

  verbosR(grid)

  s_times <-
    unique(
      melt(grid,     measure.vars = c("idx", "idy"),
           value.name = "id")[, .(id, shared)]) %>% setorderv(., c("id", "shared"))

  verbosR(s_times)



  dat_t <- joinR(s_times, dat, by = "id")[event %in% unlist(hierarchy)]

  verbosR(dat_t)

  setorderv(dat_t, c("id", "shared"))

  event_list <-
    imap(hierarchy, ~ {

      df <- copy(dat_t)

      df[event_time <= shared,] %>%
        .[, (.y) := fifelse(event == .x, 1, 0)] %>%
        .[, (.y) := cumsum(get(.y)), by = .(id, shared)] %>%
        .[, paste0("t_", .y) := min(event_time), by = .(id, event)] %>%
        .[, c("id", "shared", .y, paste0("t_", .y)), with = FALSE] %>%
        .[.[, .I[.N], by = .(id, shared)]$V1] %>%
        #Keep only events
        .[get(.y) > 0]

    })



  check.empty <- sapply(event_list, nrow)

  if(0 %in% check.empty) {
    idx <- which(check.empty == 0)
    event_list <- event_list[-idx]

    cli::cli_alert_danger("Warning: Component(s) {names(hierarchy)[idx]} removed due to no events in shared follow up")
    hierarchy <- hierarchy[-idx]
  }

  verbosR(event_list)

  event_frame <- joinR(s_times, event_list, by = c("id", "shared"))

  verbosR(event_frame)


  all_grid <-
    joinR(grid, event_frame, by = list(c("idx", "id"),
                                       c("shared", "shared"))) %>%
    joinR(., event_frame, by = list(c("idy", "id"),
                                    c("shared", "shared"))) %>%
    setcolorder(c("idx", "idy")) %>%
    rowR(vars = names(.)[-c(1:3)], type = "all.na", label = all.tie)

  tie_grid <- all_grid[all.tie == 1]

  verbosR(all_grid)

  event_grid <- all_grid[all.tie == 0] %>%
    .[, overall := NA_integer_] %>%
    .[, all.tie := NULL]

  walk(names(hierarchy), ~ {

    x <- paste0(.x, ".x")
    y <- paste0(.x, ".y")
    tx <- paste0("t_",.x, ".x")
    ty <- paste0("t_",.x, ".y")

    #1 = Win for idx, -1 = Loss for idx
    event_grid[, c(.x) := fcase(
      (is.na(get(x)) & is.na(get(y))) | !is.na(overall), NA_real_,

      #If difference is != 0, -1 or 1
      (fcoalesce(as.double(get(y)), 0) - fcoalesce(as.double(get(x)), 0)) != 0,
      as.double(sign(fcoalesce(as.double(get(y)), 0) - fcoalesce(as.double(get(x)), 0))),

      #If difference is 0, use time and return -1 og 1
      (fcoalesce(as.double(get(ty)), 0) - fcoalesce(as.double(get(tx)), 0)) != 0,
      as.double(sign(fcoalesce(as.double(get(ty)), 0) - fcoalesce(as.double(get(tx)), 0))),

      #Otherwise NA
      default = NA_real_
    )][, overall := fcoalesce(as.double(overall), get(.x))] #%>%
    #.[, c(x, y, tx, ty) := NULL]

  })

  verbosR(event_grid)

  tc_grid <- CJ(idx = trt_ids, idy = ctrl_ids)

  win_grid <- event_grid[tc_grid, on = .(idx, idy), nomatch = 0]
  tie_grid_tc <- tie_grid[tc_grid, on = .(idx, idy), nomatch = 0]

  verbosR(win_grid)

  win_counts <-
    map(c(names(hierarchy), "overall"), ~ {

      x <- win_grid[[.x]]

      list(component = .x,
           wins = sum(x > 0, na.rm=T),
           losses = sum(x < 0, na.rm = T))

    }) %>% rbindlist %>%
    .[, `:=`(wl = wins + losses, total = sum(nrow(tie_grid_tc), nrow(win_grid)))] %>%
    .[, ties := ifelse(component != "overall", total - cumsum(wl), total - wl)] %>%
    .[, total := ifelse(component != "overall", wl + ties, total)] %>%
    .[, .(component, wins, losses, ties, total)] %>%
    .[, `:=`(
      p_win  = wins  / total,
      p_loss = losses / total,
      p_ties = ties  / total
    )]

  fs_test <- function(component) {

    #Sum of wins/losses as idx and as idy (-sum) as x is reference
    U <- rbind(event_grid[, .(score = sum(get(component), na.rm = TRUE)), by = .(i = idx)],
               event_grid[, .(score = -sum(get(component), na.rm = TRUE)), by = .(i = idy)]) %>%
      .[, .(U = sum(score)), by = i] %>%
      .[, trt := fifelse(i %in% trt_ids, 1, 0)] %>%
      setorder(i)

    T_score <- U[trt == 1, sum(U)]

    var <- (n_ctrl * n_trt) / ((n_ctrl+n_trt) * ((n_ctrl+n_trt) - 1)) * U[, sum(U^2)]

    Z <- T_score / sqrt(var)

    p_value <- 2 * pnorm(-abs(Z))

    lst(component, Z, p_value)

  }

  #Add fs values
  win_counts <- win_counts[map(c(names(hierarchy), "overall"), fs_test) %>%
                             rbindlist(), on = "component"]

  verbosR(win_counts)

  win_ratio <- win_counts[, {
    log_wr <- log(wins / losses)
    SE <- fifelse(Z == 0, NA_real_, abs(log_wr / Z))

    .(WR    = exp(log_wr),
      lower = fifelse(is.na(SE), NA_real_, exp(log_wr + qnorm(alpha/2) * SE)),
      upper = fifelse(is.na(SE), NA_real_, exp(log_wr - qnorm(alpha/2) * SE)),
      SE = SE,
      Z     = Z,
      p_exact = p_value)
  }, by = component][, p_value := pvertR(p_exact)]

  verbosR(win_ratio)


  win_diff <- win_counts[, {

    wd     <- ((wins - losses) / (n_trt * n_ctrl))
    SE  <- fifelse(Z == 0, NA_real_, abs(wd / Z))

    .(win_diff = wd,
      lower    = fifelse(is.na(SE), NA_real_, exp(wd + qnorm(alpha/2) * SE)),
      upper    = fifelse(is.na(SE), NA_real_, exp(wd - qnorm(alpha/2) * SE)),
      SE = SE,
      Z        = Z,
      p_exact  = p_value)
  }, by = component][, p_value := pvertR(p_exact)]

  verbosR(win_diff)

  return(list(counts = win_counts,
              ratio = win_ratio,
              diff = win_diff))

}
