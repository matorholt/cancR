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
#
# set.seed(1)
# sim_dat <-
#   tribble(
#     ~ id, ~event, ~ event_time, ~allocation,
#     1, 4, 10, "trt",
#     1, 3, 20,"trt",
#     1, 2, 30,"trt",
#     1, 1, 40,"trt",
#     2, 2, 20,"ctrl",
#     2, 0, 30,"ctrl",
#     3, 0, 70,"trt",
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
#     11, 3, 80, "ctrl",
#     12, 0, 90, "trt") %>%
#   mutate(event_time = pmax(0, event_time + rnorm(n(), 0, 0.05)))

wR <- function(data,
               hierarchy,
               plot = T,
               max.time = NA,
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


  follow_dt <- dat[, event_time := round(event_time, digits)] %>%
    .[, .(max_time = max(event_time)), by = id]

  verbosR(follow_dt)

  #Pairs
  grid <- CJ(idy = all_ids, idx = all_ids) %>%
    .[idx < idy,] %>%
    joinR(., follow_dt, by = list(c("idy", "id"))) %>%
    joinR(., follow_dt, by = list(c("idx", "id")))%>%
    .[, shared := pmin(pmin(max_time.x, max_time.y), max.time, na.rm=T)] %>%
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

  event_grid <- merge(grid, event_frame, by.x = c("idx", "shared"), by.y = c("id", "shared")) %>%
    merge(., event_frame, by.x = c("idy", "shared"), by.y = c("id", "shared")) %>%
    setcolorder(c("idx", "idy")) %>%
    rowR(vars = names(.)[-c(1:3)], type = "all.na", label = all.tie) %>%
    setkeyv(., c("idx", "shared")) %>%
    .[, overall := NA_integer_]

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
    )][, overall := fcoalesce(as.double(overall), get(.x))] %>%
      .[, c(x, y, tx, ty) := NULL]

  })

  verbosR(event_grid)

  win_grid <- event_grid[idx %in% trt_ids & idy %in% ctrl_ids | idy %in% trt_ids & idx %in% ctrl_ids]

  verbosR(win_grid)

  win_counts <-
    map(c(names(hierarchy), "overall"), ~ {

      #Treatment as
      x <- ifelse(win_grid[["idx"]] %in% trt_ids, win_grid[[.x]], -win_grid[[.x]])

      list(component = .x,
           wins = sum(x > 0, na.rm=T),
           losses = sum(x < 0, na.rm = T))

    }) %>% rbindlist %>%
    .[, `:=`(wl = wins + losses,
             total = sum(n_trt * n_ctrl - nrow(win_grid), nrow(win_grid)))] %>%
    .[, ties := ifelse(component != "overall", total - cumsum(wl), total - wl)] %>%
    .[, total := ifelse(component != "overall", wl + ties, total)] %>%
    .[, .(component, wins, losses, ties, total)] %>%
    .[, (c("p_win", "p_loss", "p_ties")) := lapply(.SD, `/`, n_trt * n_ctrl),
      .SDcols = c("wins", "losses", "ties")]

  verbosR(win_counts)

  fs_test <- map(c(names(hierarchy),"overall"), function(h) {

    #sum of wins/losses - inverse sign if IDy
    U <-
      rbindlist(imap(c("idx", "idy"), ~ event_grid[, .(score = c(1,-1)[.y]*sum(get(h), na.rm=TRUE)), by = c(i = .x)])
      ) %>%
      .[, .(U = sum(score)), by = i] %>%
      .[, trt := fifelse(i %in% trt_ids, 1, 0)] %>%
      setorder(i)

    T_score <- U[trt == 1, sum(U)]

    var <- (n_ctrl * n_trt) / ((n_ctrl+n_trt) * ((n_ctrl+n_trt) - 1)) * U[, sum(U^2)]

    Z <- T_score / sqrt(var)

    p_value <- 2 * pnorm(-abs(Z))

    lst(component = h, Z, p_value)

  }) %>% rbindlist

  verbosR(fs_test)

  #Add fs values
  win_counts <- win_counts[fs_test, on = "component"]

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

  out <- list(counts = win_counts,
              ratio = win_ratio,
              diff = win_diff)

  if(plot) {

    rows <- length(hierarchy) + 1

    wlt <- c("Wins", "Losses", "Ties")

    labs <- map(seq_len(rows), function(i) {

      map(seq_len(3)+1, ~  {

        paste0(wlt[.x-1], ": ", as.data.frame(win_counts)[i,.x],"\n(",round(as.data.frame(win_counts)[i, .x + 4]*100,1), "%)")

      }) %>% set_names()

    }) %>% unlist

    vert_mid <- data.frame(
      x    = 2,
      xend = 2,
      y    = rows + 1.5,
      yend = 2
    )

    vert_lat <- data.frame(
      x=c(1,1),
      xend=c(1,1),
      y=c(2,3),
      yend = c(2.5,3.5)
    )

    vert_lat <-
      data.frame(x=c(rep(1, rows-1), rep(3, rows-1)),
                 xend = c(rep(1, rows-1), rep(3, rows-1)),
                 y = rep(seq(2,rows), 2),
                 yend = rep(seq(2,rows)+0.5,2))

    horiz <-
      data.frame(
        x    = 1,
        xend = 3,
        y    = seq(2.5, rows + 0.5),
        yend = seq(2.5, rows + 0.5)
      ) %>%
      rbind(c(1.5,2.5, rep(rows + 1.5,2)))

    lines_df <- rbind(vert_mid, vert_lat, horiz)

    text_size <- 5
    out[["plot"]] <- data.frame(x = c(rep(c(1,3,2), rows), 2),
               y = c(rep(rev(seq_len(rows)), each = 3), rows+1),
               labels = c(labs, paste0("Total comparisons \n (n = ", win_counts[rows, 5], ")"))) %>%

     ggplot(aes(x=x, y=y, label = labels, fill = as.factor(x))) +
      scale_fill_manual(values = c(cancR_palette[c(4, 8)], "#C75D5D")) +
      geom_segment(data=lines_df, aes(x=x, y=y, xend = xend, yend = yend), inherit.aes = FALSE, linewidth = 1) +
      geom_label(label.padding = unit(1.5, "lines"), size = text_size) +
      annotate("label", x = c(1.5,2.5), y = rows + 1.5, label = c(paste0("Intervention \n (n = ", n_trt, ")"),
                                                                  paste0("Control \n (n = ", n_ctrl, ")")),
               fill = cancR_palette[8],
               label.padding = unit(1.5, "lines"),
               size = text_size) +
      annotate("text", x = 0, y = rev(seq_len(rows)), label = str_to_title(win_counts$component), size = text_size+1, fontface = 2, hjust = "left") +
      annotate("text", x = c(3.8,4.5), y = rows + 0.5, label = c("Win Ratio", "Win Difference"), size = text_size+1, fontface = 2) +
      annotate("label",
               x = 3.8,
               y = rev(seq_len(rows)),
               label = paste0(round(win_ratio$WR,1), "\n(95%CI ", round(win_ratio$lower,1), " to ", round(win_ratio$lower,2), ")"),
               label.padding = unit(1.5, "lines"),
               size = text_size) +
      annotate("label",
               x = 4.5,
               y = rev(seq_len(rows)),
               label = paste0(round(win_diff$win_diff*100,1), "\n(95%CI ", round(win_diff$lower*100,1), " to ", round(win_diff$upper*100,1), ")"),
               label.padding = unit(1.5, "lines"),
               size = text_size) +
      theme_void() +
      theme(legend.position = "none") +
      coord_cartesian(xlim = c(0,4.7))



  }
  return(out)
}

# res <- wR(sim_dat,
#           hierarchy = list("dsd" = 1,
#                            "distant" = 2,
#                            "nodal" = 3,
#                            "Local Recurrence" = 4),
#           verbose = F,
#           plot = F)
