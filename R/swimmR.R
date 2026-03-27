#' Plot individual patient trajectories from time-to-event data (Swimmer plot)
#'
#' @param data data frame containg status and time-to-event columns
#' @param event columns containing status indicators. Needs to have corresponding "t_" prefix
#' @param strata optional stratification by a group
#' @param strata.labels named list of labels for the strata in the format list(new = "old)
#' @param strata.levels character vector of levels for the strata
#' @param horizon time horizon
#' @param breaks breaks between time zero and horizon
#' @param table.list named list for each stratum with a named list of rows and sequence of x-values
#' @param table.cols vector of the colors of the tables
#' @param cols named list with colors for the symbols
#' @param shapes named list with shapes
#' @param labs named list with labels for the events
#' @param id name of the id variable
#'
#' @returns a swimmer plot
#' @export
#'
#'

# n=50
#
# set.seed(1)
# tdf <-
#   data.frame(
#     id = seq_len(n),
#     t_death = runif(n, 50,70),
#     rec = rbinom(n, size = 2, prob = 0.5),
#     t_rec = runif(n, 0, 10),
#     lmeta = rbinom(n, size = 2, prob = 0.5),
#     t_lmeta = runif(n, 15, 30),
#     lrmeta = rbinom(n, size = 2, prob = 0.5),
#     t_lrmeta = runif(n, 15, 30),
#     dmeta = rbinom(n, size = 2, prob = 0.5),
#     t_dmeta = runif(n, 15, 30),
#     dsd = rbinom(n, size = 2, prob = 0.5),
#     t_dsd = runif(n, 40, 50),
#     risk.grp = sample(c("high", "medium", "low"), n, replace=TRUE)) %>%
#   mutate(death = ifelse(dsd != 0, 1, 0),
#          t_dsd = t_death,
#          across(c(contains("meta|dsd")), ~ ifelse(risk.grp == "low", 0, .)),
#          across(c(lrmeta, dmeta), ~ ifelse(risk.grp == "low", 0, .))) %>%
#   factR(risk.grp)
#
# swimmR(tdf,
#        event = c(rec, lmeta, lrmeta, dmeta, dsd),
#        strata = risk.grp,
#        strata.labels = list("Low" = "low",
#                             "High risk" = "high",
#                             "Medium risk" = "medium"),
#        strata.levels = c("low", "medium", "high"),
#        table.list = list("Clinical" = seq(0,60,12),
#                          "PET" = seq(0,60,10),
#                          "Local MRI" = c(0,30,60)),
#        labs = list("recurrence" = "rec",
#                    "Local Metastasis" = "lmeta",
#                    "Loco-Regional Metastasis" = "lrmeta",
#                    "Distant Metstasis" = "dmeta",
#                    "Disease-Specific Death" = "dsd",
#                    "All-cause Death" = "death"))



swimmR <- function(data,
                   event,
                   strata,
                   strata.labels = list(),
                   strata.levels = list(),
                   horizon = 60,
                   breaks = 6,
                   table.list = NULL,
                   table.cols = c("gainsboro", "grey", "#BCCFE8"),
                   cols = list(),
                   shapes = list(),
                   labs,
                   id = "id") {

  event <- data %>% select({{event}}) %>% names
  id <- data %>% select({{id}}) %>% names

  if(!missing(strata)) {

    strata <- data %>% select({{strata}}) %>% names

    data <- data %>%
      rename(grp = strata) %>%
      factR(grp, labels = strata.labels, levels = strata.levels, lab_to_lev = T)

  } else {
    data <- data %>% mutate(grp = "g")
  }

  all.events <- unique(sort(c(event, "follow", "death", "dsd")))

  f.index <- which(all.events == "follow")

  #Customization - shapes
  shapes.default <- as.list(c(23,5,NA,24,22,25,20)[1:length(all.events)]) %>% set_names(all.events)

  shapes.custom <- list(follow = NA,
                        dsd = 23,
                        death = 20,
                        rec = 21,
                        meta = 22,
                        lmeta = 25,
                        lrmeta = 24,
                        dmeta = 22)


  shapes.default <- list_assign(shapes.default, !!!shapes.custom[names(shapes.custom) %in% all.events])
  shapes <- unlist(list_assign(shapes.default, !!!shapes))

  #Customization - colors
  cols.default <- as.list(cancR_palette[1:length(all.events)]) %>% set_names(all.events)

  cols.custom <- list(follow = "grey",
                        dsd = "grey",
                        death = "grey",
                        rec = "#FFB347",
                        meta = "#252A6E",
                        lmeta = "#97AFCC",
                        lrmeta = "#252A6E",
                        dmeta = "#AB5CB8")

  cols.default <- list_assign(cols.default, !!!cols.custom[names(cols.custom) %in% all.events])

  cols <- unlist(list_assign(cols.default, !!!cols))

  #Customization - labels
  if(!missing(labs)) {
  labels <- str_replace_all(all.events[-f.index], unlist(names(labs)) %>% set_names(labs))
  } else {
    labels <- all.events
  }

  split_df <- split(data, ~ grp)

  #Tables
  height <- max(unlist(map(split_df, ~ nrow(.x))))
  n <- length(table.list)
  seqs <- -seq(5, 5+(3*n), 3)
  ymins <- seqs[1:n]
  ymaxs <- seqs[2:(n+1)]
  l.events <- length(labs)

  #Legend
  legend.min <- horizon/4
  legend.max <- horizon-horizon/4
  legend.col <- 3
  legend.row <- case_when(l.events <= 3 ~ 1,
                    l.events <= 6 ~ 2,
                    T ~ 3)


  # legend.x <- seq(legend.min+(legend.max - legend.min) / (legend.col + 1),
  #                 legend.max-(legend.max - legend.min) / (legend.col + 1),
  #                 (legend.max - legend.min) / (legend.col + 1))
  legend.x <- c(20, 30, 40)
  legend.y <- case_when(l.events <= 3 ~ 10,
                        l.events > 3 ~ c(15-3.3, 5+3.3))


  legend.df <- data.frame(labs = names(labs),
                          shapes = shapes[-f.index][match(names(labs), labels)],
                          cols = cols[-f.index][match(names(labs), labels)],
                          x = rep(legend.x, legend.row)[1:l.events],
                          y = rep(legend.y, each = legend.col)[1:l.events]+height) %>%
    mutate(symb = x - (str_count(labs))*0.2) %>%
    group_by(x) %>%
    mutate(symb = min(symb))



  plot_list <-
    map(seq_along(split_df), function(i) {

    data <- split_df[[i]]


   plot_df <-
      map(seq_along(event), ~ {

        if(.x == length(event)) value <- c(0,1,2) else value <- 1

        data %>% select(id, event[.x], paste0("t_", event[.x]), grp) %>%
         filter(!!sym(event[.x]) %in% value) %>%

         recodR(list(list("2", "1", "0") %>% set_names("death", event[.x], "follow")) %>% set_names(event[.x])) %>%
         rename(time = paste0("t_", event[.x]),
                outcome = event[.x])

      }) %>% discard(., ~ nrow(.x) == 0) %>% bind_rows %>%
      group_by(id) %>%
      # filter(
      #   outcome %in% event |
      #     (outcome == "follow" & !any(c("death", "dsd") %in% outcome))
      # ) %>%
      mutate(time = ifelse(time > horizon, time + 20, time)) %>%
      arrange(desc(time)) %>%
      mutate(end = pmax(0, lag(time), na.rm=T)) %>%
      rollR(id, label = order)

   cur.events <- sort(unique(plot_df$outcome))



    p <- ggplot(plot_df, aes(x=time, y=order, color = outcome, fill=outcome, shape = outcome)) +
      #Axes
      annotate("segment", x=seq(breaks,horizon,breaks), xend=seq(breaks,horizon,breaks), y=0, yend = length(unique(plot_df$order))+1, linetype = "dashed", color = "grey") +
      annotate("text", x=seq(0,horizon,breaks), y=-2, label = seq(0, horizon, breaks)) +
      annotate("text", x=-5, y=-2, label = "Months", hjust = "left", fontface = 2, size = 5) +
      geom_segment(aes(x=time, xend=end, y=order), linewidth = 1) +
      geom_point(size = 4, color = "black") +
      scale_color_manual(values=cols[which(all.events %in% cur.events)]) +
      scale_fill_manual(values=cols[which(all.events %in% cur.events)]) +
      scale_shape_manual(values = shapes[which(all.events %in% cur.events)]) +
      coord_cartesian(xlim=c(-3,horizon), ylim = c(min(seqs), height+15)) +
      theme_classic() +
      theme(legend.position = "none",
            axis.text = element_blank(),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank()) +
      labs(x="Months", y="Patients", fill = "", color = "", shape = "")

    if(length(split_df) > 1) {
      p <- p +
        annotate("text", x = -5, y = length(unique(plot_df$order))+5, label = names(split_df)[i], size = 6, hjust = "left")
    }

    if(i == 1) {

      p <- p +
        annotate("rect", xmin = horizon/4,
                 xmax = horizon-horizon/4,
                 ymin = height+5,
                 ymax = height+15,
                 color = "Black",
                 fill = "White",
                 linewidth = 1) +
        annotate("text", x=legend.df$symb+1, y=legend.df$y, label = legend.df$labs, size = 5, hjust = "left") +
        annotate("point", x=legend.df$symb, y=legend.df$y,
                 shape = legend.df$shapes,
                 size = 5,
                 fill = legend.df$cols)

    }

    for(i in seq_len(n)) {

      p <- p +
        annotate("rect", xmin=-6, xmax=horizon+2, ymin = ymins[i], ymax = ymaxs[i], fill = table.cols[i], color = "black", linewidth = 0.8) +
        annotate("text", x=-5, y=seqs[i]-1.5, label = names(table.list)[i], hjust = "left", size = 5) +
        annotate("text", x=table.list[[i]], y=seqs[i]-1.5, label = "X", hjust = "left")



    }

    p


  })


 collectR(plot_list, ncol = 1, collect=F)




}
