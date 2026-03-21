#' Weight diagnostics for IPTW
#'
#' @description
#' Compute weights and diagnostic plots and mean differences
#'
#'
#' @param data data.frame
#' @param model logistic regression model for estimating propensity scores
#' @param treatment treatment variable
#' @param vars vector of covariates to estimate weights
#' @param labs.headings list of varible names in the format list(new = "old")
#' @param labs.subheadings list of variable levels in the format list(var = list(new = "old"))
#' @param difference whether the results should be outputtet as weighted or standardized weighted differences
#' @param weights name of the column containing weights
#' @param num.vars pseudo numerical variables for ordering of levels
#' @param simplify drop first level of binary variables
#'
#' @returns list containing a table of standardized differences (table), overall propensity scores and weights (overall), balance plot (balance) and weight plots (weightst)
#' @export
#'

# analysis_df <- analysis_df %>%
#   mutate(X11 = sample(c(NA, "no", "yes"), nrow(analysis_df), replace=TRUE)) %>%
#   factR(X11)
#
# model <- glm(g2 == "T1" ~ X4 + X5 + X6 + X7, data = analysis_df, family = "binomial")
#
# test <- weightR(analysis_df %>% factR(g2),
#                 model,
#                 treatment = g2,
#                 vars = c(X4, X5, X6, X7, X11))
#
# test$table
# test$balance
# test$overall


weightR <- function(data,
                    model,
                    treatment,
                    vars,
                    labs.headings = list(),
                    labs.subheadings = list(),
                    difference = "standardized",
                    weights = "w",
                    num.vars,
                    simplify = F,
                    digits = 2) {


  data <- data %>%
    drop_na({{treatment}})

  vars <- data %>% select({{vars}}) %>% names
  treatment <- data %>% select({{treatment}}) %>% names

  #Clean unspecified labs
  default.headings <- as.list(str_replace_all(str_to_title(vars), "_", " ")) %>% set_names(vars)

  labs.headings <- listR(modifyList(default.headings, listR(labs.headings, "reverse")), "reverse")


  if(!missing(num.vars)) {

    data <- data %>%
      factR(num.vars = {{num.vars}})

  }

  dat <- as.data.table(data)

  dat[, ps := predict(model, newdata=dat, type="response")]
  dat[, w := ifelse(eval(parse(text=names(model$model)[1])), 1/ps, 1/(1-ps))]

  dat <- recodR(dat, labs.subheadings, dt=T)

  # dat <- factR(dat,
  #              vars = data %>% select(vars) %>% select(is.factor) %>% names,
  #              auto.format = T,
  #              dt=T)

  w.df <- rbindlist(
    map(vars, function(i) {

      if(any(class(dat[[i]]) %in% c("factor", "character"))) {

        if(is.factor(dat[[i]])) {

          levels <- as.character(levels(dat[[i]]))

        } else {

          levels <- na.omit(unique(as.character(dat[[i]])))
        }

        if(simplify & length(levels) == 2) {
          levels <- levels[length(levels)]
        }


        rbindlist(
          map(levels, function(level) {

            dat[!is.na(get(i)), .(u.mean = mean((get(i) == level), na.rm=T),
                                  w.mean = sum(as.numeric((get(i) == level))*get(weights), na.rm=T)/sum(get(weights)),
                                  u.sd = sd((get(i) == level), na.rm=T),
                                  w.sd = sqrt(sum(get(weights)*(((get(i) == level)-(sum((get(i) == level)*get(weights))/sum(get(weights))))^2))/(sum(get(weights))-1))
            ), by = get(treatment)][, as.list(unlist(.SD))][
              , `:=`(absolute.diff.unweighted = (u.mean2-u.mean1),
                     absolute.diff.weighted = (w.mean2-w.mean1),
                     standardized.diff.unweighted = (u.mean2-u.mean1) / sqrt(((u.mean2*(1-u.mean2)) + (u.mean1*(1-u.mean1))) / 2),
                     standardized.diff.weighted = (w.mean2-w.mean1) / sqrt(((w.mean2*(1-w.mean2)) + (w.mean1*(1-w.mean1))) / 2),
                     var = i,
                     level = level)]

          }))




      } else {

        dat[!is.na(get(i)), .(u.mean = mean(get(i)),
                              w.mean = sum(get(i)*get(weights))/sum(get(weights)),
                              u.sd = sd(get(i)),
                              w.sd = sqrt(sum(get(weights)*((get(i)-(sum(get(i)*get(weights))/sum(get(weights))))^2))/(sum(get(weights))-1))
        ), by = get(treatment)][, as.list(unlist(.SD))][
          , `:=`(absolute.diff.unweighted = (u.mean2-u.mean1),
                 absolute.diff.weighted = (w.mean2-w.mean1),
                 standardized.diff.unweighted = (u.mean2-u.mean1) / sqrt((u.sd1^2 + u.sd2^2)/2),
                 standardized.diff.weighted = (w.mean2-w.mean1) / sqrt((w.sd1^2 + w.sd2^2)/2),
                 var = i,
                 level = " ")]
      }


    })
  )[,c(paste0("u.mean", c(1,2)), paste0("w.mean", c(1,2)), paste0(difference, ".diff.unweighted"), paste0(difference, ".diff.weighted"), "var", "level"), with=FALSE] %>%
    recodR(list(var = labs.headings)) %>%
    rename(!!sym(paste0(levels(dat[[treatment]])[1], "_crude")) := u.mean1,
           !!sym(paste0(levels(dat[[treatment]])[2], "_crude")) := u.mean2,
           !!sym(paste0(levels(dat[[treatment]])[1], "_weighted")) := w.mean1,
           !!sym(paste0(levels(dat[[treatment]])[2], "_weighted")) := w.mean2) %>%
    mutate(across(c(1:6), ~ round(., digits)))

  x <- ceiling(max(abs(range(w.df[[paste0(difference, ".diff.unweighted")]])))*10)/10

  plot.df <- as.data.frame(w.df) %>%
    pivot_longer(cols = c(!!sym(paste0(levels(dat[[treatment]])[1], "_crude")):!!sym(paste0(difference, ".diff.weighted"))), names_to = "est", values_to = "value") %>%
    filter(str_detect(est, difference)) %>%
    ungroup() %>%
    rollR(vars = c(var, level), label = order) %>%
    mutate(order = rev(order))

  labels.df <- plot.df %>%
    group_by(var, level) %>%
    slice(1) %>%
    arrange(desc(order)) %>%
    group_by(var) %>%
    mutate(header = max(order))

  balance.plot <- ggplot(plot.df, aes(x=value, y=order, color = est)) +
    annotate("segment", x=-x, xend=x, y=0, yend=0, linewidth = 0.8) +
    annotate("segment", x=0, xend=0, y=0, yend = nrow(labels.df)+1, linewidth = 0.8) +
    annotate("segment", x=c(-0.1, 0.1), xend=c(-0.1, 0.1), y=0, yend = nrow(labels.df)+1, linewidth = 0.5, linetype = "dashed") +
    annotate("text", x=c(seq(-x,-0.1,0.1), seq(0,x,0.1)), y=-0.5, label = c(seq(-x,-0.1,0.1), seq(0,x,0.1))) +
    annotate("text", x=0, y=-2, label = ifelse(difference == "standardized", "Standardized Mean Difference", "Mean Difference"), size = 6) +
    geom_point(size = 2.5) +
    scale_color_manual(values = cancR_palette, labels = c("Crude", "Weighted")) +
    scale_x_continuous(breaks = c(-x,x)) +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          legend.text = element_text(size = 16),
          legend.position = c(1-((x/(x+(x*1.4)))),0.98),
          legend.direction = "horizontal") +
    labs(color = "") +
    annotate("text", x=-x*1.2, y=labels.df$order, label = labels.df$level) +
    annotate("text", x=-x*1.4, y=unique(labels.df$header), label = unique(labels.df$var), fontface = 2) +
    coord_cartesian(xlim=c(-x*1.4, x))

  dat <- as.data.frame(dat)

  weight.plots <- collectR(
    map(vars, function(v) {

      if(class(dat[[v]]) %in% c("factor", "character")) {

        p <- ggplot(dat, aes(y=w, fill=!!sym(v), color = !!sym(v), group = !!sym(v)))

      } else {

        p <- ggplot(dat, aes(y=w, fill = v, color = v)) +
          guides(fill="none", color = "none")

      }

      max.weight <- max(dat$w)

      p <- p +
        geom_density(data = dat %>% filter(!!sym(treatment) == as.character(levels(dat[[treatment]]))[1]), alpha = 0.4) +
        geom_density(data = dat %>% filter(!!sym(treatment) == as.character(levels(dat[[treatment]]))[2]), aes(after_stat(-density)), alpha = 0.4) +
        geom_vline(xintercept = 0) +
        geom_hline(yintercept = 1) +
        scale_y_continuous(breaks = seq(2.5,max.weight,2.5)) +
        scale_fill_manual(values = cancR_palette) +
        scale_color_manual(values = cancR_palette) +
        theme_classic() +
        theme(axis.line = element_blank(),
              axis.title.x = element_text(size = 16),
              axis.text.x = element_blank(),
              axis.ticks = element_blank(),
              panel.grid.major.y = element_line(color = "grey"),
              legend.position = "top") +
        labs(x=names(labs.headings)[labs.headings == v], y="", fill = "", color = "")

      dens <- max(abs(c(ggplot_build(p)$data[[1]]$density, ggplot_build(p)$data[[2]]$density)))

      p <- p +
        coord_cartesian(xlim=c(-dens, dens), ylim = c(1,max.weight))



    }), collect = F)

  weight.plots <- ggpubr::annotate_figure(weight.plots,
                                          left = ggpubr::text_grob("Weights", face = "bold", size = 14, rot = 90),
                                          bottom = ggpubr::text_grob(paste0(levels(dat[[treatment]])[1],
                                                                            " versus " ,
                                                                            levels(dat[[treatment]])[2]), face = "bold", size = 14))

  total.plots <-
    collectR(
      map(c("ps","w"), function(i) {
        ggplot(dat, aes(x=!!sym(i), fill = !!sym(treatment))) +
          geom_density(alpha = 0.4) +

          # Boxplots (computed internally)
          geom_boxplot(
            aes(y = 16, group = !!sym(treatment)),
            width = 2,
            outlier.shape = NA,
            alpha = 0.7
          ) +
          scale_fill_manual(values = cancR_palette) +
          theme_classic()
      }), ncol = 2)

  return(list(table = w.df,
              overall = total.plots,
              balance = balance.plot,
              weights = weight.plots))


}
