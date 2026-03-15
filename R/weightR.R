weightR <- function(data,
                    model,
                    treatment,
                    vars,
                    labs.headings = list(),
                    labs.subheadings = list(),
                    difference = "weighted",
                    weights = "w",
                    num.vars) {

  vars <- data %>% select({{vars}}) %>% names
  treatment <- data %>% select({{treatment}}) %>% names

  if(!missing(num.vars)) {

    #num.vars <- data %>% select({{num.vars}}) %>% names

    data <- data %>%
      factR(num.vars = {{num.vars}})

  }

  dat <- as.data.table(data)

  dat[, ps := predict(model, newdata=dat, type="response")]
  dat[, w := ifelse(eval(parse(text=names(model$model)[1])), 1/ps, 1/(1-ps))]

  dat <- recodR(dat, labs.subheadings, dt=T)

  w.df <- rbindlist(
    map(vars, function(i) {

      levels <-(as.character(levels(dat[[i]])))

      if(length(levels) == 2) {
        levels <- levels[length(levels)]
      }

      rbindlist(map(levels, function(level) {



        #if continuous
        #u.std = (u.mean2-u.mean1) / sqrt((u.sd1^2 + u.sd2^2)/2),
        # w.std = (w.mean2-w.mean1) / sqrt((w.sd1^2 + w.sd2^2)/2),

        dat[, .(u.mean = mean((get(i) == level)),
                w.mean = sum((get(i) == level)*get(weights))/sum(get(weights)),
                u.sd = sd((get(i) == level)),
                w.sd = sqrt(sum(get(weights)*(((get(i) == level)-(sum((get(i) == level)*get(weights))/sum(get(weights))))^2))/(sum(get(weights))-1))
        ), by = get(treatment)][, as.list(unlist(.SD))][
          , `:=`(u.diff.weighted = (u.mean2-u.mean1),
                 w.diff.weighted = (w.mean2-w.mean1),
                 u.diff.standardized = (u.mean2-u.mean1) / sqrt(((u.mean2*(1-u.mean2)) + (u.mean1*(1-u.mean1))) / 2),
                 w.diff.standardized = (w.mean2-w.mean1) / sqrt(((w.mean2*(1-w.mean2)) + (w.mean1*(1-w.mean1))) / 2),
                 var = i,
                 level = level)]

      }))


    }))[,c(paste0("u.mean", c(1,2)), paste0("w.mean", c(1,2)), paste0("u.diff.", difference), paste0("w.diff.", difference), "var", "level"), with=FALSE] %>%
    recodR(list(var = labs.headings)) %>%
    mutate(var = ifelse(var %nin% names(labs.headings), str_replace_all(str_to_title(var), "_", " "), var))

  x <- ceiling(max(abs(range(w.df[[paste0("u.diff.", difference)]])))*10)/10

  plot.df <- as.data.frame(w.df) %>%
    pivot_longer(cols = c(u.mean1:!!sym(paste0("w.diff.", difference))), names_to = "est", values_to = "value") %>%
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

  plot <- ggplot(plot.df, aes(x=value, y=order, color = est)) +
    annotate("segment", x=-x, xend=x, y=0, yend=0, linewidth = 0.8) +
    annotate("segment", x=0, xend=0, y=0, yend = nrow(labels.df)+2, linewidth = 0.8) +
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

  return(list("table" = w.df,
              "plot" = plot))


}
