#' Graphical overview of an entire dataset
#'
#'
#' @param data dataset
#' @param vars vector of variables to be presented
#' @param group Optional grouping variable
#' @param type shortcut to present all numeric or categorical variables ("numeric" or "categorical")
#' @param layout whether the bar plots should be "horizontal" (side-by-side) or "vertical" (stacked)
#' @param exclude regex of variables that should not be presented
#' @param bins number of bins for histograms
#' @param cols Color palette, defaults to cancR_palette
#' @param headings list specifying new variabel labels for layout
#' @param vjust vertical adjustment of the counts (pct) labels
#' @param text.color label colors
#' @param label.size label size
#'
#' @return Returns bar charts or density plots depending on format. Numerical variables with less than 5 unique values are considered as factors.
#' @export
#'
#' @examples
#'
#' df <- analysis_df %>%
#'mutate(event_date = sample(c(seq(as.Date("1980-01-01"), as.Date("2000-01-01"), by = "years"), NA), size = n(), replace = TRUE))
#'
#'summarisR(df, vars = c(X6, X7, event_date))
#'summarisR(data=df,vars=c(X6, X7, X1, X3), group = X2)
#'summarisR(df, exclude = "time|event|t_", group = X2)
#'summarisR(df, c(X3,X1), group=X2, layout = "vertical")
#'summarisR(df, c(X3,X1), group=X2, layout = "horizontal", label.size = 3)



summarisR <- function(data,
                      vars,
                      group,
                      type = NULL,
                      layout = "horizontal",
                      exclude = NULL,
                      bins = 1,
                      cols = cancR_palette,
                      headings = list(),
                      labels = F,
                      vjust = -0.5,
                      text.color = "White",
                      label.size = 3,
                      alpha = 0.8) {


  if(!is.null(exclude)) {
    data <- data %>% select(-matches(exclude))
  }

  if(!missing(vars)) {

    vars_c <- data %>% select({{vars}}) %>% names()

  } else if(!is.null(type)) {

    type <- match.arg(type, c("numeric", "categorical"))

   if(type == "numeric") {
     vars_c <- data %>% select(where(~all(length(unique(.))>10))) %>% names()
   }
    if(type == "categorical") {
      vars_c <- data %>% select(where(~all(length(unique(.))<=10 | is.factor(.) | is.character(.)))) %>% names()
    }
  } else {
    vars_c <- colnames(data)
  }

  layout <- match.arg(layout, c("horizontal", "vertical"))

  if(!missing(group)) {

    grp_c <- data %>% select({{group}}) %>% names()
    vars_c <- vars_c[vars_c != grp_c]
    data <- data %>% drop_na({{group}}) %>% factR({{group}})

  } else {
    grp_c <- NULL
  }

  headings_default <- as.list(str_to_title(str_replace_all(vars_c, "_", " "))) %>% set_names(vars_c)
  headings <- modifyList(headings_default, headings)

  data <- data %>% rename_with(~ unlist(headings), all_of(names(headings)))

 plist <-
 lapply(headings, function(v) {

      if(length(unique(data[,v]))>10 & class(data[,v]) != "character") {

        c <- sample(cols, 1)

        if(!is.null(grp_c)) {

          p1 <- ggplot(data, aes(y=!!sym(v), x=!!sym(grp_c), fill=!!sym(grp_c))) +
            geom_boxplot(alpha=0.8)

          p2 <- ggplot(data, aes(x=!!sym(v), fill=!!sym(grp_c), color = !!sym(grp_c))) +
            geom_histogram(binwidth = bins, color = "Black", alpha = alpha)


          c <- cols

          gheight = 1

        } else {

          p1 <- ggplot(data, aes(y=!!sym(v))) +
            geom_boxplot(fill = c, alpha = alpha)

          p2 <- ggplot(data, aes(x=!!sym(v))) +
            geom_histogram(binwidth = bins, fill = c, color = "Black", alpha = alpha)

          gheight = 2
        }

        p1 <- p1 +
          geom_label(
            aes(y=stage(!!sym(v), c(ymin)),
                label = round(after_stat(c(ymin)),1)),
            stat = StatBoxplot,
            show.legend=F,
            size = 3
          ) +
          geom_label(
            aes(y=stage(!!sym(v), c(lower)),
                label = round(after_stat(c(lower)),1)),
            stat = StatBoxplot,
            show.legend=F,
            size = 3
          ) +
          geom_label(
            aes(y=stage(!!sym(v), c(middle)),
                label = round(after_stat(c(middle)),1)),
            stat = StatBoxplot,
            show.legend=F,
            size = 3
          ) +
          geom_label(
            aes(y=stage(!!sym(v), c(upper)),
                label = round(after_stat(c(upper)),1)),
            stat = StatBoxplot,
            show.legend=F,
            size = 3
          ) +
          geom_label(
            aes(y=stage(!!sym(v), c(ymax)),
                label = round(after_stat(c(ymax)),1)),
            stat = StatBoxplot,
            show.legend=F,
            size = 3
          ) +
          scale_fill_manual(values=c) +
          scale_color_manual(values=c) +
              theme_classic() +
              theme(axis.line = element_blank(),
                    axis.text = element_blank(),
                    axis.ticks = element_blank(),
                    axis.title = element_blank(),
                    plot.title = element_text(hjust=0.5),
                    legend.position = "none") +
              labs(title = v) +
          coord_flip()
        #
        p2 <- p2 +
          scale_fill_manual(values=c) +
          scale_color_manual(values=c) +
          theme_classic() +
          theme(axis.line = element_blank(),
                axis.ticks = element_blank(),
                axis.text.y = element_blank(),
                axis.title.y = element_blank(),
                legend.position = "none")



        ggarrange(plotlist = list(p1,p2), ncol = 1, nrow=2, heights = c(1,gheight), common.legend=F)


      } else{


        if(!is.factor(data[, v])) {
          data[, v] <- as.factor(data[,v])
        }

        c <- sample(cols,1)

        if(layout == "vertical") {

          c <- cols

          p <- ggplot(data, aes(x=!!sym(grp_c), fill = !!sym(v))) +
             geom_bar(position = "fill", color = "Black", alpha = alpha) +
            scale_y_continuous(breaks = seq(0,1,0.25), labels = paste0(seq(0,100,25), " %")) +
            theme_classic() +
            theme(axis.line.x = element_blank(),
                  axis.title = element_blank(),
                  axis.ticks = element_blank(),
                  plot.title = element_text(hjust=0.5),
                  legend.position = "top")

          if(labels) {

            p <- p +
              geom_text(aes(x = !!sym(grp_c),
                            label = paste0(after_stat(count), " (", scales::percent(after_stat(count / tapply(count, x, sum)[x])), ")"),
                            group = !!sym(v)), position = position_fill(0.5), stat = "count", color = text.color,
                        vjust = vjust,
                        size = label.size)

          }


        } else {

        if(!is.null(grp_c)) {

          p <- ggplot(data %>% drop_na(!!sym(v)), aes(x=!!sym(v), fill=!!sym(grp_c), group=!!sym(grp_c))) +
            geom_bar(position = "dodge", color = "Black", alpha = alpha)

          c <- cols

          p <- p +
            scale_fill_manual(values=c) +
            labs(x=v, title=v, fill = "")

        } else {

          p <- ggplot(data %>% drop_na(!!sym(v)), aes(x=!!sym(v), group=1)) +
               geom_bar(position = "dodge", fill = c, color = "Black", alpha = alpha)
        }
        p <- p +
            geom_text(aes(y=after_stat(count)+max(after_stat(count))/15,
                          label=paste0(after_stat(count), " (", round(after_stat(prop),2)*100, "%)")),
                      position = position_dodge(width = .9),
                      size = label.size,
                      stat="count") +
          theme_classic() +
          theme(axis.line = element_blank(),
                axis.text.y = element_blank(),
                axis.title = element_blank(),
                axis.ticks = element_blank(),
                plot.title = element_text(hjust=0.5),
                legend.position = "top")

        }

        p <- p +
          scale_fill_manual(values=c) +
          labs(x=v, title=v, fill = "")

      }
    })

  ggarrange(plotlist=plist, common.legend = ifelse(layout == "horizontal", T,F))



}

