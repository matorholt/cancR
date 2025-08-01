#' summarisR
#'
#' @description
#' Graphical overview of an entire dataset
#'
#'
#' @param data dataset
#' @param vars vector of variables to be presented
#' @param type shortcut to present all numeric or categorical variables ("numeric" or "categorical")
#' @param exclude regex of variables that should not be presented
#' @param bins number of bins for histograms
#'
#' @return Returns bar charts or density plots depending on format. Numerical variables with less than 5 unique values are considered as factors.
#' @export
#'
#'
# n <- 500
# set.seed(1)
# df <- riskRegression::sampleData(n, outcome="survival")
# df$time <- round(df$time,1)*12
# df$time2 <- df$time + rnorm(n)
# df$X1 <- factor(rbinom(n, prob = c(0.3,0.4) , size = 2), labels = paste0("T",0:2))
# df$X3 <- factor(rbinom(n, prob = c(0.3,0.4,0.3) , size = 3), labels = paste0("T",0:3))
# df$event2 <- rbinom(n, 2, prob=.3)
# df <- as.data.frame(df)
#
# df <- df %>% mutate(X2 = ifelse(row_number()==1, NA, X2),
#                      event = as.factor(event)) %>%
#   rename(ttt = time)
#
# summarisR(data=df,vars=c(X6, X7, X1, X3), group = X2)
# summarisR(data=df,vars=c(X6))
# summarisR(df, exclude = "time|event|t_", group = X2)

summarisR <- function(data, vars, group, type = NULL, exclude = NULL, bins = 1, cols = cancR_palette) {


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

  if(!missing(group)) {

    grp_c <- data %>% select({{group}}) %>% names()
    vars_c <- vars_c[vars_c != grp_c]
    data <- data %>% drop_na({{group}}) %>% factR({{group}})

  } else {
    grp_c <- NULL
  }

 plist <-
 lapply(vars_c, function(v) {

      if(length(unique(data[,v]))>10) {

        c <- sample(cols, 1)

        s <- round(summary(data[,v])[c(1:3,5:6)],1)

        if(!is.null(grp_c)) {
          p1 <- ggplot(data, aes(y=!!sym(v), x=X2, fill=X2)) +
            geom_boxplot()

          p2 <- ggplot(data, aes(x=!!sym(v), fill=!!sym(grp_c), color = !!sym(grp_c))) +
            geom_histogram(binwidth = bins, color = "Black")


          c <- cols

          gheight = 1

        } else {

          p1 <- ggplot(data, aes(y=!!sym(v))) +
            geom_boxplot(fill = c)

          p2 <- ggplot(data, aes(x=!!sym(v))) +
            geom_histogram(binwidth = bins, fill = c, color = "Black")

          gheight = 2
        }

        p1 <- p1 +
          geom_label(
            aes(y=stage(!!sym(v), c(min, lower,middle,upper, max)),
                label = round(after_stat(c(min, lower, middle, upper, max)),1)),
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

        if(!is.null(grp_c)) {
          p <- ggplot(data %>% drop_na(!!sym(v)), aes(x=!!sym(v), fill=!!sym(grp_c), group=!!sym(grp_c))) +
            geom_bar(position = "dodge")

          c <- cols

        } else {

          p <- ggplot(data %>% drop_na(!!sym(v)), aes(x=!!sym(v), group=1)) +
               geom_bar(position = "dodge", fill = c)
        }
        p <- p +
            geom_text(aes(y=after_stat(count)+max(after_stat(count))/15,
                          label=paste0(after_stat(count), " (", round(after_stat(prop),3)*100, "%)")),
                      position = position_dodge(width = .9),
                      size = 3,
                      stat="count") +
          scale_fill_manual(values=c) +
                labs(x=v) +
                theme_classic() +
                labs(title=v) +
            theme(axis.line = element_blank(),
                  axis.text.y = element_blank(),
                  axis.title.y = element_blank(),
                  axis.ticks = element_blank(),
                  plot.title = element_text(hjust=0.5))

        p

      }
    })

  ggarrange(plotlist=plist, common.legend = T)


}

