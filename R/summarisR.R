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
# summarisR(df, exclude = "time|event|t_")

summarisR <- function(data, vars, type = NULL, exclude = NULL, bins = 1) {


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

  plist <-
    lapply(vars_c, function(v) {

      if(length(unique(data[,v]))>10) {

        c <- sample(c("#9B62B8", "#224B87", "#67A8DC", "#D66ACE", "orange"), 1)

        s <- round(summary(data[,v])[c(1:3,5:6)],1)

        ggarrange(plotlist=list(

          ggplot(data, aes(x=!!sym(v))) +
            geom_boxplot(fill=c, col="Black") +
            theme_classic() +
            theme(axis.line = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  axis.title = element_blank(),
                  plot.title = element_text(hjust=0.5)) +
            labs(title = v) +
            annotate("text",
                     x=s,
                     y=-1,
                     label = s,
                     size = 3) +
            annotate("text",
                     x=s,
                     y=-2,
                     label = c("Min",
                               "Q1",
                               "Median",
                               "Q3",
                               "Max"),
                     size = 3) +
            scale_y_continuous(limits = c(-3,1)),

          ggplot(data, aes(x=!!sym(v))) +
          geom_histogram(fill=c, col="Black", binwidth = bins) +
          theme_classic() +
            theme(axis.line = element_blank(),
                  axis.ticks = element_blank(),
                  axis.text.y = element_blank(),
                  axis.title.y = element_blank())
          ), ncol = 1, nrow=2, heights = c(1,2))


      } else{


        if(!is.factor(data[, v])) {
          data[, v] <- as.factor(data[,v])
        }

        grps <- unique(data %>% drop_na(!!sym(v)) %>% pull(!!sym(v)))

        c <- sample(c("#9B62B8", "#224B87", "#67A8DC", "#D66ACE", "orange"), 1)

        t <- df %>% drop_na(!!sym(v)) %>%
          group_by(!!sym(v)) %>%
          summarise(n = n()) %>%
          ungroup()

        p <- ggplot(data %>% drop_na(!!sym(v)), aes(x=!!sym(v))) +
              geom_bar(fill=c, col="Black") +
              labs(x=v) +
              theme_classic() +
              labs(title=v) +
          theme(axis.line = element_blank(),
                axis.text.y = element_blank(),
                axis.title.y = element_blank(),
                axis.ticks = element_blank(),
                plot.title = element_text(hjust=0.5)) +
          annotate("text",
                   x = 1,
                   y = 1.1*max(t$n),
                   label = paste0("NA: ", sum(is.na(data %>% pull(!!sym(v))))))

        for(i in 1:length(grps)) {

          p <- p +
            annotate("text",
                     x=i,
                     y=1.20*max(t$n),
                     label = t[i,"n"])

        }

        p

      }
    })

  ggarrange(plotlist=plist)

}
