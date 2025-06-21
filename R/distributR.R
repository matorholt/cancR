#' distributR
#'
#' @description
#' Fast assessment of distribution of continuous variables with histograms, QQ-plots and the Shapiro-Wilks test
#'
#'
#' @param data dataframe
#' @param vars variables to test. If not specified all numeric variables with more than 5 unique values are assessed
#' @param bins binwidth
#'
#' @return Combined plotframe of histograms, QQ-plots and the Shapiro-Wilks tests
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
# df2 <- df %>% mutate(X2 = ifelse(row_number()==1, NA, X2),
#                      event = as.factor(event)) %>%
#   rename(ttt = time)
#
# distributR(df2, vars=c(X6, X7, X8))

distributR <- function(data, vars, bins = 1) {

  if(missing(vars)) {
    vars_c <- data %>% select(where(~all(length(unique(.))>5 & is.numeric(.)))) %>% names()
  } else {
    vars_c <- data %>% select({{vars}}) %>% names()
  }

  plotlist <- list()

  for(v in 1:length(vars_c)) {

    c <- sample(c("#9B62B8", "#224B87", "#67A8DC", "#D66ACE", "orange"), 1)

    plotlist[[v]] <-
      ggarrange(
      ggplot(data, aes(x=!!sym(vars_c[v]))) +
      geom_histogram(fill=c, col="Black", binwidth = bins) +
      theme_classic() +
        labs(title = vars_c[v]),
      ggplot(df2, aes(sample = !!sym(vars_c[v]))) +
        stat_qq(col = c) +
        stat_qq_line() +
        theme_classic() +
        labs(title = paste0("Shapiro-Wilks test: ", pvertR(shapiro.test(data[, vars_c[v]])$p.value))))

  }

  ggarrange(plotlist = plotlist, ncol = 1, nrow = length(vars_c))

}

