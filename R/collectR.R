#' collectR
#'
#' @description
#' Collection of multiple plots into one. Wrapper for ggarrange. Input must be a list of plots.
#'
#'
#' @param plots List of plots to be collected
#' @param collect Whether labels should be collected
#' @param nrow Number of rows
#' @param ncol Number of colums
#' @param ... See ggarrange
#'
#' @return Collected plot
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
# t2 <- estimatR(df2, ttt, event2, X2, time = 60)
# t3 <- estimatR(df2, ttt, event2, X1, time = 60)
# t4 <- estimatR(df2, ttt, event2, X3, time = 60)
#
# p2 <- plotR(t2)
# p3 <- plotR(t3)
# p4 <- plotR(t4)
#
# collectR(list(p2,p3,p4), legend.grob = get_legend(p4), widths = c(2,2,1))

collectR <- function(plots, collect=T, nrow=1, ncol=3, ...) {

  ggarrange(plotlist = plots, common.legend=collect, nrow=nrow, ncol=ncol, ...)
}





