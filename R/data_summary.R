#' Data summary
#'
#' @description
#' Graphical overview of entire dataset
#'
#'
#' @param data Dataset
#'
#' @return Returns bar charts or density plots depending on format. Numerical variables with less than 5 unique values are considered as factors.
#' @export
#'
#'
data_summary <- function(data) {

  plist <- lapply(colnames(data), data=data, function(data, var) {

    if(is.character(data[,var]) | is.factor(data[,var])) {
      ggplot(data, aes_string(x=var, fill = var)) +
        geom_bar() +
        labs(x=var) +
        theme_classic()
    }

    if(is.numeric(data[,var]) & length(unique(data[,var])) < 5) {
      data[, var] <- as.character(data[, var])
      ggplot(data, aes_string(x=var, fill = var)) +
        geom_bar() +
        labs(x=var) +
        theme_classic()
    } else if(is.numeric(data[,var])) {
      ggplot(data, aes_string(x=var)) +
        geom_density(fill = "Steelblue", color = "Steelblue", alpha=0.5) +
        theme_classic()
    }

  })

  ggarrange(plotlist = plist[lengths(plist) != 0])

}
