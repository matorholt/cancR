#' colR
#'
#' @description
#' Set default color palette
#'
#' @param cols Vector to set the default color palette. If "default", the default palette is reverted
#'
#' @return Adds cancR_palette to the global environment
#' @export
#'
#'
colR <- function(cols = NULL, option = "", show = T) {

  if(option %in% "default") {

    cols <- c("#AB5CB8", "#34397D", "#FB8E4B", "#819296", "#6F9580", "#FFB8EE", "#B8D6FF", "#C4C4C4", "#815CB8")

  }

  if(!is.null(cols)) {
    cancR_palette <<- cols

      }


  if(show) {

    cdf <- data.frame(id = as.factor(1:length(cancR_palette)),
                      cols = cancR_palette)


    ggplot(cdf, aes(x=1, y=id, fill=id, label = cols)) +
      geom_tile() +
      scale_fill_manual(values = cancR_palette) +
      theme_void() +
      geom_label() +
      theme(legend.position = "none")

  }

}

cancR_palette <- c("#AB5CB8", "#34397D", "#FB8E4B", "#819296", "#6F9580", "#FFB8EE", "#B8D6FF", "#C4C4C4", "#815CB8")

