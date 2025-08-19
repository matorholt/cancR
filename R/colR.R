#' colR
#'
#' @description
#' Set default color palette
#'
#' @param cols Vector to set the default color palette. If "default", the default palette is reverted
#' @param option miscellaneous features. Includes "default" to reset to default palette, "shuffle" to shuffle the colors
#'
#' @return Adds cancR_palette to the global environment
#' @export
#'
#'
colR <- function(cols = NULL, option = "", show = T, seed = 69) {

  if(option %in% "default") {

    cols <- c("#AB5CB8", "#252A6E", "#FFB347", "#97AFCC", "#5D7A66", "#FFB8EE", "#B8D6FF", "#C4C4C4", "#815CB8", "#5164B0", "#95AD9F")

  }

  if(option %in% "shuffle") {

    set.seed(seed)
    cols <- sample(c("#AB5CB8", "#252A6E", "#FFB347", "#97AFCC", "#5D7A66", "#FFB8EE", "#B8D6FF", "#C4C4C4", "#815CB8", "#5164B0", "#95AD9F"))

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

.onLoad <- function(libname, pkgname) {

  cancR_palette <<- c("#AB5CB8", "#252A6E", "#FFB347", "#97AFCC", "#5D7A66", "#FFB8EE", "#B8D6FF", "#C4C4C4", "#815CB8", "#5164B0", "#95AD9F")
}


