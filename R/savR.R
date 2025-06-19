#' SavR
#'
#' @description
#' Wrapper for ggsave with default settings and automatic saving of multiple formats
#'
#'
#' @param plot Plot to plot
#' @param name File name of saved plot without extension
#' @param width width
#' @param height heigth. If missing autoscaling is performed
#' @param unit mm or cm
#' @param scale to fit
#' @param dpi resolution
#' @param device cairo for special occasions
#' @param compression for tiff
#' @param formats choose between pdf, svg, tiff, jpg and png
#'
#' @return Saves plot automatically in current project folder
#' @export
#'

savR <- function(plot,
                  name,
                  width = 154,
                  height,
                  unit = "mm",
                  scale = 2,
                  dpi=1200,
                  device= NULL,
                  compression="lzw",
                  formats = c("pdf", "svg", "tiff", "jpg")) {

  if(missing(name)) {
    name <- paste0(substitute(plot))
  }

  #Autoscale
  if(missing(height)) {
    height <- sum(abs(plot$coordinates$limits$y))*1.6*100
  }

  if(!dir.exists(paste0(getwd(), "/Plots"))) {
    dir.create(paste0(getwd(), "/Plots"))
  }

  invisible(suppressWarnings(sapply(formats, function(x, plot, width, name, height, unit, scale, dpi, device, compression) {

    if("tiff" %in% x) {
      ggsave(filename=paste0(name, ".", x, collapse=""),
             plot=plot,
             path="Plots",
             width = width,
             height = height,
             unit = unit,
             scale = scale,
             dpi=dpi,
             device= grDevices::tiff,
             compression=compression)
    }

    ggsave(filename=paste0(name, ".", x, collapse=""),
           plot=plot,
           path="Plots",
           width = width,
           height = height,
           unit = unit,
           dpi = dpi,
           scale = scale,
           device=device)

  }, plot=plot, width = width, height = height, unit = unit, scale = scale, name=name, dpi=dpi, device=device, compression=compression)))
}

