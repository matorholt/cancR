#' Saver function to automatically save plots
#'
#' @description
#' Wrapper for ggsave with default settings and automatic saving of multiple formats
#'
#'
#' @param plot Plot to plot
#' @param name File name of saved plot without extension
#' @param width width
#' @param height heigth
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
#'
saver <- function(plot,
                  name,
                  width = 154,
                  height = 60,
                  unit = "mm",
                  scale = 2,
                  dpi=1200,
                  device= NULL,
                  compression="lzw",
                  formats = c("pdf", "svg", "tiff")) {

  invisible(suppressWarnings(sapply(formats, function(x, plot, width, name, height, unit, scale, dpi, device, compression) {

    if("tiff" %in% x) {
      ggsave(ifelse(missing(name), paste0(substitute(plot), ".", x, collapse=""), paste0(name, ".", x, collapse="")),
             plot=plot,
             width = width,
             height = height,
             unit = unit,
             scale = scale,
             dpi=dpi,
             device= grDevices::tiff,
             compression=compression)
    }

    ggsave(ifelse(missing(name), paste0(substitute(plot), ".", x, collapse=""), paste0(name, ".", x, collapse="")),
           plot=plot,
           width = width,
           height = height,
           unit = unit,
           scale = scale,
           device=device)

  }, plot=plot, width = width, height = height, unit = unit, scale = scale, name=name, dpi=dpi, device=device, compression=compression)))
}
