#' SavR
#'
#' @description
#' Wrapper for ggsave with default settings and automatic saving of multiple formats
#'
#'
#' @param object object to save
#' @param name File name of saved object without extension
#' @param width width
#' @param height heigth. If missing autoscaling is performed
#' @param unit mm or cm
#' @param scale to fit
#' @param dpi resolution
#' @param device cairo for special occasions
#' @param compression for tiff
#' @param formats choose between pdf, svg, tiff, jpg and png
#'
#' @return Saves object automatically in current project folder
#' @export
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
# t2 <- estimatR(df2, ttt, event2, X2, time = 60, type = "select", vars = c(X6,X7))
# t3 <- estimatR(df2, ttt, event2, X1, time = 60, type = "select", vars = c(X6,X7))
# t4 <- estimatR(df2, ttt, event2, X3, time = 60, type = "select", vars = c(X6,X7))
#
#
# p2 <- plotR(t2)
# p3 <- plotR(t3)
# p4 <- plotR(t4)
# e <- extractR(t2) %>% flextable()
#
# savR(e)

savR <- function(object,
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
    name <- paste0(substitute(object))
  }

  if(!dir.exists(paste0(getwd(), "/Tables and Figures"))) {
    dir.create(paste0(getwd(), "/Tables and Figures"))
  }

  if(class(object) %in% "flextable") {

    object %>%
      set_table_properties(layout = "autofit", width = 1) %>%
      save_as_docx(path = paste0(getwd(), "/Tables and Figures/", name, ".docx", collapse=""))

  }

  if(class(object) %in% "ggplot") {
  #Autoscale
  if(missing(height)) {

    height <- sum(abs(object$coordinates$limits$y))/object$y*100*47
  }

  invisible(suppressWarnings(sapply(formats, function(x, object, width, name, height, unit, scale, dpi, device, compression) {

    if("tiff" %in% x) {
      ggsave(filename=paste0(name, ".", x, collapse=""),
             plot=object,
             path="Tables and Figures",
             width = width,
             height = height,
             unit = unit,
             scale = scale,
             dpi=dpi,
             device= grDevices::tiff,
             compression=compression)
    }

    ggsave(filename=paste0(name, ".", x, collapse=""),
           plot=object,
           path="Tables and Figures",
           width = width,
           height = height,
           unit = unit,
           dpi = dpi,
           scale = scale,
           device=device)

  }, object=object, width = width, height = height, unit = unit, scale = scale, name=name, dpi=dpi, device=device, compression=compression)))

  }
}

