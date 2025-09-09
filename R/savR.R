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

# Save plotR
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
#
#
# Save extractR
# e <- extractR(t2) %>% flextable()
#
# savR(e)
#
# Save tablR
# n <- 300
# set.seed(1)
# df <- riskRegression::sampleData(n, outcome="survival")
# df$time <- round(df$time,1)*12
# df$X1 <- factor(rbinom(n, prob = c(0.3,0.4) , size = 2), labels = paste0("T",0:2))
# df$set <- as.factor(rep(seq(1,10),each=30))
# df$age_group <- sample(c("80-90", "10-20", "110-120", "0-40"), n, replace=TRUE)
#
# df <- as.data.frame(df)
#
# tablR(df,
#       group=X2,
#       vars=c(X1,X3,X4,X6,X7, age_group),
#       labels = list("age_group" = c("0-40" = "<=40"),
#                     "X1" = c("T2" = "T2-T3")),
#       headings = list("age_group" = "Age2"),
#       total = T,
#       numeric = c("median", "q1q3", "range"),
#       test = T,
#       show_na=T) %>%
# savR(name="table1")

savR <- function(object,
                 name,
                 width = 154,
                 height,
                 unit = "mm",
                 scale = 2,
                 dpi=900,
                 device= NULL,
                 compression="lzw",
                 formats = c("pdf", "tiff"),
                 size = 9,
                 table.width = 1) {

  formats <- match.arg(formats, c("pdf", "svg", "tiff", "jpg", "png"), several.ok=TRUE)

  if(missing(name)) {
    name <- paste0(substitute(object))
  }

  if(!dir.exists(paste0(getwd(), "/Tables and Figures"))) {
    dir.create(paste0(getwd(), "/Tables and Figures"))
  }

  if("summary.tableby" %in% class(object)) {
    object <- as.data.frame(object) %>%
      rename("char" = 1) %>%
      flextable() %>%
      set_header_labels(values = list("char" = ""))

  }

  if("flextable" %in% class(object)) {

    cat("Exports")
    cat(paste0("\nFlextable: "))

    object %>%
      fontsize(size = size, part = "all") %>%
      autofit() %>%
      width(width=table.width) %>%
      save_as_docx(path = paste0(getwd(), "/Tables and Figures/", name, ".docx", collapse=""))

    cat("Done")
  }

  if("ggplot" %in% class(object)) {
  #Autoscale
    if(missing(height)) {

    height <- sum(abs(object$coordinates$limits$y))/object$y*100*47
    }

    cat("Exports")
    for(p in formats) {

      cat(paste0("\n", p, ": "))

        if(p == "tiff") {
          ggsave(filename=paste0(name, ".", p, collapse=""),
                 plot=object,
                 path="Tables and Figures",
                 width = width,
                 height = height,
                 unit = unit,
                 scale = scale,
                 dpi=dpi,
                 device= grDevices::tiff,
                 compression=compression)
        } else {

            ggsave(filename=paste0(name, ".", p, collapse=""),
                   plot=object,
                   path="Tables and Figures",
                   width = width,
                   height = height,
                   unit = unit,
                   dpi = dpi,
                   scale = scale,
                   device=device)

        }

      cat("Done")

    }
  }

}


