#' Format p-values to AMA manual of style
#'
#' @param x A p-value
#' @return Prints the raw p-value according to AMA manual of style
#' @export

pvertR <- function(x) {
  sapply(x, function(x) {
    if(is.na(x)){
      y="NA"
    }
    else if(x<0.001){
      y = "p < 0.001"
    }
    else if(x < 0.01){
      y = paste0("p = ", numbR(x,3,2))
    }
    else if(x>=0.045 & x < 0.05){
      y = paste0("p = ", numbR(x,3,2))
    }
    else if(x>=0.01 & x<1){
      y = paste0("p = ", numbR(x,2,2))
    }
    else if(x>=1){
      y = paste0("p = 1.00")
    }
    else {
      y = "NA"
    }
    return(y)
  })
}
