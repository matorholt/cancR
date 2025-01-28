pfun <- function(x) {
#' P-function
#' @description
#' The function takes a raw p-value and prints it according to the AMA manual of style
#'
#' @param x A p-value
#' @return Prints the raw p-value according to AMA manual of style
#' @export
#' @examples
#' pfun(0.0245)
#' pfun(0.000001)
#' pfun(0.048)
#' pfun(0.056)


  sapply(x, function(x) {
    if(is.na(x)){
      y="NA"
    }
    else if(x<0.001){
      y = "p < 0.001"
    }
    else if(x < 0.01){
      y = paste0("p = ", format(round(x,3), nsmall=2))
    }
    else if(x>=0.045 & x < 0.05){
      y = paste0("p = ", format(round(x,3), nsmall=2))
    }
    else if(x>=0.01 & x<1){
      y = paste0("p = ", format(round(x,2), nsmall=2))
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
