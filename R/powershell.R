#' powershell
#'
#' @description
#' Converter from Rscript to powershell format
#'
#'
#' @param text R script that needs to be converted to powershell format
#'
#' @return Printed text compatible with powershell
#' @export
#'
#' @examples
#' powershell('rscript')
#'

powershell <- function(text) {
  if(str_detect(text, "\\^")) {warning("Caution: the text contains ^ which cannot be translated in powershell")}
  cat(str_replace_all(text, c("\\{" = "x{x",
                              "\\}" = "x}x",
                              "\\\\" = "\\\\\\\\",
                              "\\(" = "{\\(}",
                              "\\)" = "{\\)}",
                              "\\+" = "{\\+}",
                              "\\%" = "{\\%}",
                              "\\~" = "{\\~}  ",
                              "\\^" = "{\\^}",
                              "x\\{x" = "{{}",
                              "x\\}x" = "{}}",
                              "\n" = "{ENTER}",
                              "\\}\\s" = "}",
                              "\\s+" = ' '
  )))
  cat("\n\n\n\n#########  Instructions:  ############")
  cat("\n1)Open Windows PowerShell
  \n2)Insert the following separated by ctrl+enter:
  \n3)Insert the output in the R-console in 'text'
  \n4)Press enter
  \n5)Left-click on the screen where the script should be inserted and wait 5 seconds
  \n\n\n########  Copy  #############
  \nAdd-Type -AssemblyName System.Windows.Forms
  \nStart-Sleep 5
  \n[System.Windows.Forms.SendKeys]::SendWait('text’)
  \n########  Copy  #############")

}


