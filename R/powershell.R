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
  cat(str_replace_all(text, c("\\{" = "x{x",
                              "\\}" = "x}x",
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
  cat("\n\n---------------------------------------------------------------------")
  cat("\n\nAdd-Type -AssemblyName System.Windows.Forms
\nStart-Sleep 5
\n[System.Windows.Forms.SendKeys]::SendWait('text’)")

}
