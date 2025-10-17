#' Converter from Rscript to powershell format
#'
#'
#' @param text R script that needs to be converted to powershell format
#'
#' @return Printed text compatible with powershell
#' @export
#'
#'

powR <- function(text) {
  cat("\n#########  Instructions:  ############")
  cat("\n\n1)Open Windows PowerShell and insert the output below:
  \n2)Press enter
  \n3)Left-click on the screen where the script should be inserted and wait 5 seconds
  \n\n########  COPY  #############")

  cat(paste0("\n\nAdd-Type -AssemblyName System.Windows.Forms; Start-Sleep 5; [System.Windows.Forms.SendKeys]::SendWait('", str_replace_all(text, c("\\{" = "x{x",
                                                                                                                                                    "\\}" = "x}x",
                                                                                                                                                    "\\\\" = "\\\\\\\\",
                                                                                                                                                    "\\(" = "{\\(}",
                                                                                                                                                    "\\)" = "{\\)}",
                                                                                                                                                    "\\+" = "{\\+}",
                                                                                                                                                    "\\%" = "{\\%}",
                                                                                                                                                    "\\~" = "{\\~}  ",
                                                                                                                                                    "\\^" = "+¨",
                                                                                                                                                    "x\\{x" = "{{}",
                                                                                                                                                    "x\\}x" = "{}}",
                                                                                                                                                    "\n" = "{ENTER}",
                                                                                                                                                    "nylinje" = "\\\\n",
                                                                                                                                                    "\\}\\s" = "}",
                                                                                                                                                    "\\s+" = ' '
  )), "’)"))

}
