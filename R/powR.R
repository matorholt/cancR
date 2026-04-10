#' Automated keyboard strokes
#'
#' @param text single-quote enclosed text to automate
#' @param write.sleep delay between text chunks (default = 0.025 secs)
#' @param mouse.sleep delay between mouse actions (default = 0.15 secs)
#' @param special.sleep delay betweem alt-hold and alt code (default = 0.1)
#' @param where enviroment, pc, laptop or iw containing customised coordinates
#' @param server.language whether the language on the server should be changed (default = T)
#' @param server.automate whether automatic parenthesis and quotes should be disabled (default = T)
#'
#' @return a keyboard and mouse automation that transfers the assigned text
#' @export
#'
#'


powR <- function(text,
                 write.sleep = 0.02,
                 mouse.sleep = 0.5,
                 special.sleep = 0.05,
                  where = "pc",
                 server.language = T,
                 server.options = T,
                 toggle.options = T) {

  tickR()

  # return(    str_replace_all(text, c('(?<!(\\s{3}))\n(?=("))' = "NEW",
  #                                    '(?<=("))\n(?!(\\s{3}))' = "NEW",
  #                                    "\n" = "xsplitxenterxsplitx",
  #                                    "NEW" = "xsplitxNEWxsplitx",
  #                                    "\\\\" = "xsplitxdbackslashxsplitx",
  #                                    "," = "xsplitxcommaxsplitx",
  #                                    "<" = "xsplitxarrowxsplitx",
  #                                    "-" = "xsplitxlinexsplitx",
  #                                    "≤" = "xsplitxlessxsplitx",
  #                                    "_" = "xsplitxunsxsplitx",
  #                                    ":" = "xsplitxcolonxsplitx",
  #                                    ";" = "xsplitxsemcol",
  #                                    "~" = "xsplitxtildexsplitx",
  #                                    "`" = "xsplitxffxsplitx")) %>% str_split(., "xsplitx"))

  #Coordinates
  coord.list <- list(pc = list(rs = c(2665, 167),
                               tool = c(2944, 31),
                               go = c(2993, 401),
                               code = c(3569, 483),
                               match = c(3688, 575),
                               app = c(4113, 1034),
                               ok = c(3917, 1034),
                               local = c(623, 546)),
                     laptop = list(rs = c(648,1572),
                                   tool = c(381, 34),
                                   go = c(467, 404),
                                   code = c(1052, 589),
                                   match = c(1144, 676),
                                   app = c(1528, 1095),
                                   ok = c(1349, 1095),
                                   local = c(3917, 546)))


  #Define current and on.exits

  #Current cursor location
  current <- KeyboardSimulator::mouse.get_cursor()

  on.exit({
    #Release ALT
    KeyboardSimulator::keybd.release("alt")
    #Toggle matching parens/quotes
    if(toggle.options) {
      KeyboardSimulator::mouse.move(coord.list[[where]][["rs"]][1],coord.list[[where]][["rs"]][2])
      Sys.sleep(mouse.sleep)
      KeyboardSimulator::mouse.click("left")
      Sys.sleep(0.5)
      KeyboardSimulator::keybd.press("ctrl", hold = T)
      KeyboardSimulator::keybd.press("shift", hold = T)
      KeyboardSimulator::keybd.press("p", hold = T)
      KeyboardSimulator::keybd.release("ctrl")
      KeyboardSimulator::keybd.release("shift")
      KeyboardSimulator::keybd.release("p")
      Sys.sleep(0.5)
      KeyboardSimulator::keybd.type_string("parentheses and")
      Sys.sleep(0.5)
      KeyboardSimulator::keybd.press("enter")
      KeyboardSimulator::keybd.press("esc")
      # cat("\nReturning to standard settings\n")
      # KeyboardSimulator::mouse.move(coord.list[[where]][["tool"]][1], coord.list[[where]][["tool"]][2])
      # Sys.sleep(mouse.sleep)
      # KeyboardSimulator::mouse.click("left")
      # KeyboardSimulator::mouse.move(coord.list[[where]][["go"]][1],coord.list[[where]][["go"]][2])
      # Sys.sleep(mouse.sleep)
      # KeyboardSimulator::mouse.click("left")
      # KeyboardSimulator::mouse.move(coord.list[[where]][["code"]][1],coord.list[[where]][["code"]][2])
      # Sys.sleep(mouse.sleep)
      # KeyboardSimulator::mouse.click("left")
      # KeyboardSimulator::mouse.move(coord.list[[where]][["match"]][1],coord.list[[where]][["match"]][2])
      # Sys.sleep(mouse.sleep)
      # KeyboardSimulator::mouse.click("left")
      # KeyboardSimulator::mouse.move(coord.list[[where]][["app"]][1],coord.list[[where]][["app"]][2])
      # Sys.sleep(mouse.sleep)
      # KeyboardSimulator::mouse.click("left")
      # KeyboardSimulator::mouse.move(coord.list[[where]][["ok"]][1],coord.list[[where]][["ok"]][2])
      # Sys.sleep(mouse.sleep)
      # KeyboardSimulator::mouse.click("left")
    }

    #Change language back server
    if(server.language){
      cat("\nReturning to standard language on server\n")
      Sys.sleep(0.5)
      KeyboardSimulator::keybd.press("win", hold = T)
      KeyboardSimulator::keybd.press(" ")
      KeyboardSimulator::keybd.release("win")
      KeyboardSimulator::mouse.click("left")
    }

    #Close window
    if(where == "laptop") {
      KeyboardSimulator::mouse.move(1485,13)
      Sys.sleep(mouse.sleep)
      KeyboardSimulator::mouse.click("left")
      Sys.sleep(mouse.sleep)
    }

    #Change language back main computer
    cat("\nReturning to standard language on local\n")
    KeyboardSimulator::mouse.move(coord.list[[where]][["local"]][1],coord.list[[where]][["local"]][2])
    Sys.sleep(mouse.sleep)
    KeyboardSimulator::mouse.click("left")
    Sys.sleep(mouse.sleep)
    KeyboardSimulator::keybd.press("win", hold = T)
    KeyboardSimulator::keybd.press(" ")
    KeyboardSimulator::keybd.release("win")

    #Default cursor location
    KeyboardSimulator::mouse.move(current[1], current[2])

    cat(paste0("\nTotal runtime: \n"))
    cat(tockR("diff"))

  }, add=TRUE)

  cat("\nConverting to US keyboard\n")
  Sys.sleep(0.5)
  KeyboardSimulator::keybd.press("win", hold = T)
  KeyboardSimulator::keybd.press(" ")
  KeyboardSimulator::keybd.release("win")

  cat("\nChoosing screen to write and configuring R\n")



  Sys.sleep(mouse.sleep)

  #Select screen
  if(where == "laptop") {
  KeyboardSimulator::mouse.move(coord.list[[where]][["rs"]][1],coord.list[[where]][["rs"]][2])
  Sys.sleep(mouse.sleep)
  KeyboardSimulator::mouse.click("left")
  Sys.sleep(mouse.sleep)
  }

  KeyboardSimulator::mouse.move(coord.list[[where]][["rs"]][1],coord.list[[where]][["rs"]][2])
  Sys.sleep(mouse.sleep)
  KeyboardSimulator::mouse.click("left")

  #Select US keyboard if on server
  if(server.language){
    Sys.sleep(0.5)
    KeyboardSimulator::keybd.press("win", hold = T)
    KeyboardSimulator::keybd.press(" ")
    KeyboardSimulator::keybd.release("win")
    KeyboardSimulator::mouse.click("left")
  }

  #Toggle matching parens/quotes
  if(toggle.options) {
    KeyboardSimulator::mouse.move(coord.list[[where]][["rs"]][1],coord.list[[where]][["rs"]][2])
    Sys.sleep(mouse.sleep)
    KeyboardSimulator::mouse.click("left")
    Sys.sleep(0.5)
    KeyboardSimulator::keybd.press("ctrl", hold = T)
    KeyboardSimulator::keybd.press("shift", hold = T)
    KeyboardSimulator::keybd.press("p", hold = T)
    KeyboardSimulator::keybd.release("ctrl")
    KeyboardSimulator::keybd.release("shift")
    KeyboardSimulator::keybd.release("p")
    Sys.sleep(0.5)
    KeyboardSimulator::keybd.type_string("parentheses and")
    Sys.sleep(0.5)
    KeyboardSimulator::keybd.press("enter")
    KeyboardSimulator::keybd.press("esc")
}
  # KeyboardSimulator::mouse.move(coord.list[[where]][["tool"]][1], coord.list[[where]][["tool"]][2])
  # Sys.sleep(mouse.sleep)
  # KeyboardSimulator::mouse.click("left")
  # KeyboardSimulator::mouse.move(coord.list[[where]][["go"]][1],coord.list[[where]][["go"]][2])
  # Sys.sleep(mouse.sleep)
  # KeyboardSimulator::mouse.click("left")
  # KeyboardSimulator::mouse.move(coord.list[[where]][["code"]][1],coord.list[[where]][["code"]][2])
  # Sys.sleep(mouse.sleep)
  # KeyboardSimulator::mouse.click("left")
  # KeyboardSimulator::mouse.move(coord.list[[where]][["match"]][1],coord.list[[where]][["match"]][2])
  # Sys.sleep(mouse.sleep)
  # KeyboardSimulator::mouse.click("left")
  # KeyboardSimulator::mouse.move(coord.list[[where]][["app"]][1],coord.list[[where]][["app"]][2])
  # Sys.sleep(mouse.sleep)
  # KeyboardSimulator::mouse.click("left")
  # KeyboardSimulator::mouse.move(coord.list[[where]][["ok"]][1],coord.list[[where]][["ok"]][2])
  # Sys.sleep(mouse.sleep)
  # KeyboardSimulator::mouse.click("left")
  # }


Sys.sleep(1)

  cat("\nPasting text\n")
  split_list <-
    str_replace_all(text, c("\n(?=(\\s{2,20}))" = "xsplitxenterxsplitx",
                            '(?<=(cat\\((.|\n){0,5}"(.|\n){0,150}))\n(?=((.|\n){0,150}"(.|\n){0,5}\\))(?!(\\s{2,20})))' = "NEW",
                            "\n" = "xsplitxenterxsplitx",
                            "NEW" = "xsplitxNEWxsplitx",
                            "," = "xsplitxcommaxsplitx",
                            "\\\\" = "xsplitxdbackslashxsplitx",
                            "<" = "xsplitxarrowxsplitx",
                            "-" = "xsplitxlinexsplitx",
                            "≤" = "xsplitxlessxsplitx",
                            "_" = "xsplitxunsxsplitx",
                            ":" = "xsplitxcolonxsplitx",
                            ";" = "xsplitxsemcolxsplitx",
                            "~" = "xsplitxtildexsplitx",
                            "`" = "xsplitxffxsplitx")) %>% str_split(., "xsplitx")



  for(i in split_list[[1]]) {
    if(i == "enter") {
      KeyboardSimulator::keybd.press("enter")
    }  else if(i == "comma") {
      KeyboardSimulator::keybd.press("alt", hold = T)
      Sys.sleep(special.sleep)
      KeyboardSimulator::keybd.press("num4") #alt codes
      KeyboardSimulator::keybd.press("num4")
      KeyboardSimulator::keybd.release("alt")
      KeyboardSimulator::keybd.press(" ")
    }
    else if(i == "arrow") {
      KeyboardSimulator::keybd.press("alt", hold = T)
      Sys.sleep(special.sleep)
      KeyboardSimulator::keybd.press("num6")
      KeyboardSimulator::keybd.press("num0")
      KeyboardSimulator::keybd.release("alt")
    }
    else if(i == "line") {
      KeyboardSimulator::keybd.press("alt", hold = T)
      Sys.sleep(special.sleep)
      KeyboardSimulator::keybd.press("num4")
      KeyboardSimulator::keybd.press("num5")
      KeyboardSimulator::keybd.release("alt")
    }
    else if(i == "less") {
      KeyboardSimulator::keybd.press("alt", hold = T)
      Sys.sleep(special.sleep)
      KeyboardSimulator::keybd.press("num9")
      KeyboardSimulator::keybd.press("num2")
      KeyboardSimulator::keybd.release("alt")
      KeyboardSimulator::keybd.type_string("u2264 3")
    }
    else if(i == "uns") {
      KeyboardSimulator::keybd.press("alt", hold = T)
      Sys.sleep(special.sleep)
      KeyboardSimulator::keybd.press("num9")
      KeyboardSimulator::keybd.press("num5")
      KeyboardSimulator::keybd.release("alt")
    }
    else if(i == "dbackslash") {
      KeyboardSimulator::keybd.press("alt", hold = T)
      Sys.sleep(special.sleep)
      KeyboardSimulator::keybd.press("num9")
      KeyboardSimulator::keybd.press("num2")
      KeyboardSimulator::keybd.release("alt")
      KeyboardSimulator::keybd.press("alt", hold = T)
      Sys.sleep(special.sleep)
      KeyboardSimulator::keybd.press("num9")
      KeyboardSimulator::keybd.press("num2")
      KeyboardSimulator::keybd.release("alt")

    }
    else if(i == "colon"){
      KeyboardSimulator::keybd.press("alt", hold = T)
      Sys.sleep(special.sleep)
      KeyboardSimulator::keybd.press("num5")
      KeyboardSimulator::keybd.press("num8")
      KeyboardSimulator::keybd.release("alt")
    }
    else if(i == "semcol"){
      KeyboardSimulator::keybd.press("alt", hold = T)
      Sys.sleep(special.sleep)
      KeyboardSimulator::keybd.press("num5")
      KeyboardSimulator::keybd.press("num9")
      KeyboardSimulator::keybd.release("alt")
    }
    else if (i == "tilde"){
      KeyboardSimulator::keybd.press("alt", hold = T)
      Sys.sleep(special.sleep)
      KeyboardSimulator::keybd.press("num1")
      KeyboardSimulator::keybd.press("num2")
      KeyboardSimulator::keybd.press("num6")
      KeyboardSimulator::keybd.release("alt")
    }
    else if(i == "ff"){
      KeyboardSimulator::keybd.press("alt", hold = T)
      Sys.sleep(special.sleep)
      KeyboardSimulator::keybd.press("num9")
      KeyboardSimulator::keybd.press("num6")
      KeyboardSimulator::keybd.release("alt")
    }
    else if(i == "NEW"){
      KeyboardSimulator::keybd.press("alt", hold = T)
      KeyboardSimulator::keybd.press("num9")
      KeyboardSimulator::keybd.press("num2")
      KeyboardSimulator::keybd.release("alt")
      Sys.sleep(special.sleep)
      KeyboardSimulator::keybd.press("n")

    }
    else if(i == "") {
      next
    } else {
      KeyboardSimulator::keybd.type_string(str_trim(i, side = "both"))
    }
    Sys.sleep(write.sleep)
  }

  cat("\nPrinting Done\n")


}
