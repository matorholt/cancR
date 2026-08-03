#' Automated keyboard strokes
#'
#' @param text single-quote enclosed text
#' @param path path to r-script
#' @param start.delay delay (secs) before automated typing (default = 5 seconds)
#' @param chunk.delay delay (secs) between blocks of text (default = 0 seconds)
#' @param chunks chunk length of the text in number of lines (default = 10)
#' @param mouse.sleep delay between mouse actions (default = 0.15 secs)
#' @param screens number of screens that is being used
#' @param automate whether automatic parenthesis and code indention should be disabled (default = T)
#' @param debug whether the code output should be printed to the console instead of automated
#'
#' @details
#' If the command is aborted abruptly, the keyboard can malfunction due to pressed alt. Release with shift-alt to toggle back to danish
#'
#'
#' @return a keyboard and mouse automation that automatically types the assigned text
#' @export
#'
#'

powR <- function(text = NULL,
                 path = NULL,
                 trim.start = 0,
                 trim.end = 0,
                 start.delay = 3,
                 chunk_delay = 0,
                 chunks = 20,
                 mouse.sleep = 0.5,
                 screens = 2,
                 automate = TRUE,
                 debug = FALSE) {

  cli::cli_h2("Initializing powR algorithm: {tickR(cli = F)}")

  if(debug) {
    start.delay <- 0
    if(missing(automate)) automate <- F
  }

  #load text
  if (!is.null(path)) {
    if (!file.exists(path)) return({cli::cli_alert_danger("Error: File not found"); invisible(NULL)})
    text <- readLines(path, warn = FALSE)
    text <- str_replace_all(text, "\\\\\\\\", "\\\\")  # Escape \\
  } else {
    text <- str_split(text, "\n")[[1]]
  }
  if(is.null(text)) return({cli::cli_alert_danger("Error: No path or text provided"); invisible(NULL)})

  if(any(c(trim.start, trim.end) %nin% 0)) text <- text[c((1+trim.start):(length(text)-trim.end))]

  #coordinates
  coord.list <- list(c(214, 290), c(2700, 250))

  #Custom toggle function
  toggle_option <- function(search_term) {
    KeyboardSimulator::mouse.move(coord.list[[screens]][1], coord.list[[screens]][2])
    Sys.sleep(mouse.sleep)
    KeyboardSimulator::mouse.click("left")
    Sys.sleep(0.5)
    KeyboardSimulator::keybd.press("ctrl",  hold = TRUE)
    KeyboardSimulator::keybd.press("shift", hold = TRUE)
    KeyboardSimulator::keybd.press("p",     hold = TRUE)
    KeyboardSimulator::keybd.release("ctrl")
    KeyboardSimulator::keybd.release("shift")
    KeyboardSimulator::keybd.release("p")
    Sys.sleep(0.5)
    KeyboardSimulator::keybd.type_string(search_term)
    Sys.sleep(0.5)
    KeyboardSimulator::keybd.press("enter")
    Sys.sleep(0.2)
    KeyboardSimulator::keybd.press("esc")
  }

  #Original cursor position
  current <- KeyboardSimulator::mouse.get_cursor()

  #Restore on.exit
  on.exit({

    if (automate) {
      cli::cli_alert_info("Restoring options")
      toggle_option("parentheses and")
    }

    # Close laptop window if needed
    if (screens == 1 & automate) {
      KeyboardSimulator::mouse.move(1485, 13)
      Sys.sleep(mouse.sleep)
      KeyboardSimulator::mouse.click("left")
      Sys.sleep(mouse.sleep)
    }

    # Restore cursor position
    KeyboardSimulator::mouse.move(current[1], current[2])

    cli::cli_alert_success("Typing complete!")
    cli::cli_text("Total runtime: {tockR(\'diff\', cli=F)}")

  }, add = TRUE)

  #Countdown
  if (start.delay > 0) {
    for (i in start.delay:0) {
      cli::cli_progress_message("Typing in: {i}")
      Sys.sleep(1)
    }
    cli::cli_progress_done()
  }

  #Open remote server
  if (screens == 1 & automate) {
    KeyboardSimulator::mouse.move(602, 1577)
    Sys.sleep(mouse.sleep)
    KeyboardSimulator::mouse.click("left")
    Sys.sleep(mouse.sleep)
  }

  #Toggle options
  if (automate) {
    cli::cli_alert_info("Disabling RStudio options")
    toggle_option("parentheses and")
  }

  #Custom escape function
  escape_line <- function(line) {

    # Strip leading whitespace — RStudio auto-indent handles indentation
    line <- str_trim(line, side = "left")

    # Replace quotes with placeholders before SendKeys escaping
    line_esc <- str_replace_all(line, c('"' = "DQUOTE",
                                        "'" = "SQUOTE"))

    # SendKeys escaping
    line_esc <- str_replace_all(line_esc, c(
      "\\{"   = "x{x",        # temporarily mask { to avoid SendKeys syntax conflict
      "\\}"   = "x}x",        # temporarily mask }
      "\\\\"  = "\\\\\\\\",   # escape backslashes
      "\\("   = "{\\(}",      # ( is special in SendKeys (group modifier)
      "\\)"   = "{\\)}",      # ) is special in SendKeys
      "\\+"   = "{\\+}",      # + is special in SendKeys (Shift modifier)
      "\\%"   = "{\\%}",      # % is special in SendKeys (Alt modifier)
      "\\~"   = "{\\~}",      # ~ is special in SendKeys (Enter)
      "\\^"   = "+¨",         # ^ on Danish keyboard is typed as +¨
      "x\\{x" = "{{}",        # restore { as SendKeys literal
      "x\\}x" = "{}}",        # restore } as SendKeys literal
      "\\s+"  = " "           # normalise multiple spaces to single space
    ))

    #Replace double and single quotes
    line_esc <- str_replace_all(line_esc, c(
      "DQUOTE" = "'+[char]34+'",
      "SQUOTE" = "'+[char]39+'"
    ))

    # Strip empty string artifacts at boundaries
    line_esc <- str_remove(line_esc,  "^'\\+")
    line_esc <- str_replace(line_esc, "(\\[char\\]\\d+)\\+'$", "\\1")
    line_esc <- str_replace(line_esc, "(\\[char\\]\\d+)'$",    "\\1")
    line_esc <- str_remove(line_esc,  "\\+'$")

    # Ensure valid PowerShell string
    if (!str_starts(line_esc, "'") && !str_detect(line_esc, "\\[char\\]")) {
      line_esc <- paste0("'", line_esc, "'")  # no [char] — wrap fully
    } else if (!str_starts(line_esc, "'") && !str_starts(line_esc, fixed("[char]"))) {
      line_esc <- paste0("'", line_esc)        # has [char] — opening quote only
    }
    if (!str_ends(line_esc, "'") && !str_detect(line_esc, "\\[char\\]\\d+$")) {
      line_esc <- paste0(line_esc, "'")        # add closing quote if needed
    }

    line_esc
  }

  ########
  #TYPING#
  ########
cli::cli_text("Typing {length(text)} lines...")

  chunk_groups <- split(text, ceiling(seq_along(text) / chunks))

  cli::cli_progress_bar(
    format = "{cli::pb_spin} ({round((cli::pb_current/cli::pb_total)*100,0)}%)",
    total = length(chunk_groups),
    clear = T
  )

  for (i in seq_along(chunk_groups)) {
    group <- chunk_groups[[i]]
    escaped_lines <- sapply(group, escape_line)
    full_escaped <- paste(escaped_lines, collapse = "+'+{ENTER}'+")
    if (debug) {
      cli::cli_text("CHUNK {i} of {length(chunk_groups)}")
      for (j in seq_along(group)) {
        cli::cli_text("Line:    {group[j]}")
        cli::cli_text("Escaped: {escaped_lines[j]}")
      }
    } else {
      system2("powershell", args = c("-Command", paste0(
        "Add-Type -AssemblyName System.Windows.Forms; ",
        "[System.Windows.Forms.SendKeys]::SendWait(", full_escaped, "+'+{ENTER}')"
      )))
    }
    Sys.sleep(chunk_delay)
    cli::cli_progress_update()
  }

  cli::cli_progress_done()
}
