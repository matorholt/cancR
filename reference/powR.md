# Automated keyboard strokes

Automated keyboard strokes

## Usage

``` r
powR(
  text = NULL,
  path = NULL,
  trim.start = 0,
  trim.end = 0,
  start.delay = 3,
  chunk_delay = 0,
  chunks = 20,
  mouse.sleep = 0.5,
  screens = 2,
  automate = TRUE,
  debug = FALSE
)
```

## Arguments

- text:

  single-quote enclosed text

- path:

  path to r-script

- start.delay:

  delay (secs) before automated typing (default = 5 seconds)

- chunks:

  chunk length of the text in number of lines (default = 10)

- mouse.sleep:

  delay between mouse actions (default = 0.15 secs)

- screens:

  number of screens that is being used

- automate:

  whether automatic parenthesis and code indention should be disabled
  (default = T)

- debug:

  whether the code output should be printed to the console instead of
  automated

- chunk.delay:

  delay (secs) between blocks of text (default = 0 seconds)

## Value

a keyboard and mouse automation that automatically types the assigned
text

## Details

If the command is aborted abruptly, the keyboard can malfunction due to
pressed alt. Release with shift-alt to toggle back to danish
