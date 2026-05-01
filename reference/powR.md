# Automated keyboard strokes

Automated keyboard strokes

## Usage

``` r
powR(
  text,
  write.sleep = 0.02,
  mouse.sleep = 0.5,
  special.sleep = 0.05,
  where = "pc",
  server.language = T,
  server.options = T,
  toggle.options = T
)
```

## Arguments

- text:

  single-quote enclosed text to automate

- write.sleep:

  delay between text chunks (default = 0.025 secs)

- mouse.sleep:

  delay between mouse actions (default = 0.15 secs)

- special.sleep:

  delay betweem alt-hold and alt code (default = 0.1)

- where:

  enviroment, pc, laptop or iw containing customised coordinates

- server.language:

  whether the language on the server should be changed (default = T)

- server.automate:

  whether automatic parenthesis and quotes should be disabled (default =
  T)

## Value

a keyboard and mouse automation that transfers the assigned text
