# Load csv, excel and rds files

Wrapper for the fread, readxl and readRDS functions with automatic
detection of file extension

## Usage

``` r
readR(path, leading.zeros = T, na = "", ...)
```

## Arguments

- path:

  path for the file to load

- ...:

  arguments passes to subfunctions

## Value

the given path imported as a data frame
