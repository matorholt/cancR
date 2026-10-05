# Load csv, excel, rds and parquet files

Wrapper for the fread, readxl, readRDS and read_parquet functions with
automatic detection of file extension.

## Usage

``` r
readR(path, leading.zeros = T, na = "", ...)
```

## Arguments

- path:

  path for the file to load.

- leading.zeros:

  whether leading zeros should be kept (default = T)

- na:

  character vector specifying NA strings (default = "")

- ...:

  arguments passes to subfunctions

## Value

the given path imported as a data frame
