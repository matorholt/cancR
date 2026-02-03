# Load csv, excel, rds and parquet files

Wrapper for the fread, readxl, readRDS and read_parquet functions with
automatic detection of file extension.

## Usage

``` r
readR(path, extension, leading.zeros = T, na = "", ...)
```

## Arguments

- path:

  path for the file to load.

- ...:

  arguments passes to subfunctions

## Value

the given path imported as a data frame
