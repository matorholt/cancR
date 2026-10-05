# Convert dates from character to date format

Convert dates easily without specifying format. The format is identified
automatically and converted to standard Year-month-day.

## Usage

``` r
datR(data, vars = contains(c("date", "dato")), HMS = F, dt = FALSE)
```

## Arguments

- data:

  data frame or vector of dates

- vars:

  character vector for specifying variables to convert to date format.
  Default is all columns containing "date\|dato"

- HMS:

  whether hours, minutes and seconds should be kept (default = F)

- dt:

  whether a data.table should be returned

## Value

the input data frame with correctly formatted date variables

## Examples

``` r


datR(c("2001-02-01", "03-02-2002", 12345))
#> [1] "2001-02-01" "3-02-20"    NA          

datR(1234)
#> [1] "1973-05-19"
```
