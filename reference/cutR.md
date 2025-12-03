# Cut numeric variables into intervals

Wrapper for cut() with simplified labels and automatic conversion to
factor based on most frequent levels

## Usage

``` r
cutR(data, vars, seqlist, name.list = list(), name.pattern = NULL, digits = 0)
```

## Arguments

- data:

  dataframe. Works with piping.

- vars:

  Vector of vars which should be cut

- seqlist:

  List of the split patterns for each var. E.g. list("v1" = seq(0,10),
  "v2" = round(quantile(df %\>% pull(age), seq(0,1,0.1)),0))

- name.list:

  Whether the split should overwrite or as a new variable

- name.pattern:

  Optional naming pattern that should automatically be pasted on the end
  of the variable name.

## Value

The inputted dataframe with the cut variables

## Examples

``` r
redcap_df %>%
  cutR(size, seq(0,100,10)) %>%
  str
#> [1] "size"
#>  [1]   0  10  20  30  40  50  60  70  80  90 100
#> 'data.frame':    500 obs. of  17 variables:
#>  $ id             : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ sex            : num  1 2 1 1 2 1 1 1 2 2 ...
#>  $ age            : num  79.1 38.1 60.6 45.2 39.5 81.2 74.9 38.8 31.8 51.3 ...
#>  $ birth          : chr  "20-09-1929" "12-10-1953" "11-05-1948" "22-04-1949" ...
#>  $ followup       : chr  "03-04-2023" "18-12-2025" "21-09-2025" "11-02-2022" ...
#>  $ date_of_surgery: chr  "2008-11-03" "1991-11-13" "2008-12-17" "1994-07-17" ...
#>  $ size           : Factor w/ 6 levels "10-20","40-50",..: 4 3 2 3 3 2 2 4 1 3 ...
#>  $ type           : int  1 1 1 1 2 2 0 0 0 2 ...
#>  $ localisation   : int  3 1 2 3 1 3 2 5 3 1 ...
#>  $ necrosis       : num  NA NA 0 1 NA 1 0 1 NA NA ...
#>  $ margins        : chr  "0" "0" "1" "0" ...
#>  $ cd10           : num  1 0 NA 0 0 1 0 0 1 1 ...
#>  $ sox10          : num  1 0 NA 0 NA 1 1 0 1 1 ...
#>  $ ck             : num  1 NA 0 0 0 0 0 NA 0 1 ...
#>  $ death_date     : chr  NA NA "2017-09-02" "2018-10-04" ...
#>  $ recurrence_date: chr  "2015-03-22" NA NA "2011-12-31" ...
#>  $ metastasis_date: chr  NA NA NA NA ...
```
