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
#> 'data.frame':    500 obs. of  16 variables:
#>  $ id             : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ sex            : num  1 2 2 2 2 2 2 1 1 2 ...
#>  $ age            : num  40.9 57.8 32.2 58.7 29.3 18.3 52.7 36.2 42.6 55.8 ...
#>  $ birth          : chr  "26-04-1958" "18-01-1941" "30-07-1963" "31-12-1936" ...
#>  $ followup       : chr  "23-04-2021" "12-08-2024" "20-02-2022" "29-06-2024" ...
#>  $ date_of_surgery: chr  "1999-03-29" "1998-10-20" "1995-10-26" "1995-09-14" ...
#>  $ size           : Factor w/ 6 levels "40-50","20-30",..: 5 1 4 4 5 1 1 1 5 3 ...
#>  $ type           : int  0 0 0 2 0 2 0 2 2 1 ...
#>  $ localisation   : int  2 1 4 1 3 0 3 5 4 1 ...
#>  $ necrosis       : num  0 1 0 1 0 NA 1 NA 0 0 ...
#>  $ cd10           : num  NA 0 0 1 0 NA 1 1 1 NA ...
#>  $ sox10          : num  1 0 1 NA 0 NA 0 0 0 1 ...
#>  $ ck             : num  0 0 NA 0 0 0 NA 0 0 1 ...
#>  $ death_date     : chr  NA NA NA "2019-07-27" ...
#>  $ recurrence_date: chr  NA "2014-12-09" NA NA ...
#>  $ metastasis_date: chr  NA NA NA "2012-04-04" ...
```
