# Calculate median follow-up time using the inverse Kaplan-Meier method

Calculate median follow-up time using the inverse Kaplan-Meier method

## Usage

``` r
followR(df, timevar, event, group, time.unit = "m2y")
```

## Arguments

- df:

  Input dataframe

- timevar:

  Time to last follow-up

- event:

  Status indicator (censored=0 or dead=1)

- group:

  Optional group indicator if follow-up time should be reported
  separately

## Value

Returns the median follow-up time with IQR for the whole sample or
groupwise

## Examples

``` r
followR(analysis_df, time2, event, group = X2, time.unit = "months")
#>     group    months    lower    upper
#> 1 Overall 100.26199 63.16424 142.5373
#> 2       1 101.05301 63.67120 142.5373
#> 3       2  99.27505 62.55548 143.3097
```
