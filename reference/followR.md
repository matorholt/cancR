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
followR(analysis_df, time2, event, group = X2)
#>     group    years    lower    upper
#> 1 Overall 8.355165 5.263687 11.87811
#> 2       1 8.421084 5.305933 11.87811
#> 3       2 8.272921 5.212956 11.94247
```
