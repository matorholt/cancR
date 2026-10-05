# Assessment of distribution of continuous variables with histograms, QQ-plots and the Shapiro-Wilks test

Assessment of distribution of continuous variables with histograms,
QQ-plots and the Shapiro-Wilks test

## Usage

``` r
distributR(data, vars, bins = 1, test = "shapiro")
```

## Arguments

- data:

  dataframe

- vars:

  variables to test. If not specified all numeric variables with more
  than 5 unique values are assessed

- bins:

  binwidth

- test:

  whether the Shapiro-Wilks (default) or Kolmogorov-Smirnov test should
  be performed

## Value

Combined plotframe of histograms, QQ-plots and the Shapiro-Wilks tests
