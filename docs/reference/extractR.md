# Extraction of key results from the estimatR function

Extraction of key results from the estimatR function

## Usage

``` r
extractR(
  list,
  outcome = c("counts", "risks", "diff"),
  format = "long",
  ci = "95%CI ",
  nsep = " / ",
  sep = " to ",
  total.count = T,
  labs = NULL,
  headings = NULL,
  censur = F,
  risk.digits = 1,
  diff.digits = 1,
  ratio.digits = 1,
  sep.ci = F,
  flextable = F,
  reverse = T
)
```

## Arguments

- list:

  estimatR object

- outcome:

  Which outcome measures to provide (counts, risks, differneces and/or
  ratios)

- format:

  Format of the table (long or wide).

- ci:

  The format of the confidence intervals (default = 95%CI)

- nsep:

  The separator between event counts (default = /)

- sep:

  The separator between all other ranges (default = to)

- censur:

  Whether counts \<= 3 should be censured (default = FALSE)

- risk.digits:

  number of digits on risk estimates

- diff.digits:

  number of digits on risk differences

- ratio.digits:

  number of digits on risk ratios

## Value

Returns a data frame compatible with flextable
