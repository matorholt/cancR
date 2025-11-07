# Generation of time-dependent CCI and comorbidities from the Charlson Comorbidity Index

Generation of time-dependent CCI and comorbidities from the Charlson
Comorbidity Index

## Usage

``` r
cci_timR(
  data,
  interval = 365.25/2,
  structure = "binned",
  format = "long",
  comorb = F
)
```

## Arguments

- data:

  Full LPR dataset

- interval:

  Interval in which the CCIs should be calculated

- structure:

  Whether CCI should be the full 18 parametres or binned into 1, 2-3,
  4-5 and 6+

- format:

  Whether the output should be in long or wide format

- comorb:

  Whether the specific comorbidites in the CCI score should be
  extracted. Default is FALSE.

## Value

cci_full_long/wide: Dates of all CCI scores. Repeted scores in adjacent
rows are removed as the status is not changed. cci_bin_long/wide: Same
as cci_all with CCI scores binned 1, 2-3, 4-5 and 6+ comorb_long/wide:
Dates of all comorbidites
