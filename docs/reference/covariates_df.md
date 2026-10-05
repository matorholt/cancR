# Simulated time-dependent covariates dataset.

Simulation of a time-dependent data frame with covariates from 4000
patients for the matchR function

## Usage

``` r
covariates_df
```

## Format

A data frame with 84858 rows and 22 columns:

- pnr:

  unique patient pnr

- from:

  start of the covariate window

- to:

  end of the covariate window

- cci:

  Charlson Comorbidity Index binned into the groups: 0, 1, 2-3, 4-5 and
  6+

- education:

  Educational level according to ISCED groups: low, medium and high

- income:

  Quantile of the equivalated disposable income: q1, q2, q3 and q4

- marital:

  Marital status: Unmarried, married and divorced

- region:

  Region of habitance: Capital, central, north, south and zealand

- degurba:

  Degree of urbanization in the municipality: city, suburb and rural

- infection, cancer, hema, endo, psych, neuro, cvd, lungs, gi, skin,
  connective, urinary, congenital:

  ICD main groups of covariates: 0 and 1
