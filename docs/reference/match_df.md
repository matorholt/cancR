# Simulated dataset for the matchR algorithm

Simulation of a population data frame before using the matchR function

## Usage

``` r
match_df
```

## Format

A data frame with 40,100 rows and 11 columns:

- pnr:

  unique patient pnr

- case:

  whether case = 1 or control = 0

- index:

  index date for cases. NA for controls

- follow:

  date of last follow-up which can be death, emigration or end of
  database

- birth:

  date of birth

- sex:

  patient sex, f = female, m = male

- skinc:

  date of first skin cancer occurence

- imm_sup:

  index of first diagnosis of immunosuppression

- random1, random2:

  random variables
