# Directly standardized incidence rates using the WHO standard population

Directly standardized incidence rates inspired by the dsr package by
Matt Kumar. The incidence rates are standardized on 5-year age intervals
and sex for each year in the period 1990-2025 based on population tables
of the Danish population. Lastly the incidence rates are weighted based
on the WHO world population.

## Usage

``` r
incidencR(
  data,
  group,
  strata = list(c("year")),
  unit = 1e+05,
  ci.method = "normal",
  index,
  age = age,
  sex = sex
)
```

## Arguments

- data:

  data set containing age, sex and index date and group

- group:

  optional if incidence rates should be provided per group

- strata:

  list of vectors for which strata the incidence rates should be
  reported (e.g. per age-group and sex)

- ci.method:

  the method for derivation of confidence intervals. Default is
  "normal". If negative CIs are reported, use "lognormal"

- index:

  variable name of the index data

- age:

  name of the age variable

- sex:

  name of the sex variable

- pyears:

  the unit of the incidence rate. Default is 100.000 person years

## Value

a standardized incidence rate of the overall population and, if
specified in the strata-argument, stratified incidence rates
