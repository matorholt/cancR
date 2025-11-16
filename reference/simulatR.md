# Simulate danish health registers

A wrapper for the simulation functions in the heaven package.

## Usage

``` r
simulatR(
  register,
  n = 10,
  start.date = "2000-01-01",
  pattern.list = list(),
  lpr.diag.count = 5,
  lmdb.max.prescriptions = 20,
  lmdb.max.packages = 3,
  opr.diag.count = 5,
  pop.min.age = 20,
  pop.max.age = 100,
  pop.sex = 0.5,
  pop.mortality = 0.1,
  match.cases,
  match.birth = "1960-01-01",
  match.index = c("1990-01-01", "2010-01-01"),
  covariates.period = c("1980-01-01", "2025-01-01"),
  seed = 1
)
```

## Arguments

- register:

  vector of the registers to simulate. Choose between "lpr", "lmdb",
  "opr", "pop", "pato", "match" and "covariates".

- n:

  number of unique pnrs to simulate

- start.date:

  starting date of the register

- pattern.list:

  list of vectors of diagnoses codes for each register in the format
  ("lpr" = c("DC92", "DC21"))

- lpr.diag.count:

  maximum number of diagnoses per patient in LPR

- lmdb.max.prescriptions:

  maximum number of prescriptions per patient in LMDB

- lmdb.max.packages:

  maximum number of packages purchased per day

- opr.diag.count:

  maximum number of diagnoses per patient in OPR

- pop.min.age:

  minmmum age in the population

- pop.max.age:

  maximum age in the population

- pop.sex:

  distribution of sex in the population, default = 0.5

- pop.mortality:

  mortality rate in the population, default = 0.1

- match.cases:

  number of cases for the "match" dataset

- match.birth:

  maximum birth date. Cohorts are established backwards based on n
  assuming 100.000 per cohort

- match.index:

  vector of length 2 with range of dates for indices

- covariates.period:

  vector of length 2 withrange of dates for covariate status

- seed:

  for reproducibility

## Value

a single data frame or named list of data frames with simulated
registers
