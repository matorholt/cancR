# Load registers

Simple loading function of the most used registers including variable
selection, regex filtering and id_list filtering.

## Usage

``` r
loadR(
  regs,
  pattern.list = NULL,
  pattern.custom = NULL,
  n = NULL,
  id.filter = NULL,
  keep.list = NULL,
  vars.list = NULL,
  lmdb.start = 1995,
  lmdb.stop = 2023,
  simulation = F,
  cores = 4,
  dt = F,
  gb = NULL,
  cancR.covariates = "main",
  ...
)
```

## Arguments

- regs:

  which registers should be loaded. Default is all (lpr, pop, pato,
  cancer, lmdb and opr)

- pattern.list:

  named list of vectors of diagnoses codes for each register in the
  format ("lpr" = c("DC92", "DC21")). If multiple columns should be
  searched, an extra list layer is added ("lpr" = list("diag" =
  c("DC1"), "tildiag" = "DC2"))

- pattern.custom:

  named list for custom filter expressions

- n:

  number of observations that should be loaded

- id.filter:

  optional possibility to limit the registers to a defined patient
  population of PNRs

- keep.list:

  which variables should be kept provided as a list("lpr" = c("vars")).

- vars.list:

  which columns should the pattern filter be applied to. Defaults to
  diag, atc, opr and c_morfo3,

- lmdb.start:

  first year of LMDB

- lmdb.stop:

  last year of LMDB

- simulation:

  whether the registers should be simulated

- cores:

  number of cores for parallel processing

- dt:

  whether the returned data should be in data.table format

- gb:

  max size for future options

- cancR.covariates:

  which covariates that should be loaded. Options are: main (non-major),
  major (only major) and all.

- ...:

  arguments passed to simulatR()

## Value

Returns the selected registers to the global environment
