# Load registers

Simple loading function of the most used registers including variable
selection, regex filtering and id_list filtering.

## Usage

``` r
loadR(
  regs,
  pattern.list = NULL,
  pattern.list2 = NULL,
  n = NULL,
  id.filter = NULL,
  keep.list = NULL,
  vars.list = NULL,
  lmdb.start = 1995,
  lmdb.stop = 2023,
  simulation = F,
  ...
)
```

## Arguments

- regs:

  which registers should be loaded. Default is all (lpr, pop, pato,
  cancer, lmdb and opr)

- pattern.list:

  list of vectors of diagnoses codes for each register in the format
  ("lpr" = c("DC92", "DC21"))

- pattern.list2:

  supplemental pattern if multiple columns should be filtered. Works the
  same way as pattern

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

- ...:

  arguments passed to simulatR()

## Value

Returns the selected registers to the global environment
