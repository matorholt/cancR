# Find covariates or outcomes from the registers

Find covariates or outcomes from the registers

## Usage

``` r
searchR(
  reglist,
  search.list,
  name.list = NULL,
  sub.list = list(),
  sub.labels = NULL,
  exclusion.list = list(),
  slice = "first",
  format = "date",
  date.filter = NULL,
  match = "start",
  casename = "index",
  pnr = "pnr",
  cores = 4,
  dt = F
)
```

## Arguments

- reglist:

  list of dataframe(s)

- search.list:

  list with list(label = diagnosis code) structure.

- name.list:

  list of labels for the main columns (e.g. list("case" = "diabetes))

- sub.list:

  list of variables where the diagnosis code should be kept (e.g.
  list("KOL" = "subtype"))

- sub.labels:

  list of labels for the diagnosis code colum (e.g. list("subtype" =
  list("a" = c("DC1", "DC2"))))

- exclusion.list:

  list with same structure as search.list for exclusion codes

- slice:

  which rows should be selected (first(default)/last/all)

- format:

  whether selected rows should contain date or 1

- match:

  the match of the regex code (match, start, end or contains(default))

- casename:

  the name of the case variable (default = "index")

- pnr:

  name of the pnr column

## Value

A dataframe with the selected columns with corresponding diagnosis codes
