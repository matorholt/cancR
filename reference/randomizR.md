# Generate stratified, block randomized allocation sequence

Stratified block randomization sequence with optional redcap API. Each
stratum will have its own unique allocation sequence.

## Usage

``` r
randomizR(
  strata,
  block.size,
  block.probabilities = NULL,
  replications = 50,
  allocation.levels = c("1", "2"),
  allocation.name = "allocation",
  block.delay = 10,
  token,
  seed = 1,
  print = F
)
```

## Arguments

- strata:

  named list of strata levels (e.g. list("age" = c(1,2), "sex" =
  c("f","m"))). If missing, an API token can be provided (see "token")

- block.size:

  vector of block size(s).

- block.probabilities:

  vector of block size probabilities

- replications:

  length of the allocation sequence in each stratum

- allocation.levels:

  vector of the allocated treatment levels, default = c("1", "2")

- allocation.name:

  column name for the allocated treatment levels, default = "allocation"

- block.delay:

  number of blocks that is fixed to the first block.size to avoid large
  blocks in the beginning of the sequence

- token:

  token for redcap API to automatically extract strata levels from
  redcap. If strata is a vector of variables, token must be provided.

- seed:

  seed for reproducibility

- print:

  whether a full report for each stratum should be printed

## Value

a data frame containing rows equal to the product of unique strata and
replications.
