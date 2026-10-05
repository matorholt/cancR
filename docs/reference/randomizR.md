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
  allocation.name = "redcap_randomization_group",
  block.delay = 10,
  token,
  seed = "none",
  report = T,
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

  seed for reproducibility. If "none", no seed is set

- report:

  whether the allocation report should be made

- print:

  whether a full report for each stratum should be printed

## Value

a data frame containing rows equal to the product of unique strata and
replications.

## Examples

``` r
allocation <-
  randomizR(strata=c("age_group_depth",
                     "gender_depth",
                     "ana_site_depth",
                     "immuno_depth",
                     "seq_width_depth",
                     "seq_slnb_depth"),
            block.size = c(2,4),
            block.probabilities = c(0.9,0.1),
            replications = 50,
            block.delay = 10,
            token = '859A8159EB61A5AD6ABB8DE0BC78E67F',
            seed = 2,
            report = T,
            print=T)
#> Stratified block randomization complete
#> 
#> Stratified on 6 strata with the levels:
#> $age_group_depth
#> [1] "1" "2"
#> 
#> $gender_depth
#> [1] "1" "2"
#> 
#> $ana_site_depth
#> [1] "1" "2"
#> 
#> $immuno_depth
#> [1] "1" "2"
#> 
#> $seq_width_depth
#> [1] "1" "2" "3"
#> 
#> $seq_slnb_depth
#> [1] "1" "2" "3"
#> 
#> Number of unique strata: 144
#> 
#> Strata balances:
#> # A tibble: 144 × 9
#>    Stratum     Replications Allocation.ratio Block.Count Block.Sizes   `2`   `4`
#>    <chr>              <int> <chr>                  <int> <chr>       <int> <int>
#>  1 1 1 1 1 1 1           50 25/25                     24 2,4            23     1
#>  2 2 1 1 1 1 1           50 25/25                     21 2,4            17     4
#>  3 1 2 1 1 1 1           50 25/25                     22 2,4            19     3
#>  4 2 2 1 1 1 1           50 25/25                     21 2,4            17     4
#>  5 1 1 2 1 1 1           50 25/25                     23 2,4            21     2
#>  6 2 1 2 1 1 1           50 25/25                     23 2,4            21     2
#>  7 1 2 2 1 1 1           50 25/25                     22 2,4            19     3
#>  8 2 2 2 1 1 1           50 25/25                     24 2,4            23     1
#>  9 1 1 1 2 1 1           50 25/25                     23 2,4            21     2
#> 10 2 1 1 2 1 1           50 25/25                     23 2,4            21     2
#> # ℹ 134 more rows
#> # ℹ 2 more variables: `2(%)` <dbl>, `4(%)` <dbl>
#> Error in map(which(str_detect(names(strata_report), "%")), function(i) {    label <- names(strata_report)[i]    vec <- report_strata[[label]]    data.frame(block = label, min = min(vec), q1 = quantile(vec,         0.25), median = median(vec), q3 = quantile(vec, 0.75),         max = max(vec))}): ℹ In index: 1.
#> Caused by error in `.f()`:
#> ! object 'report_strata' not found
```
