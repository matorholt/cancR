# Map unique tumors

Customized function for the Danish Pathology Register to map unique
tumors taking reexcisions and changes in diagnosis into account.

## Usage

``` r
tumR(data, tumor, loc.exact = F, cores = NULL, dt = F)
```

## Arguments

- data:

  dataframe of data from the Danish National Pathology Register

- tumor:

  character vector containing prefixes for tumors of interest (e.g.
  m807)

- loc.exact:

  whether tumors should match on exact location to be considered linked

- cores:

  number of cores for parallel processing

- dt:

  whether a data.table should be returned (default = F)

## Value

a data.frame of unique tumors with date, diagnosis and location
