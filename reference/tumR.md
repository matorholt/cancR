# Map unique tumors

Customized function for the Danish Pathology Register to map unique
tumors and outcomes taking reexcisions and changes in diagnosis into
account.

## Usage

``` r
tumR(
  data,
  tumor,
  loc.exact = F,
  pnr = pnr,
  date = date,
  dt = F,
  verbose = F,
  tumor_distance = c(-90, 365.25 * 5),
  meta_distance = 365.25 * 2,
  exclude = NULL,
  skin.only = F
)
```

## Arguments

- data:

  dataframe of data from the Danish National Pathology Register

- tumor:

  named list of tumors with vectors of snomed codes (e.g. list("pcc" =
  c("m0703", "m805"), "bcc" = "m809"))

- loc.exact:

  whether tumors should match on exact location to be considered linked

- dt:

  whether a data.table should be returned (default = F)

- verbose:

  whether detailed status for each patient should be printed (for
  debugging)

- tumor_distance:

  the interval where a metastasis is considered relevant

- meta_distance:

  the distance between metastases that classifies a cluster

- id:

  name of the main identifier, default = pnr

## Value

a data.frame of unique tumors with date, diagnosis and location
