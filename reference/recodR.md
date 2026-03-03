# Recode multiple variables

Recode multiple variables

## Usage

``` r
recodR(data, namelist, match = "exact", replace = F)
```

## Arguments

- data:

  dataframe

- namelist:

  list og variables with named vector(s) containing diagnosis codes
  (e.g. list(var1 = list(name = diagnosis codes)))

- match:

  Whether the provided diagnosis codes should be matched exactly, start
  with/end with, be bounded by or contain (default)

- replace:

  whether multiple matches should be replaced directly (such as 1,4 to
  head, arm)

## Value

the input data frame with recoded variables

## Examples

``` r
            df %>%
  recodR(list("diag" = list("KOL" = "DX123",
                            "Astma" = c("DC123", "DC2"),
                            "AMI" = c("DG123", "DG234"))),
         match = "exact")
#> Error in setDT(data): Argument 'x' to 'setDT' should be a 'list', 'data.frame' or 'data.table'
df %>%
  factR(c(diag, type, type2)) %>%
  recodR(list("diag" = list("KOL" = "DX123",
                            "Astma" = c("DC123", "DC234"),
                            "AMI" = list("DG123", "DG234"))),
         match = "exact")
#> Error in UseMethod("select"): no applicable method for 'select' applied to an object of class "function"

df %>%
  recodR(list("split" = list("one" = "1",
                             "four" = "4",
                             "ten" = "10",
                             "fifteen" = "15")),
         match = "boundary")
#> Error in setDT(data): Argument 'x' to 'setDT' should be a 'list', 'data.frame' or 'data.table'

df %>%
  recodR(list("split" = list("one" = "1",
                             "two" = "2",
                            "eleven" = "11",
                             "four" = "4",
                             "ten" = "10",
                             "fifteen" = "15")),
         match = "boundary",
         replace=T)
#> Error in setDT(data): Argument 'x' to 'setDT' should be a 'list', 'data.frame' or 'data.table'
```
