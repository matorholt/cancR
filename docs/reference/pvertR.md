# Format p-values to AMA manual of style

Format p-values to AMA manual of style

## Usage

``` r
pvertR(pval, na = "NA", drop.p = F, drop.zero = F, trim = F, style = "ama")
```

## Arguments

- na:

  the print of NA values, default = "NA.

- drop.p:

  whether "p = " should be printed (default = F)

- drop.zero:

  whether the leading zero should be printed (default = F)

- trim:

  whether white spaces should be removed (default = F)

- style:

  style of the formatting (default = "ama")

- x:

  vector of p-values

## Value

Prints the raw p-value according to AMA manual of style
