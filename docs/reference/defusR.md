# Defuse arguments to character string regardless of quoted or non-quoted in functions.

Defuse arguments to character string regardless of quoted or non-quoted
in functions.

## Usage

``` r
defusR(input, data = NULL)
```

## Arguments

- input:

  quoted or non-quoted vector (e.g. variable names)

- data:

  data frame if tidyselection is used.

## Value

the defused argument as a character string of class "def_ex" for later
detection
