# Generate all possible cominations/permutations

All possible combinations of a single vector (ABC: ABC, ACB, BAC, BCA,
CAB, CBA) or two separate vectors (AB, CD: ABCD, CDAB) The function does
not allow for replacements.

## Usage

``` r
combinR(letters, letters2 = NULL, list = F)
```

## Arguments

- letters:

  Vector either of length 1 (will be split for each subelement) og
  length \>1

- letters2:

  Optional second vector if chunks are to be combined

- list:

  Whether all combinations should be returned as a list (useful for
  looping with lapply)

## Value

Returns all possible combinations of the input vector(s)
