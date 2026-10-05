# Get the mode (most common value) of a vector.

Get the mode (most common value) of a vector.

## Usage

``` r
modeR(x, ties = "first", na.rm = T)
```

## Arguments

- x:

  Vector of values

- ties:

  If two or more values are equally common, which should be chosen.
  Default is first.

- na.rm:

  Whether NAs should be removed, defaults to TRUE

## Value

The most common value of the vector excluding NAs
