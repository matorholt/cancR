# Format numeric vectors

Format numeric vectors

## Usage

``` r
numbR(numbers, digits = 1, nsmall, ama = F)
```

## Arguments

- numbers:

  numeric value or vector for formatting

- digits:

  number of digits

- nsmall:

  number of zero-digits

- ama:

  whether the numbers should be printed according to AMA guidelines (no
  digits on values \>= 10). Default = F.

## Value

returns a vector of same lentgh with formatted digits

## Examples

``` r
numbR(c(5,2,4,10,100, 41.2), ama=F)
#> [1] "  5.0" "  2.0" "  4.0" " 10.0" "100.0" " 41.2"
numbR(c(5,2,4,10,100, 41.2), ama=T)
#> [1] "  5.0" "  2.0" "  4.0" " 10"   "100"   " 41"  
```
