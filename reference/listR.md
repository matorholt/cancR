# Routine modifications of lists

Routine modification of lists such as reverse values and names for use
in other functions. The functions is also a wrapper for the rrapply and
map_depth functions with the dots argument.

## Usage

``` r
listR(input, type, ...)
```

## Arguments

- input:

  the list or vector

- type:

  type of modification of the list. See details

- ...:

  arguments for the rrapply function

## Value

returns a modified list based on the "type" argument

## Details

"reverse" reverses the values and names of a list which is used as
inputs in functions such as str_replace. "vec2list" converts a vector to
a list with names corresponding to the vector elements

## Examples

``` r
reverse_list <- list("first" = "a1",
                     "second" = "b2")

listR(reverse_list, type = "reverse")
#>       a1       b2 
#>  "first" "second" 
```
