# Routine modifications of lists

Routine modification of lists such as reverse values and names for use
in other functions. The functions is also a wrapper for the rrapply and
map_depth functions with the dots argument.

## Usage

``` r
listR(input, type, layer, chunks, collapse = F, ...)
```

## Arguments

- input:

  the list or vector

- type:

  type of modification of the list. See details

- layer:

  vector of integers indicating which layer to remove if type is "peel"
  or to keep if type is "pick". Layer 1 is the top layer

- chunks:

  specifies the length of chunks for type = "chunk_inner" or
  "chunk_outer"

- collapse:

  whether duplicated elements should be collapsed (defualt = F)

- ...:

  arguments for the rrapply function

## Value

returns a modified list based on the "type" argument

## Details

"reverse" reverses the values and names of a list which is used as
inputs in functions such as str_replace. "vec2list" converts a vector to
a list with names corresponding to the vector elements "peel" and "pick"
depends on "layer" and either drops or keeps the specified vector.
"chunk_inner" chunks a list into elements each of length = "chunks".
"chunk_outer" chunks a list into length = "chunks"

## Examples

``` r

reverse_list <- list("first" = "a1",
                     "second" = "b2")

listR(reverse_list, type = "reverse")
#> $a1
#> [1] "first"
#> 
#> $b2
#> [1] "second"
#> 
```
