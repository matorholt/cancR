# Routine modifications of lists

Routine modification of lists such as reverse values and names for use
in other functions. The functions is also a wrapper for the rrapply and
map_depth functions with the dots argument.

## Usage

``` r
listR(list, type, ...)
```

## Arguments

- list:

  the list to modify

- type:

  type of modification of the list. See details

- ...:

  arguments for the rrapply function

## Value

returns a modified list based on the "type" argument

## Details

"reverse" reverses the values and names of a list which is used as
inputs in functions such as str_replace.
