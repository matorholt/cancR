# Join two or more dataframes

Join two or more dataframes as a left, right, full or inner join. The
dataframes can be provided separately or as a list.

## Usage

``` r
joinR(..., by, type = "left", vars = NULL, dt = NULL)
```

## Arguments

- ...:

  dataframes or list of dataframes to be joined

- by:

  variable to join on. This can either be a vector of one or multiple
  keys (e.g. by = c("id", "group")) or a list of vectors containing key
  columns.

- type:

  type of join (see details)

- vars:

  vector of variables the join should update/insert

- dt:

  whether the returned dataframe should be a data.table

## Value

a single joined dataframe

## Details

The join types fall into four groups.

**Mutating joins** combine the columns of `x` and `y`:

- `left`:

  Keeps all rows of `x`. Columns from `y` are added, and rows without a
  match get `NA`.

- `right`:

  Keeps all rows of `y`. Columns from `x` are added, and rows without a
  match get `NA`.

- `full`:

  Keeps all rows of both `x` and `y`. Unmatched rows on either side get
  `NA` in the other side's columns.

- `inner`:

  Keeps only rows with a match in both `x` and `y`.

**Filtering joins** only filter `x` and never add columns from `y`:

- semi:

  Keeps rows of `x` that have a match in `y`.

- `anti`:

  Keeps rows of `x` that have **no** match in `y`.

**Row-update joins** modify `x` using rows from `y`. They use the
columns that `x` and `y` have in common, apart from the keys, or only
the columns named in `vars`. They do not add new columns to `x`:

- `insert`:

  For matching keys, only `NA` values in `x` are filled with the values
  from `y`. Existing non-`NA` values are kept, and no new rows are
  added.

- `update`:

  For matching keys, values in `x` are overwritten with the values from
  `y`, and rows in `y` with keys not found in `x` are added as new rows.

**Keys**

Use `by = "id"` or `by = c("id", "grp")` when the key columns have the
same name in all tables. When the names differ, use the form
`by = "id = pnr = id2"`, corresponding to the same length as the number
of input datasets. Alternatively pass a list with one \#' element per
key set e.g. `list(c("id", "pnr"), "grp")`

## Examples

``` r
x <- data.table(id = c(1,2,3,3), grp = c("g1", "g1", "g1", "g2"), name = c("a", "b", "c", "d"), score = c(10, NA, 30, NA))
y <- data.table(id = 2:4, grp = c("g1", "g2", "g1"), city = c("X", "Y", "Z"), active = c(TRUE, FALSE, TRUE))
new_rows <- data.table(id = 2:4, name = c("b2", "c2", "d"), score = c(20, 35, 40))
z <- data.table(pnr = c(1, 3, 5), region = c("N", "S", "E"))
df_list <- list(
  data.table(id2 = 1:3,        grp = c("g1", "g1", "g2"), val_a = c("a1", "a2", "a3")),
  data.table(pnr = c(1, 2, 4), grp = c("g1", "g2", "g1"), val_b = c("b1", "b2", "b4")),
  data.table(id  = c(1, 3, 4), grp = c("g1", "g2", "g1"), val_c = c("c1", "c3", "c4"))
)

# Basic joins
joinR(x, y, by = id, type = left)
#> Key: <id>
#>       id  grp.x   name score  grp.y   city active
#>    <int> <char> <char> <num> <char> <char> <lgcl>
#> 1:     1     g1      a    10   <NA>   <NA>     NA
#> 2:     2     g1      b    NA     g1      X   TRUE
#> 3:     3     g1      c    30     g2      Y  FALSE
#> 4:     3     g2      d    NA     g2      Y  FALSE
joinR(x, y, by = c(id, grp), type = left)
#> Key: <id, grp>
#>       id    grp   name score   city active
#>    <int> <char> <char> <num> <char> <lgcl>
#> 1:     1     g1      a    10   <NA>     NA
#> 2:     2     g1      b    NA      X   TRUE
#> 3:     3     g1      c    30   <NA>     NA
#> 4:     3     g2      d    NA      Y  FALSE
joinR(x, y, by = id, type = full)
#> Key: <id>
#>       id  grp.x   name score  grp.y   city active
#>    <int> <char> <char> <num> <char> <char> <lgcl>
#> 1:     1     g1      a    10   <NA>   <NA>     NA
#> 2:     2     g1      b    NA     g1      X   TRUE
#> 3:     3     g1      c    30     g2      Y  FALSE
#> 4:     3     g2      d    NA     g2      Y  FALSE
#> 5:     4   <NA>   <NA>    NA     g1      Z   TRUE
joinR(x, y, by = c(id, grp), type = inner)
#> Key: <id, grp>
#>       id    grp   name score   city active
#>    <int> <char> <char> <num> <char> <lgcl>
#> 1:     2     g1      b    NA      X   TRUE
#> 2:     3     g2      d    NA      Y  FALSE

# Filter-joins
joinR(x, y, by = id, type = semi)
#>       id    grp   name score
#>    <num> <char> <char> <num>
#> 1:     2     g1      b    NA
#> 2:     3     g1      c    30
#> 3:     3     g2      d    NA
joinR(x, y, by = c(id, grp), type = anti)
#>       id    grp   name score
#>    <num> <char> <char> <num>
#> 1:     1     g1      a    10
#> 2:     3     g1      c    30

# Update/insert joins
joinR(x, new_rows, by = id, type = insert)
#>       id    grp   name score
#>    <num> <char> <char> <num>
#> 1:     1     g1      a    10
#> 2:     2     g1      b    20
#> 3:     3     g1      c    30
#> 4:     3     g2      d    35
joinR(x, new_rows, by = id, type = update)
#>       id    grp   name score
#>    <num> <char> <char> <num>
#> 1:     1     g1      a    10
#> 2:     2     g1     b2    20
#> 3:     3     g1     c2    35
#> 4:     3     g2     c2    35
# Update only score
joinR(x, new_rows, by = id, type = update, vars = score)
#>       id    grp   name score
#>    <num> <char> <char> <num>
#> 1:     1     g1      a    10
#> 2:     2     g1      b    20
#> 3:     3     g1      c    35
#> 4:     3     g2      d    35

# Different keys
joinR(x, z, by = "id = pnr", type = left)
#> Key: <id>
#>       id    grp   name score region
#>    <num> <char> <char> <num> <char>
#> 1:     1     g1      a    10      N
#> 2:     2     g1      b    NA   <NA>
#> 3:     3     g1      c    30      S
#> 4:     3     g2      d    NA      S
joinR(x, df_list, by = c("id = id2 = pnr = id"), type = left)
#> Warning: column names [grp.x, grp.y] are duplicated in the result
#> Key: <id>
#>       id  grp.x   name score  grp.y  val_a  grp.x  val_b  grp.y  val_c
#>    <int> <char> <char> <num> <char> <char> <char> <char> <char> <char>
#> 1:     1     g1      a    10     g1     a1     g1     b1     g1     c1
#> 2:     2     g1      b    NA     g1     a2     g2     b2   <NA>   <NA>
#> 3:     3     g1      c    30     g2     a3   <NA>   <NA>     g2     c3
#> 4:     3     g2      d    NA     g2     a3   <NA>   <NA>     g2     c3
joinR(x, y, df_list, by = list(c("id", "id", "id2", "pnr", "id"),
                               "grp"), type = "left")
#> Key: <id, grp>
#>       id    grp   name score   city active  val_a  val_b  val_c
#>    <int> <char> <char> <num> <char> <lgcl> <char> <char> <char>
#> 1:     1     g1      a    10   <NA>     NA     a1     b1     c1
#> 2:     2     g1      b    NA      X   TRUE     a2   <NA>   <NA>
#> 3:     3     g1      c    30   <NA>     NA   <NA>   <NA>   <NA>
#> 4:     3     g2      d    NA      Y  FALSE     a3   <NA>     c3
```
