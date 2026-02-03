# Create frequency tables

Wrapper for the tableby function in the Arsenal package

## Usage

``` r
tablR(
  data,
  group,
  vars,
  num.vars,
  test = FALSE,
  total = FALSE,
  numeric = c("medianq1q3", "range"),
  direction = "colwise",
  reference = list(),
  levels = list(),
  labs.groups = list(),
  labs.headings = list(),
  labs.subheadings = list(),
  reverse = F,
  test.stats = c("kwt", "chisq"),
  show.na = FALSE,
  censur = F,
  weights,
  digits = 1,
  ama = T,
  simplify = list(),
  simplify.remove = c("no", "0"),
  print = F,
  flextable = F
)
```

## Arguments

- data:

  dataframe

- group:

  Grouping variable. If omitted a single column tabel is provided

- vars:

  Variables that the grouping variable should be aggregated by

- num.vars:

  vector of variables that should be sorted based on numerical order

- test:

  Whether statistical tests should be performed (default = FALSE)

- total:

  Whether a total column should be included (default = FALSE)

- numeric:

  Selection of the type of stats for numerical variables. Options
  include: median, q1q3, medianq1q3 (default), iqr, range, mean, sd,
  meansd, min, max)

- direction:

  Direction for percentages (colwise or rowwise)

- reference:

  List specifying reference group for each variable

- labs.headings:

  List specifying labels for variable names

- reverse:

  whether the order of groups should start with the highest level
  (default = T)

- test.stats:

  Vector of length 2 containing statistical tests that should be
  performed

- show.na:

  Whether NAs should be presented

- censur:

  whether counts \<= 3 should be censored

- weights:

  optional name of the column containing weights for weighted summaries

- digits:

  number of digits

- ama:

  whether percentages \>10 should be without digits per AMA journal of
  style. Default = T.

- simplify:

  a list of column names that should be simplified by dropping specified
  levels (e.g. 0 or "no") for simple output. Groups of size \> 1 should
  be named such as: list("IHC" = c("cd10", "sox10", "ck"), "necrosis",
  "margins")

- simplify.remove:

  vector of labels that should be removed from the columns assigned in
  the "simplify" argument. Default = c("no", "0")

- print:

  whether the table should be printed in the console

- flextable:

  whether the table should be returned as a flextable

- labels:

  List specifying labels of the specific labels for each variable

## Value

Returns a table as a dataframe or flextable

## Examples

``` r
tablR(population_denmark,
     group = sex,
     vars = c(age_group, population))
#>                                           M (N=648)
#> 1  Age Group                                       
#> 2     0-5                       36 (5.6%)          
#> 3     10-15                     36 (5.6%)          
#> 4     15-20                     36 (5.6%)          
#> 5     20-25                     36 (5.6%)          
#> 6     25-30                     36 (5.6%)          
#> 7     30-35                     36 (5.6%)          
#> 8     35-40                     36 (5.6%)          
#> 9     40-45                     36 (5.6%)          
#> 10    45-50                     36 (5.6%)          
#> 11    5-10                      36 (5.6%)          
#> 12    50-55                     36 (5.6%)          
#> 13    55-60                     36 (5.6%)          
#> 14    60-65                     36 (5.6%)          
#> 15    65-70                     36 (5.6%)          
#> 16    70-75                     36 (5.6%)          
#> 17    75-80                     36 (5.6%)          
#> 18    80-85                     36 (5.6%)          
#> 19    85+                       36 (5.6%)          
#> 20 Population                                      
#> 21    Median (Q1, Q3) 169647.0 (129036.5, 189037.0)
#> 22    Range                23026.0 - 220477.0      
#>                        F (N=648)
#> 1                               
#> 2            36 (5.6%)          
#> 3            36 (5.6%)          
#> 4            36 (5.6%)          
#> 5            36 (5.6%)          
#> 6            36 (5.6%)          
#> 7            36 (5.6%)          
#> 8            36 (5.6%)          
#> 9            36 (5.6%)          
#> 10           36 (5.6%)          
#> 11           36 (5.6%)          
#> 12           36 (5.6%)          
#> 13           36 (5.6%)          
#> 14           36 (5.6%)          
#> 15           36 (5.6%)          
#> 16           36 (5.6%)          
#> 17           36 (5.6%)          
#> 18           36 (5.6%)          
#> 19           36 (5.6%)          
#> 20                              
#> 21 165007.5 (132838.0, 183598.5)
#> 22      54832.0 - 211165.0      
```
