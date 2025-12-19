# Create baseline table

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
  numeric = c("median", "q1q3", "range"),
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
  digits = 1,
  simplify = list(),
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

  Selection of the type of stats for numerical variables (e.g. median,
  q1q3, range, mean, sd)

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

- digits:

  number of digits

- labels:

  List specifying labels of the specific labels for each variable

## Value

Returns a table as a dataframe or flextable

## Examples

``` r
tablR(population_denmark,
     group = sex,
     vars = c(age_group, population))
#>                        M (N=648)          F (N=648)
#> 1  Age Group                                       
#> 2     0-5         36 (5.6%)          36 (5.6%)     
#> 3     10-15       36 (5.6%)          36 (5.6%)     
#> 4     15-20       36 (5.6%)          36 (5.6%)     
#> 5     20-25       36 (5.6%)          36 (5.6%)     
#> 6     25-30       36 (5.6%)          36 (5.6%)     
#> 7     30-35       36 (5.6%)          36 (5.6%)     
#> 8     35-40       36 (5.6%)          36 (5.6%)     
#> 9     40-45       36 (5.6%)          36 (5.6%)     
#> 10    45-50       36 (5.6%)          36 (5.6%)     
#> 11    5-10        36 (5.6%)          36 (5.6%)     
#> 12    50-55       36 (5.6%)          36 (5.6%)     
#> 13    55-60       36 (5.6%)          36 (5.6%)     
#> 14    60-65       36 (5.6%)          36 (5.6%)     
#> 15    65-70       36 (5.6%)          36 (5.6%)     
#> 16    70-75       36 (5.6%)          36 (5.6%)     
#> 17    75-80       36 (5.6%)          36 (5.6%)     
#> 18    80-85       36 (5.6%)          36 (5.6%)     
#> 19    85+         36 (5.6%)          36 (5.6%)     
#> 20 Population                                      
#> 21    Median       169647.0           165007.5     
#> 22    Q1, Q3  129036.5, 189037.0 132838.0, 183598.5
#> 23    Range   23026.0 - 220477.0 54832.0 - 211165.0
```
