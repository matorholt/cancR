# Create baseline table

Wrapper for the tableby function in the Arsenal package

## Usage

``` r
tablR(
  data,
  group,
  vars,
  test = FALSE,
  total = FALSE,
  numeric = c("median", "q1q3", "range"),
  direction = "colwise",
  reference = list(),
  levels = list(),
  labs.groups = list(),
  labs.headings = list(),
  labs.subheadings = list(),
  reverse = T,
  test.stats = c("kwt", "chisq"),
  show.na = FALSE,
  censur = F,
  digits = 1,
  simplify = F
)
```

## Arguments

- data:

  dataframe

- group:

  Grouping variable. If omitted a single column tabel is provided

- vars:

  Variables that the grouping variable should be aggregated by

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

Returns a table and exports a word-file (optional if filename is
provided)
