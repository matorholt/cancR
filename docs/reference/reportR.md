# Overview of matched and unmatched cases

Overview of matched and unmatched cases

## Usage

``` r
reportR(
  data,
  casename,
  vars = c(period, age_group, sex, education, income, cci, region, marital),
  table = F,
  plot = F,
  type = "simple",
  cols = cancR_palette,
  headings = list(),
  layout = "vertical",
  vjust = -0.5,
  text.color = "White",
  ...
)
```

## Arguments

- data:

  matched dataset

- casename:

  name or number indicating cases (e.g. "1" or "CLL")

- vars:

  vars that should be in the table

- table:

  whether a table should be made (default = F)

- plot:

  whether a plot should be made (default = F)

- type:

  whether non-matched counts should be collapsed to "unmatched"
  ("simple") or remain stratified ("full")

- cols:

  color palette (default is cancR_palette)

- headings:

  List specifying labels for variable names

- layout:

  layout of the bar chart (horizontal or vertical (default))

- vjust:

  vertical adjustment of the counts (pct) labels

- text.color:

  label colors

- ...:

  passed to tablR

## Value

Prints the matching report, table and plot. Returns af list of the table
and plot.
