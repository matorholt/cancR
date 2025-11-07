# Autoplot for estimatR, incidencR, inferencR and clustR

Autoplot for estimatR, incidencR, inferencR and clustR

## Usage

``` r
plotR(
  list,
  y = 100,
  col = cancR_palette,
  table.col = "#616161",
  risk.col = T,
  time.unit = "m2y",
  labs = levels,
  print.est = TRUE,
  contrast = "rd",
  se = T,
  border = T,
  style = NULL,
  linewidth = 1,
  title = "",
  title.size = 7,
  x.title = unit,
  x.title.size = 6,
  x.title.shift = 0,
  x.text.size = 6,
  y.title = "Risk of Event (%)",
  y.title.size = 6,
  y.title.shift = 0,
  y.text.size = 6,
  res.size = 5,
  res.shift = c(0, 0),
  res.spacing = 1,
  res.digits = 1,
  border.shift = 0,
  contrast.digits = 1,
  table = c("event", "risk"),
  table.space = 1,
  table.padding = 1,
  table.title.size = 6,
  table.text.size = 5,
  table.linewidth = 1,
  border.linewidth = 1,
  legend.pos = c(0.5, 0.9),
  tscale = 1,
  censur = F
)
```

## Arguments

- list:

  an object of class estimatR, incidencR or clustR

- y:

  Upper limit for y-axis

- col:

  Vector of colors

- table.col:

  Grid color

- risk.col:

  Whether risk table numbers should be colored (T/F)

- time.unit:

  Specification of the time-unit and optional conversion. Conversions
  include Months to years ("m2y"), days to years ("d2y") and days to
  months ("d2m")

- labs:

  Character vector of similar length to the number of levels in the
  group with labels. Reference is first.

- print.est:

  Whether absolute risks at the time horizon should be printet. Defaults
  to TRUE

- contrast:

  The type of contrast that should be provided. Includes risk difference
  ("rd", default), risk ratio ("rr"), hazard ratio ("hr") or "none".

- se:

  whether the confidence interval should be shown

- border:

  whether there should be borders around the results

- style:

  the formatting style of the contrast. Currently JAMA and italic

- linewidth:

  thickness of the risk curve lines

- title:

  Plot title

- title.size:

  Plot title size

- x.title:

  X-axis title

- x.title.size:

  X-axis title size

- x.title.shift:

  X-axis vertical shift

- x.text.size:

  X-axis text size

- y.title:

  Y-axis title

- y.title.size:

  Y-axis title size

- y.title.shift:

  Y-axis title horizontal shift

- y.text.size:

  Y-axis text.size

- res.size:

  Size of the results

- res.shift:

  Vector of XY shifting of the results

- res.spacing:

  Vertical spacing between results

- res.digits:

  Number of digits on the risk estimates

- border.shift:

  Horizontal shifting of right border

- contrast.digits:

  Number of digits on the contrasts

- table:

  Which parts of the risk table should be provided ("event", "risk",
  "none"). Default is c("event", "risk")

- table.space:

  Spacing between counts in risk table

- table.padding:

  Spacing between lines and first/last rows in the risk table

- table.title.size:

  Risk table titles size

- table.text.size:

  Risk table text size

- table.linewidth:

  Risk table linewidth

- border.linewidth:

  Results box linewidth

- legend.pos:

  XY vector of legend position in percentage

- tscale:

  Global size scaler

- censur:

  Whether values \<= 3 should be censored. Default = FALSE

## Value

Plot of the adjusted cumulative incidence or Kaplan-Meier curve
