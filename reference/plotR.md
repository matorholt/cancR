# Autoplot for estimatR, inferencR and clustR

Autoplot for estimatR, inferencR and clustR

## Usage

``` r
plotR(
  list,
  y = 100,
  col = cancR_palette,
  table.col = "#616161",
  risk.col = F,
  time.unit = "m2y",
  labs = levels,
  print.est = TRUE,
  contrast = "rd",
  se = T,
  p.values = T,
  style = NULL,
  linewidth = 0.8,
  title = "",
  title.size = 7,
  title.shift = c(0, 0),
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
  box = T,
  box.shift = 0,
  box.fill = "White",
  box.color = "Black",
  box.linewidth = 0.8,
  contrast.digits = 1,
  table = c("event", "risk"),
  table.space = 1,
  table.padding = 1,
  table.title.size = 6,
  table.text.size = 5,
  table.linewidth = 0.8,
  legend.pos = c(0.5, 0.9),
  legend.size = 16,
  tscale = 1,
  censur = F
)
```

## Arguments

- list:

  an object of class estimatR, inferencR or clustR

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

- p.values:

  whether p-values should be printed in the results, default = T

- style:

  the formatting style of the contrast. Currently JAMA and italic

- linewidth:

  thickness of the risk curve lines

- title:

  Plot title

- title.size:

  Plot title size

- title.shift:

  vector of XY shifting of the plot title

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

- box:

  whether there should be a box around the results

- box.shift:

  Horizontal shifting of the right end of the box

- box.fill:

  fill color for the box

- box.color:

  border color for the box

- box.linewidth:

  Results box linewidth

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

- legend.pos:

  XY vector of legend position in percentage

- tscale:

  Global size scaler

- censur:

  Whether values \<= 3 should be censored. Default = FALSE

## Value

Plot of the adjusted cumulative incidence or Kaplan-Meier curve

## Examples

``` r
#Risk in one group

t1 <- estimatR(analysis_df,
timevar = ttt,
event = event)
#> 
#> ── Initializing estimatR algorithm: 2026-06-03 14:24:49 ──
#> 
#> ── Estimation complete! 
#> Total runtime:
#> 0.2 secs

plotR(t1)


#Risks in multiple groups
t2 <- estimatR(analysis_df,
timevar = ttt,
event = event,
group = X2)
#> 
#> ── Initializing estimatR algorithm: 2026-06-03 14:24:49 ──
#> 
#> Error in select(., {    {        group    }}): Can't select columns that don't exist.
#> ✖ Column `X2` doesn't exist.

plotR(t2)
#> Error: object 't2' not found

```
