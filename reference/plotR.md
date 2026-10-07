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
  time.to.event = "none",
  cens.lines = F,
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
  y.breaks = NULL,
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
  event.title = "Cumulative Events",
  risk.title = "Number at Risk",
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

- time.to.event:

  type of time to event line, choose between "vertical", "horizontal",
  "both" or "none" (default)

- cens.lines:

  whether censoring points should be printed

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

- y.breaks:

  break size for the y-axis in percent (e.g. y.breaks = 2.5 equals 2.5%
  increments)

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

- event.title:

  title of the cumulative events table

- risk.title:

  title of the number at risk table

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
#> ── Initializing estimatR algorithm: 2026-10-07 13:57:41 ──
#> 
#> Preparing data:
#> Error in get(timevar_c): object 'ttt' not found
#> ── Estimation complete! 
#> Preparing data:
#> Total runtime:
#> Preparing data:
#> 0.01 secs
#> Preparing data:
#> 
#> Preparing data:

plotR(t1)
#> Error: object 't1' not found

#Risks in multiple groups
t2 <- estimatR(analysis_df,
timevar = ttt,
event = event,
group = X2)
#> 
#> ── Initializing estimatR algorithm: 2026-10-07 13:57:41 ──
#> 
#> Preparing data:
#> ✖ Error: X2 is not a factor. Convert using the factR() function
#> Preparing data:
#> 
#> Preparing data:
#> ── Estimation complete! 
#> Preparing data:
#> Total runtime:
#> Preparing data:
#> 0.01 secs
#> Preparing data:
#> 
#> Preparing data:

plotR(t2)
#> Data not generated with the functions estimatR, inferencR or clustR from the cancR package

```
