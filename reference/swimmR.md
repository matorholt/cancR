# Plot individual patient trajectories from time-to-event data (Swimmer plot)

Plot individual patient trajectories from time-to-event data (Swimmer
plot)

## Usage

``` r
swimmR(
  data,
  event,
  strata,
  strata.labels = list(),
  strata.levels = list(),
  horizon = 60,
  breaks = 6,
  table.list = NULL,
  table.cols = c("gainsboro", "grey", "#BCCFE8"),
  cols = list(),
  shapes = list(),
  labs,
  id = "id"
)
```

## Arguments

- data:

  data frame containg status and time-to-event columns

- event:

  columns containing status indicators. Needs to have corresponding
  "t\_" prefix

- strata:

  optional stratification by a group

- strata.labels:

  named list of labels for the strata in the format list(new = "old)

- strata.levels:

  character vector of levels for the strata

- horizon:

  time horizon

- breaks:

  breaks between time zero and horizon

- table.list:

  named list for each stratum with a named list of rows and sequence of
  x-values

- table.cols:

  vector of the colors of the tables

- cols:

  named list with colors for the symbols

- shapes:

  named list with shapes

- labs:

  named list with labels for the events

- id:

  name of the id variable

## Value

a swimmer plot
