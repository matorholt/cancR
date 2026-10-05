# Win ratio/Win difference analysis

Win ratio/Win difference analysis as described by Pocock et al. with the
Finkelstein-Schoenfeld test.

## Usage

``` r
wR(
  data,
  hierarchy,
  plot = T,
  max.time = NA,
  digits = 4,
  alpha = 0.05,
  verbose = T
)
```

## Arguments

- data:

  A data frame with four columns:

  id

  :   ID column with multiple rows per subject

  event

  :   Event type; `0` = censoring

  event_time

  :   Time to event

  allocation

  :   Treatment arm; `"trt"` = treatment, `"ctrl"` = control

  The last row per `id` must include either a terminal event or
  censoring, with the corresponding `event_time` representing the
  maximum follow-up date.

- hierarchy:

  Named list of outcomes with corresponding event numbers (e.g.
  `list("death" = 1, "recurrence" = 2)`). The order determines the
  hierarchy, with the first element being the most important outcome.

- max.time:

  Maximum follow-up time; `event_time` values beyond this will be
  truncated.

- digits:

  Number of digits used for rounding shared follow-up time, allowing for
  slightly faster computation.

- alpha:

  Alpha level (default: `0.05`).

- verbose:

  Logical; whether objects should be printed for debugging (default:
  `FALSE`).

## Value

A list containing the following elements:

- `win_counts`: Wins, losses, ties and proportions, overall and per
  component

- `win_ratio`: Win ratio with 95\\

- `win_difference`: Win difference with 95\\
