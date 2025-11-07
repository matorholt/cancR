# Convert dates to status indicator and time-to-event

Converts event dates to status indicator and a time-to-event variable.
The status indicator will be assigned to: 0 if alive and event-free, 1
in the presence of the event of interest 2 in the presence of the
competing event, often death

## Usage

``` r
structR(
  data,
  index,
  fu,
  outcomes,
  competing,
  composite = list(),
  pattern = "_date",
  unit = "months",
  keep.dates = F,
  digits = 2
)
```

## Arguments

- data:

  Dataframe

- index:

  Index time point 'T=0' provided in date format

- fu:

  End of follow-up or time of death in date format

- outcomes:

  vector of single or multiple column names with the event of interest
  in date format

- competing:

  vector of single or multiple column names that should be considered
  competing risks in the specified order.

- composite:

  named list of composite outcomes with one list per outcome specifying
  "outcomes" and "competing" (optional).

- pattern:

  Indicates the name pattern of the outcomes such as index_event or
  event_date

- unit:

  Whether time-to-event should be reported in months or years.

- digits:

  number of digits on event times

- keep_dates:

  Whether the original event dates should be kept.

## Value

For each outcome, a status indicator of 0/1/2 and a time-to-event column
are returned. If competing is missing, the levels are 0/1 For the
competing event, a status indicator of 0/1 and a time-to-event is
returned. For patients without an event the function returns the time to
last follow-up if status = 0 and time to death if status = 2
