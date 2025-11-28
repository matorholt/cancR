# Autoformatting of redcap exports

Automatic grabbing of labels from data dictionary and conversion to
correct date format for exported redcap datasets. The data dictionary
can be downloaded under: Project Home and Design -\> Dictionary -\>
Download the Data Dictionary

## Usage

``` r
redcapR(
  data,
  dictionary,
  namelist = list(),
  date.vars = NULL,
  autoformat = T,
  formatlist = NULL,
  cprlist = NULL,
  index
)
```

## Arguments

- data:

  raw redcap dataset

- dictionary:

  data dictionary in semicolon-separated .csv format

- namelist:

  optional list for manual labelling e.g. list("observer" = list("John"
  = "1", "Me" = "2"))

- date.vars:

  vector of variable names containing date if not automatically
  converted

- autoformat:

  whether all labels should be in lowercase and underscores as spaces

- formatlist:

  optional list for recoding common values such as positiv -\> 1/pos/yes
  e.g. list("pos" = "positive", "no" = "Not Present")

- cprlist:

  optional dataframe containing cpr numbers for extraction of birth and
  sex

- index:

  optional index date for calculation of age at index

## Value

relabelled redcap dataset with correctly formatted dates
