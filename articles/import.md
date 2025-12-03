# Import data to R

``` r
library(cancR)
```

All data files should optimally be located in a `data` folder in the
project folder. If the project is set up correctly (see the article
`R setup`) we can call the files directly from the `data` folder:
“data/file.csv”.

## Loading data

The [`readR()`](https://matorholt.github.io/cancR/reference/readR.md)
function in `cancR` makes loading easy by automatically chosing the
correct function based on the file extension.

``` r
mydata <- readR("data/dataset.csv")
```

[`readR()`](https://matorholt.github.io/cancR/reference/readR.md)
currently handles the extensions: `csv`, `xlsx`, `rds` and `txt`.

## Loading data from redcap

Data from redcap can conveniently be loaded with
[`redcapR()`](https://matorholt.github.io/cancR/reference/redcapR.md).
This function takes two arguments: 1) The dataset exported from redcap
(see how to export from redcap in the article `Redcap and R`) and 2) The
data dictionary from redcap (see `Redcap and R`)

``` r
redcap_data <- readR("data/redcap_data.csv")
dictionary <- readR("data/data_dictionary.csv")

mydata <- redcapR(redcap_data,
                  dictionary)
```

This will apply all labels from redcap automatically so that manual
recoding is not necessary.

If there is a sheet containing CPR-numbers and id numbers it is also
possible to automatically extract birth date and sex in `redcapR`

``` r
redcap_data <- readR("data/redcap_data.csv")
dictionary <- readR("data/data_dictionary.csv")
cprlist <- readR("data/cprlist.csv")

mydata <- redcapR(redcap_data,
                  dictionary,
                  cprlist)
```
