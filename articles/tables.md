# Tables

## Introduction

Tables are essential for presenting the study population and the
distribution of important covariates. The tablR function in the cancR
package is useful for this task.  
  
First we load the cancR package

``` r
library(cancR)
```

### Tables with only on group

We use our `redcap_df` as an example. Let us create a simple table of
the whole population and present the covariates age, sex and tumor type

``` r
tablR(redcap_df,
      vars = c(age, sex, type))
```

|           | Overall (N=500) |
|:----------|:---------------:|
| age       |                 |
| \- Median |      51.5       |
| \- Q1, Q3 |   35.2, 65.7    |
| \- Range  |   9.6 - 88.5    |
| sex       |                 |
| \- Median |       2.0       |
| \- Q1, Q3 |    1.0, 2.0     |
| \- Range  |    1.0 - 2.0    |
| type      |                 |
| \- Median |       1.0       |
| \- Q1, Q3 |    0.0, 2.0     |
| \- Range  |    0.0 - 2.0    |

We instantly conclude that sex and type is incorrectly formatted, as we
wish to see percentages and not a median as if the 0/1 structure was
numeric. We convert the variables using
[`factR()`](https://matorholt.github.io/cancR/reference/factR.md) and
pipe into the
[`tablR()`](https://matorholt.github.io/cancR/reference/tablR.md).
Notice that when we pipe we do not need to specify the data frame in
[`tablR()`](https://matorholt.github.io/cancR/reference/tablR.md)

``` r
redcap_df %>% 
  factR(c(sex, type)) %>% 
  tablR(vars = c(age, sex, type))
```

|           | Overall (N=500) |
|:----------|:---------------:|
| age       |                 |
| \- Median |      51.5       |
| \- Q1, Q3 |   35.2, 65.7    |
| \- Range  |   9.6 - 88.5    |
| sex       |                 |
| \- 2      |   269 (53.8%)   |
| \- 1      |   231 (46.2%)   |
| type      |                 |
| \- 1      |   228 (45.6%)   |
| \- 0      |   137 (27.4%)   |
| \- 2      |   135 (27.0%)   |

### Tables with multiple groups

If we want to compare multiple groups, we use the `group` argument. Let
us compare tumor types

``` r
redcap_df %>% 
  factR(c(sex, type)) %>% 
  tablR(group = type,
        vars = c(age, sex))
```

|           |  2 (N=135)  | 0 (N=137)  |  1 (N=228)  |
|:----------|:-----------:|:----------:|:-----------:|
| age       |             |            |             |
| \- Median |    53.5     |    50.6    |    51.0     |
| \- Q1, Q3 | 36.8, 66.6  | 32.7, 65.5 | 35.5, 65.5  |
| \- Range  | 11.0 - 84.2 | 9.6 - 88.5 | 11.0 - 88.0 |
| sex       |             |            |             |
| \- 2      | 74 (54.8%)  | 78 (56.9%) | 117 (51.3%) |
| \- 1      | 61 (45.2%)  | 59 (43.1%) | 111 (48.7%) |

We can also add a `total`column and also test the differences in
distributions

``` r
redcap_df %>% 
  factR(c(sex, type),
        levels = list("type" = c("0", "1", "2"))) %>% 
  tablR(group = type,
        vars = c(age, sex),
        test = TRUE,
        total = TRUE)
```

|           |  2 (N=135)  |  1 (N=228)  | 0 (N=137)  | Total (N=500) | p value |
|:----------|:-----------:|:-----------:|:----------:|:-------------:|--------:|
| age       |             |             |            |               |   0.883 |
| \- Median |    53.5     |    51.0     |    50.6    |     51.5      |         |
| \- Q1, Q3 | 36.8, 66.6  | 35.5, 65.5  | 32.7, 65.5 |  35.2, 65.7   |         |
| \- Range  | 11.0 - 84.2 | 11.0 - 88.0 | 9.6 - 88.5 |  9.6 - 88.5   |         |
| sex       |             |             |            |               |   0.559 |
| \- 2      | 74 (54.8%)  | 117 (51.3%) | 78 (56.9%) |  269 (53.8%)  |         |
| \- 1      | 61 (45.2%)  | 111 (48.7%) | 59 (43.1%) |  231 (46.2%)  |         |

### Customizing tables

Most of the content of the table can be customized in
[`tablR()`](https://matorholt.github.io/cancR/reference/tablR.md)

#### Changing labels

It is possible to rename three types of labels:  
- Group names: `labs.groups` - Variable names: `labs.headings` -
Variable labels/levels: `labs.subheadings`

``` r
redcap_df %>%
  factR(c(type, sex)) %>%
  tablR(
    group = type,
    vars=c(age, sex),
    labs.groups = list("type" = list("benign" = "0",
                                     "in situ" = "1",
                                     "malignant" = "2")),
    labs.headings = list("Age at Debut" = "age"),
    labs.subheadings = list("sex" = list("Female" = "2",
                                         "Male" = "1")))
#> 
#> 
#> |             | malignant (N=135) | benign (N=137) | in situ (N=228) |
#> |:------------|:-----------------:|:--------------:|:---------------:|
#> |Age at Debut |                   |                |                 |
#> |-  Median    |       53.5        |      50.6      |      51.0       |
#> |-  Q1, Q3    |    36.8, 66.6     |   32.7, 65.5   |   35.5, 65.5    |
#> |-  Range     |    11.0 - 84.2    |   9.6 - 88.5   |   11.0 - 88.0   |
#> |Sex          |                   |                |                 |
#> |-  Female    |    74 (54.8%)     |   78 (56.9%)   |   117 (51.3%)   |
#> |-  Male      |    61 (45.2%)     |   59 (43.1%)   |   111 (48.7%)   |
```

#### Changing orders

The order of the groups and variable levels can be specified with the
`reference()` and [`levels()`](https://rdrr.io/r/base/levels.html)
arguments

``` r
redcap_df %>%
  factR(c(type, sex, localisation)) %>%
  tablR(
    group = type,
    vars=c(age, sex, localisation),
    labs.groups = list("type" = list("benign" = "0",
                                     "in situ" = "1",
                                     "malignant" = "2")),
    labs.headings = list("Age at Debut" = "age"),
    labs.subheadings = list("sex" = list("Female" = "2",
                                         "Male" = "1"),
                            "localisation" = list("Neck" = "0",
                                                  "Head" = "1",
                                                  "Trunk" = "2",
                                                  "Upper Extremity" = "3",
                                                  "Lower Extremity" = "4",
                                                  "Unspecified" = "5")),
    reference = list("sex" = c("Female")),
    levels = list("localisation" = c("Trunk", "Head")))
#> 
#> 
#> |                   | malignant (N=135) | benign (N=137) | in situ (N=228) |
#> |:------------------|:-----------------:|:--------------:|:---------------:|
#> |Age at Debut       |                   |                |                 |
#> |-  Median          |       53.5        |      50.6      |      51.0       |
#> |-  Q1, Q3          |    36.8, 66.6     |   32.7, 65.5   |   35.5, 65.5    |
#> |-  Range           |    11.0 - 84.2    |   9.6 - 88.5   |   11.0 - 88.0   |
#> |Sex                |                   |                |                 |
#> |-  Female          |    74 (54.8%)     |   78 (56.9%)   |   117 (51.3%)   |
#> |-  Male            |    61 (45.2%)     |   59 (43.1%)   |   111 (48.7%)   |
#> |Localisation       |                   |                |                 |
#> |-  Trunk           |    37 (27.4%)     |   45 (32.8%)   |   75 (32.9%)    |
#> |-  Head            |    26 (19.3%)     |   24 (17.5%)   |   30 (13.2%)    |
#> |-  Upper Extremity |    48 (35.6%)     |   45 (32.8%)   |   73 (32.0%)    |
#> |-  Lower Extremity |    14 (10.4%)     |   17 (12.4%)   |   33 (14.5%)    |
#> |-  Unspecified     |     4 (3.0%)      |    4 (2.9%)    |    11 (4.8%)    |
#> |-  Neck            |     6 (4.4%)      |    2 (1.5%)    |    6 (2.6%)     |
```

#### Changing summary measures

The default summary measures for continuous variables are median,
interquartile range and range. This can be specified in the
[`numeric()`](https://rdrr.io/r/base/numeric.html) argument

``` r
redcap_df %>%
  factR(c(type, sex, localisation)) %>%
  tablR(
    group = type,
    vars=c(age, sex, localisation),
    labs.groups = list("type" = list("Benign" = "0",
                                     "In situ" = "1",
                                     "Malignant" = "2")),
    labs.headings = list("Age at Debut" = "age"),
    labs.subheadings = list("sex" = list("Female" = "2",
                                         "Male" = "1"),
                            "localisation" = list("Neck" = "0",
                                                  "Head" = "1",
                                                  "Trunk" = "2",
                                                  "Upper Extremity" = "3",
                                                  "Lower Extremity" = "4",
                                                  "Unspecified" = "5")),
    reference = list("sex" = c("Female")),
    levels = list("localisation" = c("Trunk", "Head"),
                  "type" = c("Benign", "In situ", "Malignant")),
    numeric = c("mean", "sd"))
#> 
#> 
#> |                   | Benign (N=137) | In situ (N=228) | Malignant (N=135) |
#> |:------------------|:--------------:|:---------------:|:-----------------:|
#> |Age at Debut       |                |                 |                   |
#> |-  Mean            |      50.2      |      50.4       |       51.2        |
#> |-  SD              |      18.7      |      18.4       |       18.0        |
#> |Sex                |                |                 |                   |
#> |-  Female          |   78 (56.9%)   |   117 (51.3%)   |    74 (54.8%)     |
#> |-  Male            |   59 (43.1%)   |   111 (48.7%)   |    61 (45.2%)     |
#> |Localisation       |                |                 |                   |
#> |-  Trunk           |   45 (32.8%)   |   75 (32.9%)    |    37 (27.4%)     |
#> |-  Head            |   24 (17.5%)   |   30 (13.2%)    |    26 (19.3%)     |
#> |-  Upper Extremity |   45 (32.8%)   |   73 (32.0%)    |    48 (35.6%)     |
#> |-  Lower Extremity |   17 (12.4%)   |   33 (14.5%)    |    14 (10.4%)     |
#> |-  Unspecified     |    4 (2.9%)    |    11 (4.8%)    |     4 (3.0%)      |
#> |-  Neck            |    2 (1.5%)    |    6 (2.6%)     |     6 (4.4%)      |
```
