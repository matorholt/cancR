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

             Overall (N=500)

1 age  
2 Median 51.5  
3 Q1, Q3 35.2, 65.7 4 Range 9.6 - 88.5 5 sex  
6 Median 2.0  
7 Q1, Q3 1.0, 2.0 8 Range 1.0 - 2.0 9 type  
10 Median 1.0  
11 Q1, Q3 0.0, 2.0 12 Range 0.0 - 2.0

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

             Overall (N=500)

1 age  
2 Median 51.5  
3 Q1, Q3 35.2, 65.7 4 Range 9.6 - 88.5 5 sex  
6 2 269 (53.8%) 7 1 231 (46.2%) 8 type  
9 1 228 (45.6%) 10 0 137 (27.4%) 11 2 135 (27.0%)

### Tables with multiple groups

If we want to compare multiple groups, we use the `group` argument. Let
us compare tumor types

``` r
redcap_df %>% 
  factR(c(sex, type)) %>% 
  tablR(group = type,
        vars = c(age, sex))
```

              1 (N=228)  0 (N=137)   2 (N=135)

1 age  
2 Median 51.0 50.6 53.5  
3 Q1, Q3 35.5, 65.5 32.7, 65.5 36.8, 66.6 4 Range 11.0 - 88.0 9.6 - 88.5
11.0 - 84.2 5 sex  
6 2 117 (51.3%) 78 (56.9%) 74 (54.8%) 7 1 111 (48.7%) 59 (43.1%) 61
(45.2%)

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

              1 (N=228)  0 (N=137)   2 (N=135) Total (N=500)  p-value

1 age p = 0.88 2 Median 51.0 50.6 53.5 51.5  
3 Q1, Q3 35.5, 65.5 32.7, 65.5 36.8, 66.6 35.2, 65.7  
4 Range 11.0 - 88.0 9.6 - 88.5 11.0 - 84.2 9.6 - 88.5  
5 sex p = 0.56 6 2 117 (51.3%) 78 (56.9%) 74 (54.8%) 269 (53.8%)  
7 1 111 (48.7%) 59 (43.1%) 61 (45.2%) 231 (46.2%)

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
    labs.groups = list("benign" = "0",
                                     "in situ" = "1",
                                     "malignant" = "2"),
    labs.headings = list("Age at Debut" = "age"),
    labs.subheadings = list("sex" = list("Female" = "2",
                                         "Male" = "1")))
#>                benign (N=137) in situ (N=228) malignant (N=135)
#> 1 Age at Debut                                                 
#> 2    Median           50.6           51.0              53.5    
#> 3    Q1, Q3        32.7, 65.5     35.5, 65.5        36.8, 66.6 
#> 4    Range         9.6 - 88.5     11.0 - 88.0       11.0 - 84.2
#> 5 Sex                                                          
#> 6    Female        78 (56.9%)     117 (51.3%)       74 (54.8%) 
#> 7    Male          59 (43.1%)     111 (48.7%)       61 (45.2%)
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
    labs.groups = list("benign" = "0",
                                     "in situ" = "1",
                                     "malignant" = "2"),
    labs.headings = list("Age at Debut" = "age"),
    labs.subheadings = list("sex" = list("Female" = "2",
                                         "Male" = "1"),
                            "localisation" = list("Neck" = "0",
                                                  "Head" = "1",
                                                  "Trunk" = "2",
                                                  "Upper Extremity" = "3",
                                                  "Lower Extremity" = "4",
                                                  "Unspecified" = "5")),
    reference = list("sex" = c("Female")))
#>                       benign (N=137) in situ (N=228) malignant (N=135)
#> 1  Age at Debut                                                       
#> 2     Median                 50.6           51.0              53.5    
#> 3     Q1, Q3              32.7, 65.5     35.5, 65.5        36.8, 66.6 
#> 4     Range               9.6 - 88.5     11.0 - 88.0       11.0 - 84.2
#> 5  Sex                                                                
#> 6     Female              78 (56.9%)     117 (51.3%)       74 (54.8%) 
#> 7     Male                59 (43.1%)     111 (48.7%)       61 (45.2%) 
#> 8  Localisation                                                       
#> 9     Neck                 2 (1.5%)       6 (2.6%)          6 (4.4%)  
#> 10    Head                24 (17.5%)     30 (13.2%)        26 (19.3%) 
#> 11    Trunk               45 (32.8%)     75 (32.9%)        37 (27.4%) 
#> 12    Upper Extremity     45 (32.8%)     73 (32.0%)        48 (35.6%) 
#> 13    Lower Extremity     17 (12.4%)     33 (14.5%)        14 (10.4%) 
#> 14    Unspecified          4 (2.9%)       11 (4.8%)         4 (3.0%)
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
    labs.groups = list("benign" = "0",
                                     "in situ" = "1",
                                     "malignant" = "2"),
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
    numeric = c("mean", "sd"))
#>                       benign (N=137) in situ (N=228) malignant (N=135)
#> 1  Age at Debut                                                       
#> 2     Mean                   50.2           50.4               51.2   
#> 3     SD                     18.7           18.4               18.0   
#> 4  Sex                                                                
#> 5     Female              78 (56.9%)     117 (51.3%)        74 (54.8%)
#> 6     Male                59 (43.1%)     111 (48.7%)        61 (45.2%)
#> 7  Localisation                                                       
#> 8     Neck                 2 (1.5%)       6 (2.6%)           6 (4.4%) 
#> 9     Head                24 (17.5%)     30 (13.2%)         26 (19.3%)
#> 10    Trunk               45 (32.8%)     75 (32.9%)         37 (27.4%)
#> 11    Upper Extremity     45 (32.8%)     73 (32.0%)         48 (35.6%)
#> 12    Lower Extremity     17 (12.4%)     33 (14.5%)         14 (10.4%)
#> 13    Unspecified          4 (2.9%)       11 (4.8%)          4 (3.0%)
```

\### Customizing tables

All levels and labels can be set manually. Furthermore, the table can be
exported as a flextable object for nicer layout. For this example we
also collapse variables of similar kind for simplicity with the
`simplify` argument. This is preferred in a lot of variables with
yes/no, 1/0, positive/negative syntax.

``` r
# redcap_df %>%
#   mutate(margins = sample(c("0","1"), nrow(redcap_df), replace=TRUE)) %>%
#   factR(c(type, sex, localisation, cd10, sox10, ck, margins, necrosis)) %>%
#   tablR(group=type,
#          vars = c(age, sex, localisation, cd10, sox10, ck, necrosis, margins),
#     labs.groups = list("benign" = "0",
#                                      "in situ" = "1",
#                                      "malignant" = "2"),
#          reverse = T,
#          labs.headings = list("Age at Debut" = "age",
#                               "Gender" = "sex",
#                               "CD10" = "cd10"),
#          labs.subheadings = list("sex" = list("Female" = "2",
#                                               "Male" = "1"),
#                                  "localisation" = list("Neck" = "0",
#                                                        "Head" = "1",
#                                                        "Trunk" = "2",
#                                                        "Upper Extremity" = "3",
#                                                        "Lower Extremity" = "4",
#                                                        "Unspecified" = "5")),
#          reference = list("sex" = c("Male")),
#          simplify=list("Immunohistochemistry" = c("cd10", "sox10", "ck"),
#                        "Tumor" = c("necrosis", "margins")),
#          censur=T,
#          numeric = c("mean", "sd", "q1q3", "iqr"),
#          test=T,
#          total=T)
```
