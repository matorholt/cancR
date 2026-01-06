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

1 Age  
2 Median (Q1, Q3) 49.7 (34.1, 64.5) 3 Range 10.8 - 88.8  
4 Sex  
5 Median (Q1, Q3) 1.0 (1.0, 2.0)  
6 Range 1.0 - 2.0  
7 Type  
8 Median (Q1, Q3) 1.0 (1.0, 2.0)  
9 Range 0.0 - 2.0

We instantly conclude that sex and type is incorrectly formatted, as we
wish to see percentages and not a median as if the 0/1 structure was
numeric. We convert the variables using
[`factR()`](../reference/factR.md) and pipe into the
[`tablR()`](../reference/tablR.md). Notice that when we pipe we do not
need to specify the data frame in [`tablR()`](../reference/tablR.md)

``` r
redcap_df %>% 
  factR(c(sex, type)) %>% 
  tablR(vars = c(age, sex, type))
```

                        Overall (N=500)

1 Age  
2 Median (Q1, Q3) 49.7 (34.1, 64.5) 3 Range 10.8 - 88.8  
4 Sex  
5 1 256 (51.2%)  
6 2 244 (48.8%)  
7 Type  
8 1 254 (50.8%)  
9 2 133 (26.6%)  
10 0 113 (22.6%)

### Tables with multiple groups

If we want to compare multiple groups, we use the `group` argument. Let
us compare tumor types

``` r
redcap_df %>% 
  factR(c(sex, type)) %>% 
  tablR(group = type,
        vars = c(age, sex))
```

                             1 (N=254)         2 (N=133)         0 (N=113)

1 Age  
2 Median (Q1, Q3) 49.6 (34.9, 66.0) 50.4 (34.8, 63.4) 48.3 (31.8, 64.2)
3 Range 10.8 - 88.8 11.2 - 86.3 12.0 - 84.6  
4 Sex  
5 1 131 (51.6%) 59 (44.4%) 66 (58.4%)  
6 2 123 (48.4%) 74 (55.6%) 47 (41.6%)

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

                             1 (N=254)         2 (N=133)         0 (N=113)

1 Age  
2 Median (Q1, Q3) 49.6 (34.9, 66.0) 50.4 (34.8, 63.4) 48.3 (31.8, 64.2)
3 Range 10.8 - 88.8 11.2 - 86.3 12.0 - 84.6  
4 Sex  
5 1 131 (51.6%) 59 (44.4%) 66 (58.4%)  
6 2 123 (48.4%) 74 (55.6%) 47 (41.6%)  
Total (N=500) P-value 1 p = 0.94 2 49.7 (34.1, 64.5)  
3 10.8 - 88.8  
4 p = 0.09 5 256 (51.2%)  
6 244 (48.8%)

### Customizing tables

Most of the content of the table can be customized in
[`tablR()`](../reference/tablR.md)

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
#>                         benign (N=113)   in situ (N=254) malignant (N=133)
#> 1 Age at Debut                                                            
#> 2    Median (Q1, Q3) 48.3 (31.8, 64.2) 49.6 (34.9, 66.0) 50.4 (34.8, 63.4)
#> 3    Range              12.0 - 84.6       10.8 - 88.8       11.2 - 86.3   
#> 4 Sex                                                                     
#> 5    Female             47 (41.6%)        123 (48.4%)       74 (55.6%)    
#> 6    Male               66 (58.4%)        131 (51.6%)       59 (44.4%)
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
#>                          benign (N=113)   in situ (N=254) malignant (N=133)
#> 1  Age at Debut                                                            
#> 2     Median (Q1, Q3) 48.3 (31.8, 64.2) 49.6 (34.9, 66.0) 50.4 (34.8, 63.4)
#> 3     Range              12.0 - 84.6       10.8 - 88.8       11.2 - 86.3   
#> 4  Sex                                                                     
#> 5     Female             47 (41.6%)        123 (48.4%)       74 (55.6%)    
#> 6     Male               66 (58.4%)        131 (51.6%)       59 (44.4%)    
#> 7  Localisation                                                            
#> 8     Neck                6 (5.3%)          8 (3.1%)          1 (0.8%)     
#> 9     Head                9 (8.0%)         31 (12.2%)        24 (18.0%)    
#> 10    Trunk              47 (41.6%)        74 (29.1%)        46 (34.6%)    
#> 11    Upper Extremity    30 (26.5%)        94 (37.0%)        35 (26.3%)    
#> 12    Lower Extremity    20 (17.7%)        45 (17.7%)        23 (17.3%)    
#> 13    Unspecified         1 (0.9%)          2 (0.8%)          4 (3.0%)
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
#>                       benign (N=113) in situ (N=254) malignant (N=133)
#> 1  Age at Debut                                                       
#> 2     Mean                   49.0           49.9               49.7   
#> 3     SD                     19.2           19.1               18.5   
#> 4  Sex                                                                
#> 5     Female              47 (41.6%)     123 (48.4%)        74 (55.6%)
#> 6     Male                66 (58.4%)     131 (51.6%)        59 (44.4%)
#> 7  Localisation                                                       
#> 8     Neck                 6 (5.3%)       8 (3.1%)           1 (0.8%) 
#> 9     Head                 9 (8.0%)      31 (12.2%)         24 (18.0%)
#> 10    Trunk               47 (41.6%)     74 (29.1%)         46 (34.6%)
#> 11    Upper Extremity     30 (26.5%)     94 (37.0%)         35 (26.3%)
#> 12    Lower Extremity     20 (17.7%)     45 (17.7%)         23 (17.3%)
#> 13    Unspecified          1 (0.9%)       2 (0.8%)           4 (3.0%)
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
