# Data Management

## Introduction

This article is a thorough guide to data management in our. It mainly
uses the dplyr package with additional cancR functions. The article is
structured as chapters, where each chapter describes a specific task and
should work as a library.

### Loading the cancR package

First we load the cancR package. The package automatically loads many
packages useful for data management

``` r
library(cancR)
```

### Data

The cancR package comes with ready-to-use datasets. In this article we
use the redcap_df dataset which imitates a dataset exported directly
from redcap.

### Combining functions with the piping operator

It is advised to combine the functions described in this article into
one code chunk that runs all functions at once. The functions are
combined with the symbol `%>%` called a pipe. The shortcut for a pipe is
ctrl+shift+m.

Piping starts by specifying the dataset of which the analyses should be
performed. After this all subsequent functions are separated by a pipe.
In the following example we start in the dataset “redcap_df”, where we
subsequently select the variables id, sex and birth, add a new variable
called “new_variable” and lastly filter so that we subset the dataset to
rows where type = 1. All these functions are combined into a piping
structure and assigned to a new object named “new_data”

``` r
new_data <- redcap_df %>% 
  select(id, sex, birth, type) %>% 
  mutate(new_variable = "new") %>% 
  filter(type == 1)
```

## Data inspection

Before starting on data management, it is important to get an overview
of the dataset.

The entire dataset can be expected by either clicking on the dataset in
the environment below or with the command
[`View()`](https://rdrr.io/r/utils/View.html)

For a simpler inspection, [`head()`](https://rdrr.io/r/utils/head.html)
shows only the first six rows in the console

``` r
head(redcap_df)
#>   id sex  age      birth   followup date_of_surgery      size type localisation
#> 1  1   1 40.9 26-04-1958 23-04-2021      1999-03-29  3.991349    0            2
#> 2  2   2 57.8 18-01-1941 12-08-2024      1998-10-20 48.688670    0            1
#> 3  3   2 32.2 30-07-1963 20-02-2022      1995-10-26 11.245841    0            4
#> 4  4   2 58.7 31-12-1936 29-06-2024      1995-09-14 11.609164    2            1
#> 5  5   2 29.3 12-01-1961 14-05-2023      1990-04-22  6.980464    0            3
#> 6  6   2 18.3 10-12-1975 06-04-2025      1994-03-21 49.029171    2            0
#>   necrosis cd10 sox10 ck death_date recurrence_date metastasis_date
#> 1        0   NA     1  0       <NA>            <NA>            <NA>
#> 2        1    0     0  0       <NA>      2014-12-09            <NA>
#> 3        0    0     1 NA       <NA>            <NA>            <NA>
#> 4        1    1    NA  0 2019-07-27            <NA>      2012-04-04
#> 5        0    0     0  0 2018-04-19            <NA>      2011-01-16
#> 6       NA   NA    NA  0       <NA>            <NA>            <NA>
```

All column names can be shown with
[`names()`](https://rdrr.io/r/base/names.html)

``` r
names(redcap_df)
#>  [1] "id"              "sex"             "age"             "birth"          
#>  [5] "followup"        "date_of_surgery" "size"            "type"           
#>  [9] "localisation"    "necrosis"        "cd10"            "sox10"          
#> [13] "ck"              "death_date"      "recurrence_date" "metastasis_date"
```

It is also important to assess the structure of the data to check for
correct formatting. E.g. are date-variables coded as dates, continouous
variables as numeric etc.

``` r
str(redcap_df)
#> 'data.frame':    500 obs. of  16 variables:
#>  $ id             : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ sex            : num  1 2 2 2 2 2 2 1 1 2 ...
#>  $ age            : num  40.9 57.8 32.2 58.7 29.3 18.3 52.7 36.2 42.6 55.8 ...
#>  $ birth          : chr  "26-04-1958" "18-01-1941" "30-07-1963" "31-12-1936" ...
#>  $ followup       : chr  "23-04-2021" "12-08-2024" "20-02-2022" "29-06-2024" ...
#>  $ date_of_surgery: chr  "1999-03-29" "1998-10-20" "1995-10-26" "1995-09-14" ...
#>  $ size           : num  3.99 48.69 11.25 11.61 6.98 ...
#>  $ type           : int  0 0 0 2 0 2 0 2 2 1 ...
#>  $ localisation   : int  2 1 4 1 3 0 3 5 4 1 ...
#>  $ necrosis       : num  0 1 0 1 0 NA 1 NA 0 0 ...
#>  $ cd10           : num  NA 0 0 1 0 NA 1 1 1 NA ...
#>  $ sox10          : num  1 0 1 NA 0 NA 0 0 0 1 ...
#>  $ ck             : num  0 0 NA 0 0 0 NA 0 0 1 ...
#>  $ death_date     : chr  NA NA NA "2019-07-27" ...
#>  $ recurrence_date: chr  NA "2014-12-09" NA NA ...
#>  $ metastasis_date: chr  NA NA NA "2012-04-04" ...
```

Here we see that all date variables are coded as characters and not
date. The conversion to date are described in the chapter: “Date
formatting”

To get a graphical glimpse of the data we can use the summarisR()
function:

``` r
summarisR(redcap_df, 
          vars = c(sex, size, type, localisation, necrosis, cd10, sox10, ck))
```

![](data_management_files/figure-html/unnamed-chunk-7-1.png)

And to exploit the number of missing values we use the missR() function

``` r
missR(redcap_df)
#> Nas detected in the following variables:
#> 
#>          variable NAs
#> 1 metastasis_date 329
#> 2      death_date 322
#> 3 recurrence_date 261
#> 4              ck 174
#> 5        necrosis 169
#> 6            cd10 167
#> 7           sox10 164
```

We can also check if numerical variables are normally distributed with
the distributR() function

``` r
distributR(redcap_df,
           vars = size)
```

![](data_management_files/figure-html/unnamed-chunk-9-1.png)

## Data management

The next section goes through the most basic data management functions
from the dplyr package.

### Selection of variables

Variables/columns can be selected and removed with the select()
function.

``` r
redcap_df %>% 
  select(id, sex, birth) %>% 
  head
#>   id sex      birth
#> 1  1   1 26-04-1958
#> 2  2   2 18-01-1941
#> 3  3   2 30-07-1963
#> 4  4   2 31-12-1936
#> 5  5   2 12-01-1961
#> 6  6   2 10-12-1975
```

Variables are removed with a minus sign.

``` r
redcap_df %>% 
  select(-id, -birth) %>% 
  head
#>   sex  age   followup date_of_surgery      size type localisation necrosis cd10
#> 1   1 40.9 23-04-2021      1999-03-29  3.991349    0            2        0   NA
#> 2   2 57.8 12-08-2024      1998-10-20 48.688670    0            1        1    0
#> 3   2 32.2 20-02-2022      1995-10-26 11.245841    0            4        0    0
#> 4   2 58.7 29-06-2024      1995-09-14 11.609164    2            1        1    1
#> 5   2 29.3 14-05-2023      1990-04-22  6.980464    0            3        0    0
#> 6   2 18.3 06-04-2025      1994-03-21 49.029171    2            0       NA   NA
#>   sox10 ck death_date recurrence_date metastasis_date
#> 1     1  0       <NA>            <NA>            <NA>
#> 2     0  0       <NA>      2014-12-09            <NA>
#> 3     1 NA       <NA>            <NA>            <NA>
#> 4    NA  0 2019-07-27            <NA>      2012-04-04
#> 5     0  0 2018-04-19            <NA>      2011-01-16
#> 6    NA  0       <NA>            <NA>            <NA>
```

It is also possible to choose variable based on text patterns, which is
useful for variables with a common prefix/suffix such as \_date

``` r
redcap_df %>% 
  select(contains("_date")) %>% 
  head
#>   death_date recurrence_date metastasis_date
#> 1       <NA>            <NA>            <NA>
#> 2       <NA>      2014-12-09            <NA>
#> 3       <NA>            <NA>            <NA>
#> 4 2019-07-27            <NA>      2012-04-04
#> 5 2018-04-19            <NA>      2011-01-16
#> 6       <NA>            <NA>            <NA>
```

The text pattern can also be starts_with, ends_with and matches for an
exact match.

If we need to select a large range of variables we call the first and
last separated by a colon:

``` r
redcap_df %>% 
  select(sex:sox10) %>% 
  head
#>   sex  age      birth   followup date_of_surgery      size type localisation
#> 1   1 40.9 26-04-1958 23-04-2021      1999-03-29  3.991349    0            2
#> 2   2 57.8 18-01-1941 12-08-2024      1998-10-20 48.688670    0            1
#> 3   2 32.2 30-07-1963 20-02-2022      1995-10-26 11.245841    0            4
#> 4   2 58.7 31-12-1936 29-06-2024      1995-09-14 11.609164    2            1
#> 5   2 29.3 12-01-1961 14-05-2023      1990-04-22  6.980464    0            3
#> 6   2 18.3 10-12-1975 06-04-2025      1994-03-21 49.029171    2            0
#>   necrosis cd10 sox10
#> 1        0   NA     1
#> 2        1    0     0
#> 3        0    0     1
#> 4        1    1    NA
#> 5        0    0     0
#> 6       NA   NA    NA
```

### Renaming variables

Renaming of variable names can be done using rename(). The syntax is
“new name” = “old name”

``` r
redcap_df %>% 
  rename(index = date_of_surgery,
         cytokeratin = ck) %>% 
  head
#>   id sex  age      birth   followup      index      size type localisation
#> 1  1   1 40.9 26-04-1958 23-04-2021 1999-03-29  3.991349    0            2
#> 2  2   2 57.8 18-01-1941 12-08-2024 1998-10-20 48.688670    0            1
#> 3  3   2 32.2 30-07-1963 20-02-2022 1995-10-26 11.245841    0            4
#> 4  4   2 58.7 31-12-1936 29-06-2024 1995-09-14 11.609164    2            1
#> 5  5   2 29.3 12-01-1961 14-05-2023 1990-04-22  6.980464    0            3
#> 6  6   2 18.3 10-12-1975 06-04-2025 1994-03-21 49.029171    2            0
#>   necrosis cd10 sox10 cytokeratin death_date recurrence_date metastasis_date
#> 1        0   NA     1           0       <NA>            <NA>            <NA>
#> 2        1    0     0           0       <NA>      2014-12-09            <NA>
#> 3        0    0     1          NA       <NA>            <NA>            <NA>
#> 4        1    1    NA           0 2019-07-27            <NA>      2012-04-04
#> 5        0    0     0           0 2018-04-19            <NA>      2011-01-16
#> 6       NA   NA    NA           0       <NA>            <NA>            <NA>
```

### Create/modify variables

Variables can be created or modified with the mutate() function with the
syntax: mutate(variable = condition). If the variable already exists in
the dataset, it is modified automatically.

We now recode necrosis, so that 1 = yes and everything else is “no”.

``` r
redcap_df %>% 
  mutate(necrosis = ifelse(necrosis == 1, "yes", "no")) %>% 
  head
#>   id sex  age      birth   followup date_of_surgery      size type localisation
#> 1  1   1 40.9 26-04-1958 23-04-2021      1999-03-29  3.991349    0            2
#> 2  2   2 57.8 18-01-1941 12-08-2024      1998-10-20 48.688670    0            1
#> 3  3   2 32.2 30-07-1963 20-02-2022      1995-10-26 11.245841    0            4
#> 4  4   2 58.7 31-12-1936 29-06-2024      1995-09-14 11.609164    2            1
#> 5  5   2 29.3 12-01-1961 14-05-2023      1990-04-22  6.980464    0            3
#> 6  6   2 18.3 10-12-1975 06-04-2025      1994-03-21 49.029171    2            0
#>   necrosis cd10 sox10 ck death_date recurrence_date metastasis_date
#> 1       no   NA     1  0       <NA>            <NA>            <NA>
#> 2      yes    0     0  0       <NA>      2014-12-09            <NA>
#> 3       no    0     1 NA       <NA>            <NA>            <NA>
#> 4      yes    1    NA  0 2019-07-27            <NA>      2012-04-04
#> 5       no    0     0  0 2018-04-19            <NA>      2011-01-16
#> 6     <NA>   NA    NA  0       <NA>            <NA>            <NA>
```

Notice that missing values in necrosis remain missing. If we want to
also assign these as “no” we change the `==` in the mutate function to
`%in%`. This will also imply that missing values are converted to the
“else” statement, here “no”.

``` r
redcap_df %>% 
  mutate(necrosis = ifelse(necrosis %in% 1, "yes", "no")) %>% 
  head
#>   id sex  age      birth   followup date_of_surgery      size type localisation
#> 1  1   1 40.9 26-04-1958 23-04-2021      1999-03-29  3.991349    0            2
#> 2  2   2 57.8 18-01-1941 12-08-2024      1998-10-20 48.688670    0            1
#> 3  3   2 32.2 30-07-1963 20-02-2022      1995-10-26 11.245841    0            4
#> 4  4   2 58.7 31-12-1936 29-06-2024      1995-09-14 11.609164    2            1
#> 5  5   2 29.3 12-01-1961 14-05-2023      1990-04-22  6.980464    0            3
#> 6  6   2 18.3 10-12-1975 06-04-2025      1994-03-21 49.029171    2            0
#>   necrosis cd10 sox10 ck death_date recurrence_date metastasis_date
#> 1       no   NA     1  0       <NA>            <NA>            <NA>
#> 2      yes    0     0  0       <NA>      2014-12-09            <NA>
#> 3       no    0     1 NA       <NA>            <NA>            <NA>
#> 4      yes    1    NA  0 2019-07-27            <NA>      2012-04-04
#> 5       no    0     0  0 2018-04-19            <NA>      2011-01-16
#> 6       no   NA    NA  0       <NA>            <NA>            <NA>
```

If we want more explicit control with the recoding or we have more than
one condition, we use case_when

``` r
redcap_df %>% 
  mutate(necrosis = case_when(necrosis == 1 ~ "yes",
                              necrosis == 0 ~ "no")) %>% 
  head
#>   id sex  age      birth   followup date_of_surgery      size type localisation
#> 1  1   1 40.9 26-04-1958 23-04-2021      1999-03-29  3.991349    0            2
#> 2  2   2 57.8 18-01-1941 12-08-2024      1998-10-20 48.688670    0            1
#> 3  3   2 32.2 30-07-1963 20-02-2022      1995-10-26 11.245841    0            4
#> 4  4   2 58.7 31-12-1936 29-06-2024      1995-09-14 11.609164    2            1
#> 5  5   2 29.3 12-01-1961 14-05-2023      1990-04-22  6.980464    0            3
#> 6  6   2 18.3 10-12-1975 06-04-2025      1994-03-21 49.029171    2            0
#>   necrosis cd10 sox10 ck death_date recurrence_date metastasis_date
#> 1       no   NA     1  0       <NA>            <NA>            <NA>
#> 2      yes    0     0  0       <NA>      2014-12-09            <NA>
#> 3       no    0     1 NA       <NA>            <NA>            <NA>
#> 4      yes    1    NA  0 2019-07-27            <NA>      2012-04-04
#> 5       no    0     0  0 2018-04-19            <NA>      2011-01-16
#> 6     <NA>   NA    NA  0       <NA>            <NA>            <NA>
```

Now we have preserved the missing values. We can also control what to do
with values that does not satisfy any of the criteria

``` r
redcap_df %>% 
  mutate(necrosis = case_when(necrosis %in% 1 ~ "yes",
                              necrosis %in% 0 ~ "no",
                              T ~ "missing")) %>% 
  head
#>   id sex  age      birth   followup date_of_surgery      size type localisation
#> 1  1   1 40.9 26-04-1958 23-04-2021      1999-03-29  3.991349    0            2
#> 2  2   2 57.8 18-01-1941 12-08-2024      1998-10-20 48.688670    0            1
#> 3  3   2 32.2 30-07-1963 20-02-2022      1995-10-26 11.245841    0            4
#> 4  4   2 58.7 31-12-1936 29-06-2024      1995-09-14 11.609164    2            1
#> 5  5   2 29.3 12-01-1961 14-05-2023      1990-04-22  6.980464    0            3
#> 6  6   2 18.3 10-12-1975 06-04-2025      1994-03-21 49.029171    2            0
#>   necrosis cd10 sox10 ck death_date recurrence_date metastasis_date
#> 1       no   NA     1  0       <NA>            <NA>            <NA>
#> 2      yes    0     0  0       <NA>      2014-12-09            <NA>
#> 3       no    0     1 NA       <NA>            <NA>            <NA>
#> 4      yes    1    NA  0 2019-07-27            <NA>      2012-04-04
#> 5       no    0     0  0 2018-04-19            <NA>      2011-01-16
#> 6  missing   NA    NA  0       <NA>            <NA>            <NA>
```

#### Collecting multiple mutate functions

Multiple mutate functions can be collected in one call

``` r
redcap_df %>% 
  mutate(new_variable = "new",
         sex = ifelse(sex == 1, "f", "m"),
         size = case_when(size > 40 ~ "large",
                          size < 10 ~ "small",
                          T ~ "intermediate")) %>% 
  head
#>   id sex  age      birth   followup date_of_surgery         size type
#> 1  1   f 40.9 26-04-1958 23-04-2021      1999-03-29        small    0
#> 2  2   m 57.8 18-01-1941 12-08-2024      1998-10-20        large    0
#> 3  3   m 32.2 30-07-1963 20-02-2022      1995-10-26 intermediate    0
#> 4  4   m 58.7 31-12-1936 29-06-2024      1995-09-14 intermediate    2
#> 5  5   m 29.3 12-01-1961 14-05-2023      1990-04-22        small    0
#> 6  6   m 18.3 10-12-1975 06-04-2025      1994-03-21        large    2
#>   localisation necrosis cd10 sox10 ck death_date recurrence_date
#> 1            2        0   NA     1  0       <NA>            <NA>
#> 2            1        1    0     0  0       <NA>      2014-12-09
#> 3            4        0    0     1 NA       <NA>            <NA>
#> 4            1        1    1    NA  0 2019-07-27            <NA>
#> 5            3        0    0     0  0 2018-04-19            <NA>
#> 6            0       NA   NA    NA  0       <NA>            <NA>
#>   metastasis_date new_variable
#> 1            <NA>          new
#> 2            <NA>          new
#> 3            <NA>          new
#> 4      2012-04-04          new
#> 5      2011-01-16          new
#> 6            <NA>          new
```

#### Mutating multiple variables simultaneously

Multiple variables can be modified with
[`across()`](https://dplyr.tidyverse.org/reference/across.html) within
mutate. The syntax is:  

\`mutate(across(c(variable1, variable2), ~ function))

Here we convert the variables cd10, sox10 and ck to characters

``` r
redcap_df %>% 
  select(cd10, sox10, ck) %>% 
  mutate(across(c(cd10, sox10, ck), ~ as.character(.))) %>% 
  head
#>   cd10 sox10   ck
#> 1 <NA>     1    0
#> 2    0     0    0
#> 3    0     1 <NA>
#> 4    1  <NA>    0
#> 5    0     0    0
#> 6 <NA>  <NA>    0
```

The `.` inside the `as.character(.)` refers to all the variables inside
[`across()`](https://dplyr.tidyverse.org/reference/across.html).

### Recoding of variables

Recoding of variables can be done with the recodR() function in the
cancR package. The syntax here is a list of lists, so that list(variable
= list(new name = old name))

``` r
redcap_df %>% 
  recodR(list("sex" = 
                list("female" = 1,
                           "male" = 2),
              "type" = 
                list("benign" = 0,
                            "in_situ" = 1,
                            "malignant" = 2),
              "localisation" = 
                list("head" = 1,
                     "neck" = 2,
                     "trunk" = 3,
                     "upper_extremity" = 4,
                     "lower_extremity" = 5))) %>% 
  head
#>   id    sex  age      birth   followup date_of_surgery      size      type
#> 1  1 female 40.9 26-04-1958 23-04-2021      1999-03-29  3.991349    benign
#> 2  2   male 57.8 18-01-1941 12-08-2024      1998-10-20 48.688670    benign
#> 3  3   male 32.2 30-07-1963 20-02-2022      1995-10-26 11.245841    benign
#> 4  4   male 58.7 31-12-1936 29-06-2024      1995-09-14 11.609164 malignant
#> 5  5   male 29.3 12-01-1961 14-05-2023      1990-04-22  6.980464    benign
#> 6  6   male 18.3 10-12-1975 06-04-2025      1994-03-21 49.029171 malignant
#>      localisation necrosis cd10 sox10 ck death_date recurrence_date
#> 1            neck        0   NA     1  0       <NA>            <NA>
#> 2            head        1    0     0  0       <NA>      2014-12-09
#> 3 upper_extremity        0    0     1 NA       <NA>            <NA>
#> 4            head        1    1    NA  0 2019-07-27            <NA>
#> 5           trunk        0    0     0  0 2018-04-19            <NA>
#> 6               0       NA   NA    NA  0       <NA>            <NA>
#>   metastasis_date
#> 1            <NA>
#> 2            <NA>
#> 3            <NA>
#> 4      2012-04-04
#> 5      2011-01-16
#> 6            <NA>
```

If the recoding should be more advanced and should be based on one or
more conditions, we can use ifelse() or case_when() (see examples under
“Create/modify variables”).

### Date formatting

Dates are often formatted as character strings and need to be converted
to correct date format. This can easily be done with the datR()
function:

``` r
redcap_df %>% 
  datR(c(contains("date"), birth, followup)) %>% 
  head
#>   id sex  age      birth   followup date_of_surgery      size type localisation
#> 1  1   1 40.9 1958-04-26 2021-04-23      1999-03-29  3.991349    0            2
#> 2  2   2 57.8 1941-01-18 2024-08-12      1998-10-20 48.688670    0            1
#> 3  3   2 32.2 1963-07-30 2022-02-20      1995-10-26 11.245841    0            4
#> 4  4   2 58.7 1936-12-31 2024-06-29      1995-09-14 11.609164    2            1
#> 5  5   2 29.3 1961-01-12 2023-05-14      1990-04-22  6.980464    0            3
#> 6  6   2 18.3 1975-12-10 2025-04-06      1994-03-21 49.029171    2            0
#>   necrosis cd10 sox10 ck death_date recurrence_date metastasis_date
#> 1        0   NA     1  0       <NA>            <NA>            <NA>
#> 2        1    0     0  0       <NA>      2014-12-09            <NA>
#> 3        0    0     1 NA       <NA>            <NA>            <NA>
#> 4        1    1    NA  0 2019-07-27            <NA>      2012-04-04
#> 5        0    0     0  0 2018-04-19            <NA>      2011-01-16
#> 6       NA   NA    NA  0       <NA>            <NA>            <NA>
```

### Categorization of continuous variables

The optimal method for splitting continuous variables depends on the
number of splits: - One splits: ifelse() - More than one split:
case_when() - Splits based on a sequence or quantiles: cutR()

One split with ifelse()

``` r
redcap_df %>% 
  mutate(size_bin = ifelse(size > 20, "large", "small")) %>% 
  head
#>   id sex  age      birth   followup date_of_surgery      size type localisation
#> 1  1   1 40.9 26-04-1958 23-04-2021      1999-03-29  3.991349    0            2
#> 2  2   2 57.8 18-01-1941 12-08-2024      1998-10-20 48.688670    0            1
#> 3  3   2 32.2 30-07-1963 20-02-2022      1995-10-26 11.245841    0            4
#> 4  4   2 58.7 31-12-1936 29-06-2024      1995-09-14 11.609164    2            1
#> 5  5   2 29.3 12-01-1961 14-05-2023      1990-04-22  6.980464    0            3
#> 6  6   2 18.3 10-12-1975 06-04-2025      1994-03-21 49.029171    2            0
#>   necrosis cd10 sox10 ck death_date recurrence_date metastasis_date size_bin
#> 1        0   NA     1  0       <NA>            <NA>            <NA>    small
#> 2        1    0     0  0       <NA>      2014-12-09            <NA>    large
#> 3        0    0     1 NA       <NA>            <NA>            <NA>    small
#> 4        1    1    NA  0 2019-07-27            <NA>      2012-04-04    small
#> 5        0    0     0  0 2018-04-19            <NA>      2011-01-16    small
#> 6       NA   NA    NA  0       <NA>            <NA>            <NA>    large
```

More splits with case_when()

``` r
redcap_df %>% 
  mutate(size_bin = case_when(size > 40 ~ "large",
                          size < 10 ~ "small",
                          T ~ "intermediate")) %>% 
  head
#>   id sex  age      birth   followup date_of_surgery      size type localisation
#> 1  1   1 40.9 26-04-1958 23-04-2021      1999-03-29  3.991349    0            2
#> 2  2   2 57.8 18-01-1941 12-08-2024      1998-10-20 48.688670    0            1
#> 3  3   2 32.2 30-07-1963 20-02-2022      1995-10-26 11.245841    0            4
#> 4  4   2 58.7 31-12-1936 29-06-2024      1995-09-14 11.609164    2            1
#> 5  5   2 29.3 12-01-1961 14-05-2023      1990-04-22  6.980464    0            3
#> 6  6   2 18.3 10-12-1975 06-04-2025      1994-03-21 49.029171    2            0
#>   necrosis cd10 sox10 ck death_date recurrence_date metastasis_date
#> 1        0   NA     1  0       <NA>            <NA>            <NA>
#> 2        1    0     0  0       <NA>      2014-12-09            <NA>
#> 3        0    0     1 NA       <NA>            <NA>            <NA>
#> 4        1    1    NA  0 2019-07-27            <NA>      2012-04-04
#> 5        0    0     0  0 2018-04-19            <NA>      2011-01-16
#> 6       NA   NA    NA  0       <NA>            <NA>            <NA>
#>       size_bin
#> 1        small
#> 2        large
#> 3 intermediate
#> 4 intermediate
#> 5        small
#> 6        large
```

Splits based on a sequence can be done with the cutR() function:

``` r
redcap_df %>% 
  cutR(size, 
       seq(0,50,10)) %>% 
  head
#>   id sex  age      birth   followup date_of_surgery  size type localisation
#> 1  1   1 40.9 26-04-1958 23-04-2021      1999-03-29  0-10    0            2
#> 2  2   2 57.8 18-01-1941 12-08-2024      1998-10-20 40-50    0            1
#> 3  3   2 32.2 30-07-1963 20-02-2022      1995-10-26 10-20    0            4
#> 4  4   2 58.7 31-12-1936 29-06-2024      1995-09-14 10-20    2            1
#> 5  5   2 29.3 12-01-1961 14-05-2023      1990-04-22  0-10    0            3
#> 6  6   2 18.3 10-12-1975 06-04-2025      1994-03-21 40-50    2            0
#>   necrosis cd10 sox10 ck death_date recurrence_date metastasis_date
#> 1        0   NA     1  0       <NA>            <NA>            <NA>
#> 2        1    0     0  0       <NA>      2014-12-09            <NA>
#> 3        0    0     1 NA       <NA>            <NA>            <NA>
#> 4        1    1    NA  0 2019-07-27            <NA>      2012-04-04
#> 5        0    0     0  0 2018-04-19            <NA>      2011-01-16
#> 6       NA   NA    NA  0       <NA>            <NA>            <NA>
```

Multiple splits can also be performed with cutR() with name assigning

``` r
redcap_df %>%
  cutR(vars = c(age, size),
       seqlist = list("age" = seq(0,100,10),
                      "size" = list("quantile", c(0,0.25,0.5,0.75,1))),
       name.list = list("age" = "age_group",
                        "size" = "size_bin")) %>% 
  head
#>   id sex  age      birth   followup date_of_surgery      size type localisation
#> 1  1   1 40.9 26-04-1958 23-04-2021      1999-03-29  3.991349    0            2
#> 2  2   2 57.8 18-01-1941 12-08-2024      1998-10-20 48.688670    0            1
#> 3  3   2 32.2 30-07-1963 20-02-2022      1995-10-26 11.245841    0            4
#> 4  4   2 58.7 31-12-1936 29-06-2024      1995-09-14 11.609164    2            1
#> 5  5   2 29.3 12-01-1961 14-05-2023      1990-04-22  6.980464    0            3
#> 6  6   2 18.3 10-12-1975 06-04-2025      1994-03-21 49.029171    2            0
#>   necrosis cd10 sox10 ck death_date recurrence_date metastasis_date age_group
#> 1        0   NA     1  0       <NA>            <NA>            <NA>     40-50
#> 2        1    0     0  0       <NA>      2014-12-09            <NA>     50-60
#> 3        0    0     1 NA       <NA>            <NA>            <NA>     30-40
#> 4        1    1    NA  0 2019-07-27            <NA>      2012-04-04     50-60
#> 5        0    0     0  0 2018-04-19            <NA>      2011-01-16     20-30
#> 6       NA   NA    NA  0       <NA>            <NA>            <NA>     10-20
#>   size_bin
#> 1     2-14
#> 2    39-50
#> 3     2-14
#> 4     2-14
#> 5     2-14
#> 6    39-50
```

The new variables can also be given the same name pattern if the input
variables are similar such as dates

``` r
redcap_df %>% 
  #Conversion into date format
  datR(contains("date")) %>% 
  cutR(vars = c(recurrence_date, metastasis_date),
       seqlist = seq(1900,2030,10),
       name.pattern = "_bin")
#>      id sex  age      birth   followup date_of_surgery      size type
#> 1     1   1 40.9 26-04-1958 23-04-2021      1999-03-29  3.991349    0
#> 2     2   2 57.8 18-01-1941 12-08-2024      1998-10-20 48.688670    0
#> 3     3   2 32.2 30-07-1963 20-02-2022      1995-10-26 11.245841    0
#> 4     4   2 58.7 31-12-1936 29-06-2024      1995-09-14 11.609164    2
#> 5     5   2 29.3 12-01-1961 14-05-2023      1990-04-22  6.980464    0
#> 6     6   2 18.3 10-12-1975 06-04-2025      1994-03-21 49.029171    2
#> 7     7   2 52.7 14-04-1945 19-03-2024      1997-12-19 48.978832    0
#> 8     8   1 36.2 01-09-1969 14-10-2024      2005-10-30 43.577441    2
#> 9     9   1 42.6 01-04-1962 25-01-2021      2004-11-09  8.699874    2
#> 10   10   2 55.8 04-01-1954 02-04-2024      2009-10-22 35.560789    1
#> 11   11   1 67.4 26-09-1930 21-06-2023      1998-02-24 27.234067    0
#> 12   12   2 46.0 08-05-1962 17-02-2025      2008-04-23 11.060677    1
#> 13   13   2 54.2 13-09-1948 01-07-2021      2002-11-15 40.521277    2
#> 14   14   1 39.6 16-08-1950 24-10-2022      1990-03-12 32.026478    1
#> 15   15   1 70.7 26-03-1939 05-07-2023      2009-11-18 15.910118    0
#> 16   16   1 58.8 01-07-1945 19-06-2024      2004-04-01 26.707943    1
#> 17   17   1 56.8 27-09-1946 05-09-2022      2003-07-16 47.472564    1
#> 18   18   2 32.0 24-05-1972 12-08-2022      2004-05-31 15.683065    0
#> 19   19   1 23.4 27-11-1975 17-11-2024      1999-05-08 14.008700    2
#> 20   20   1 30.3 01-09-1978 08-03-2023      2008-12-01 25.246919    1
#> 21   21   1 58.1 30-07-1935 20-07-2024      1993-09-17 33.808313    2
#> 22   22   1 36.5 07-01-1959 07-09-2022      1995-07-08 40.656375    2
#> 23   23   1 64.5 30-03-1929 21-09-2022      1993-10-10 28.811464    1
#> 24   24   2 46.9 11-03-1956 21-07-2021      2003-02-02 43.499568    1
#> 25   25   1 73.8 01-09-1935 31-08-2025      2009-06-16 22.116865    2
#> 26   26   1 31.8 03-03-1974 02-02-2021      2006-01-07 23.264187    2
#> 27   27   2 52.2 14-07-1946 05-06-2021      1998-10-07  9.470696    0
#> 28   28   2 51.5 17-06-1940 07-02-2021      1992-01-02  2.088167    0
#> 29   29   1 31.4 12-03-1977 28-07-2021      2008-07-28 46.733846    0
#> 30   30   1 57.5 12-05-1940 06-12-2022      1997-11-26 10.804406    1
#> 31   31   2 44.1 27-06-1954 11-07-2024      1998-08-04  7.348631    2
#> 32   32   2 81.9 04-11-1927 20-02-2022      2009-10-09 45.616307    0
#> 33   33   2 58.0 31-07-1941 29-11-2021      1999-07-22 26.452243    0
#> 34   34   2 64.6 16-07-1927 02-07-2023      1992-02-13 44.296574    0
#> 35   35   2 65.6 22-12-1925 26-02-2022      1991-07-15 31.041682    1
#> 36   36   1 37.4 08-01-1972 24-05-2025      2009-06-10 15.680843    1
#> 37   37   1 52.7 27-05-1940 07-09-2022      1993-01-29 24.687938    1
#> 38   38   2 82.7 08-01-1926 07-01-2021      2008-09-04  4.014951    0
#> 39   39   2 49.3 10-03-1945 18-07-2024      1994-06-25 42.780687    1
#> 40   40   2 19.1 05-06-1979 26-08-2021      1998-07-03 23.409384    0
#> 41   41   2 44.8 27-05-1963 10-06-2023      2008-03-07 31.865790    1
#> 42   42   2 18.7 09-04-1972 07-01-2022      1990-12-09 24.282276    2
#> 43   43   2 16.8 24-11-1973 27-04-2024      1990-09-16 28.743681    1
#> 44   44   1 70.4 16-10-1936 28-08-2025      2007-03-21 35.697078    1
#> 45   45   2 43.4 25-07-1965 20-05-2023      2008-12-23 18.212949    0
#> 46   46   1 13.4 08-10-1979 05-01-2023      1993-03-02 43.948013    0
#> 47   47   1 41.6 12-08-1963 22-09-2023      2005-03-18 42.103034    1
#> 48   48   1 20.3 02-06-1977 12-10-2024      1997-09-12 24.129047    0
#> 49   49   1 29.8 11-03-1963 20-03-2021      1993-01-12 10.193068    1
#> 50   50   2 38.5 17-12-1959 30-05-2023      1998-07-03 31.155631    1
#> 51   51   1 52.8 17-12-1940 28-11-2022      1993-10-07 42.959891    0
#> 52   52   2 85.3 03-05-1921 20-07-2022      2006-08-31 30.674709    0
#> 53   53   1 70.0 22-11-1924 17-06-2024      1994-12-05 45.147110    1
#> 54   54   1 37.9 09-01-1964 16-11-2021      2001-12-01 42.932966    1
#> 55   55   1 59.3 23-10-1931 02-02-2022      1991-02-15  9.169283    2
#> 56   56   2 29.5 16-12-1973 05-10-2024      2003-06-01 39.343639    0
#> 57   57   2 71.7 15-04-1924 13-01-2023      1995-12-14 23.241119    1
#> 58   58   2 62.1 29-05-1946 11-11-2021      2008-06-26 19.266183    1
#> 59   59   1 74.1 13-03-1927 27-10-2022      2001-04-08 26.571829    2
#> 60   60   1 41.4 05-01-1949 11-02-2022      1990-06-02 48.604305    1
#> 61   61   1 59.1 29-07-1932 23-07-2022      1991-09-02 47.616481    2
#> 62   62   2 52.2 19-07-1939 06-05-2023      1991-09-28 41.386443    1
#> 63   63   1 84.2 22-08-1925 05-10-2023      2009-11-11 42.151840    2
#> 64   64   1 67.2 22-05-1942 01-12-2022      2009-08-11 28.903493    2
#> 65   65   2 60.0 02-08-1946 02-05-2025      2006-07-16 43.379109    1
#> 66   66   1 59.7 29-04-1944 11-06-2021      2004-01-01 42.465384    1
#> 67   67   2 48.0 18-05-1954 21-05-2023      2002-05-11  7.521407    1
#> 68   68   2 67.1 14-06-1938 15-03-2022      2005-07-26  4.153029    1
#> 69   69   2 63.9 04-09-1932 04-12-2023      1996-08-15 45.754790    2
#> 70   70   1 78.9 28-10-1920 28-10-2023      1999-10-06 29.341309    2
#> 71   71   2 78.3 17-06-1926 25-05-2021      2004-09-18 34.479392    2
#> 72   72   2 40.2 08-07-1967 28-03-2021      2007-10-02 46.949310    2
#> 73   73   2 38.5 09-04-1954 28-07-2023      1992-10-10 44.303703    2
#> 74   74   2 74.0 26-12-1925 15-10-2025      2000-01-04 29.896589    0
#> 75   75   1 49.2 05-08-1954 05-11-2022      2003-10-11 42.710822    0
#> 76   76   2 24.8 07-07-1971 21-04-2025      1996-04-20 12.578559    1
#> 77   77   1 70.2 21-05-1923 13-08-2023      1993-07-21 17.336243    0
#> 78   78   1 39.9 05-02-1950 01-09-2024      1990-01-15  8.300089    1
#> 79   79   1 67.0 24-07-1928 27-12-2023      1995-07-10 34.802177    2
#> 80   80   2 34.8 24-07-1972 08-01-2025      2007-04-30  4.712726    1
#> 81   81   2 62.6 06-09-1935 01-06-2025      1998-04-05 43.920577    2
#> 82   82   2 75.7 05-06-1921 17-03-2021      1997-02-06 29.114070    1
#> 83   83   2 65.7 30-04-1925 26-06-2023      1991-01-26 37.619278    1
#> 84   84   2 61.9 13-05-1933 25-05-2022      1995-04-13 40.729330    1
#> 85   85   1 34.8 10-07-1959 07-08-2025      1994-04-12 40.275932    1
#> 86   86   2 76.8 07-03-1930 07-08-2024      2006-12-23 13.314298    2
#> 87   87   1 39.5 01-04-1965 08-04-2023      2004-09-15 30.560904    1
#> 88   88   2 77.0 23-02-1921 28-08-2022      1998-02-27 29.408628    0
#> 89   89   2 62.6 21-10-1936 05-11-2023      1999-05-19  7.309463    2
#> 90   90   1 68.2 26-11-1924 14-06-2022      1993-02-22  6.307458    1
#> 91   91   2 29.7 29-07-1965 11-03-2022      1995-03-30 11.280840    0
#> 92   92   1 56.8 18-02-1937 01-08-2021      1993-12-12 47.831321    1
#> 93   93   2 79.7 30-05-1924 03-10-2021      2004-02-09 49.889953    0
#> 94   94   1 50.6 09-08-1945 30-07-2024      1996-03-17 16.631674    0
#> 95   95   2 73.2 05-04-1923 26-09-2024      1996-06-07 32.821859    2
#> 96   96   1 68.7 04-07-1930 18-09-2025      1999-03-24  5.328178    1
#> 97   97   2 32.8 21-09-1969 24-10-2023      2002-07-26 45.153776    1
#> 98   98   1 84.1 20-02-1922 08-01-2025      2006-04-02 44.466058    0
#> 99   99   2 45.1 12-10-1959 17-09-2025      2004-11-10 40.796444    0
#> 100 100   1 43.3 01-04-1959 09-05-2021      2002-08-03 12.898573    2
#> 101 101   1 53.7 24-03-1951 30-08-2025      2004-12-01 33.574683    1
#> 102 102   2 65.7 22-02-1942 13-05-2024      2007-11-16 41.635655    0
#> 103 103   1 48.0 14-01-1942 01-07-2025      1990-01-31 48.686526    1
#> 104 104   2 52.6 11-05-1949 05-03-2024      2001-12-07  8.618654    2
#> 105 105   2 42.0 13-05-1950 12-02-2024      1992-05-20 44.745530    2
#> 106 106   2 47.8 01-08-1948 05-06-2024      1996-05-19  6.092790    0
#> 107 107   2 34.2 29-04-1961 07-05-2024      1995-07-24 14.355257    0
#> 108 108   1 24.0 09-10-1972 30-10-2023      1996-10-27 32.547667    0
#> 109 109   2 14.5 19-05-1980 07-08-2021      1994-11-19 42.002756    2
#> 110 110   1 21.4 20-04-1971 14-12-2021      1992-09-20 49.281281    1
#> 111 111   1 80.6 02-05-1922 28-07-2022      2002-11-23  6.213211    2
#> 112 112   2 65.8 03-07-1931 30-05-2023      1997-04-13 32.191060    2
#> 113 113   1 59.1 18-12-1933 05-03-2022      1993-01-10 29.012219    1
#> 114 114   1 33.7 21-08-1968 06-12-2023      2002-05-15 48.506308    0
#> 115 115   1 55.9 22-03-1943 29-06-2024      1999-02-18 35.669890    2
#> 116 116   1 32.6 14-09-1966 06-10-2024      1999-04-24  3.187798    0
#> 117 117   2 32.6 20-09-1959 09-02-2025      1992-04-13  9.081231    2
#> 118 118   2 68.0 17-05-1936 26-06-2022      2004-05-08 43.520691    1
#> 119 119   2 39.0 23-05-1965 13-02-2025      2004-05-20  9.715573    1
#> 120 120   1 57.5 27-06-1943 01-05-2024      2000-12-23 27.396645    1
#> 121 121   1 40.5 18-11-1961 24-09-2025      2002-05-18 43.743490    1
#> 122 122   1 79.4 16-11-1928 05-11-2021      2008-04-11 29.094798    2
#> 123 123   2 68.0 17-05-1940 05-11-2024      2008-05-11 44.419099    2
#> 124 124   1 65.9 21-03-1929 19-12-2024      1995-02-21 32.723713    2
#> 125 125   1 30.8 18-12-1978 29-09-2024      2009-09-19 43.238188    2
#> 126 126   1 57.5 22-06-1948 08-07-2023      2005-12-14  5.427242    2
#> 127 127   1 31.0 22-11-1977 27-04-2024      2008-11-26 42.627430    0
#> 128 128   2 57.8 21-07-1942 15-07-2025      2000-05-26 22.959899    1
#> 129 129   2 53.8 26-04-1937 19-05-2024      1991-02-17  4.917862    2
#> 130 130   2 29.8 20-06-1978 26-09-2025      2008-04-11 47.355089    1
#> 131 131   1 16.6 23-05-1974 24-02-2022      1991-01-13 40.415487    2
#> 132 132   2 50.5 15-07-1953 07-02-2024      2004-01-06 41.441022    2
#> 133 133   1 65.1 02-02-1930 01-09-2022      1995-02-23 49.310100    1
#> 134 134   2 76.0 12-01-1927 12-07-2024      2003-01-09 41.401651    0
#> 135 135   1 30.1 24-09-1977 14-10-2022      2007-10-13 34.251138    2
#> 136 136   2 78.1 15-07-1924 03-07-2021      2002-08-14 13.324153    1
#> 137 137   1 54.2 15-12-1936 16-06-2021      1991-02-13 26.115708    0
#> 138 138   1 29.5 02-06-1961 09-06-2023      1990-12-05 32.136884    2
#> 139 139   2 58.2 13-08-1949 22-01-2021      2007-11-05 35.219618    1
#> 140 140   2 70.5 15-07-1930 17-02-2023      2001-01-29 14.561083    2
#> 141 141   1 20.9 15-12-1978 13-04-2023      1999-11-22 22.712597    1
#> 142 142   1 30.0 15-09-1970 11-08-2024      2000-09-17 13.847556    0
#> 143 143   1 32.6 16-07-1958 06-07-2022      1991-02-09 10.485425    1
#> 144 144   1 69.4 18-06-1934 13-12-2021      2003-11-21 15.360415    1
#> 145 145   1 12.5 22-01-1980 04-10-2021      1992-08-08 34.985108    1
#> 146 146   1 73.3 22-08-1934 17-11-2021      2007-12-16 34.544373    0
#> 147 147   2 58.6 27-04-1938 16-12-2025      1996-12-19 44.455509    1
#> 148 148   1 15.3 04-03-1977 11-02-2021      1992-06-10 27.360902    0
#> 149 149   2 79.1 26-07-1930 23-09-2021      2009-09-12 24.971597    2
#> 150 150   2 72.6 01-11-1923 22-09-2021      1996-06-13  8.618002    0
#> 151 151   1 66.9 06-03-1925 05-08-2021      1992-01-14 21.105545    2
#> 152 152   2 16.2 17-04-1976 16-10-2024      1992-07-01 43.146653    1
#> 153 153   2 22.2 08-01-1972 15-03-2022      1994-04-05 15.697355    2
#> 154 154   2 28.4 25-06-1964 08-04-2023      1992-11-02 41.226424    1
#> 155 155   1 72.5 03-03-1927 07-09-2025      1999-08-30 47.242959    2
#> 156 156   2 39.1 15-12-1954 21-09-2021      1994-01-03  9.877846    1
#> 157 157   1 54.8 14-06-1944 20-08-2022      1999-04-12 33.360556    0
#> 158 158   1 53.5 01-07-1950 19-04-2022      2004-01-07 28.560428    0
#> 159 159   1 42.2 14-01-1961 09-12-2021      2003-03-31  6.273928    1
#> 160 160   2 57.2 22-12-1932 09-11-2024      1990-02-24 22.310174    0
#> 161 161   2 35.0 24-10-1961 19-03-2025      1996-10-08 25.743271    0
#> 162 162   2 33.5 18-09-1973 07-06-2021      2007-03-18 43.046086    1
#> 163 163   1 70.6 03-09-1924 13-08-2022      1995-04-10  3.174722    1
#> 164 164   2 66.4 13-04-1932 20-09-2025      1998-09-11 31.559543    0
#> 165 165   2 22.0 09-03-1976 30-06-2022      1998-02-26 22.461918    0
#> 166 166   1 67.4 31-05-1927 26-06-2024      1994-10-15 24.347088    2
#> 167 167   2 70.7 01-10-1922 12-12-2025      1993-06-03 45.515778    2
#> 168 168   2 58.2 20-08-1946 29-09-2024      2004-10-20  8.050159    1
#> 169 169   1 74.5 26-01-1930 22-11-2024      2004-07-20  3.908747    0
#> 170 170   2 30.3 31-03-1979 07-10-2024      2009-07-22 15.671632    0
#> 171 171   2 76.9 12-06-1927 23-02-2023      2004-05-01 11.627237    1
#> 172 172   2 29.1 30-04-1971 11-03-2022      2000-05-23 24.211482    2
#> 173 173   2 28.6 28-09-1972 16-03-2023      2001-05-11 17.245995    1
#> 174 174   1 69.6 24-01-1928 16-07-2023      1997-09-17 27.918511    0
#> 175 175   1 77.2 07-10-1929 11-11-2023      2006-12-30 29.003186    0
#> 176 176   2 75.0 24-10-1934 01-05-2023      2009-10-08 47.179951    2
#> 177 177   2 54.2 22-09-1948 14-12-2024      2002-12-21 37.214041    0
#> 178 178   2 53.5 22-03-1940 10-06-2025      1993-10-04 40.571110    2
#> 179 179   2 22.5 14-03-1974 21-11-2023      1996-09-04 31.650669    0
#> 180 180   2 65.9 30-08-1940 22-07-2024      2006-08-01  4.810916    0
#> 181 181   2 38.0 26-08-1969 06-11-2023      2007-08-31 17.779180    2
#> 182 182   2 59.1 29-07-1935 08-06-2025      1994-09-05 19.528960    1
#> 183 183   2 44.8 11-03-1961 14-05-2021      2005-12-16 21.004991    2
#> 184 184   1 18.5 23-06-1980 13-03-2021      1998-12-25  6.877743    2
#> 185 185   1 58.3 10-12-1946 25-02-2021      2005-03-23 38.049310    0
#> 186 186   2 49.5 27-11-1949 24-08-2022      1999-05-18 46.204499    0
#> 187 187   2 69.9 16-03-1933 10-08-2025      2003-02-03 43.847337    0
#> 188 188   2 68.4 06-08-1940 10-08-2023      2008-12-12 48.393558    2
#> 189 189   2 55.5 28-10-1949 08-03-2024      2005-05-15  2.507209    2
#> 190 190   2 74.0 22-10-1928 14-03-2024      2002-11-02  9.570776    1
#> 191 191   1 11.0 01-12-1980 23-04-2021      1991-11-18 13.595515    1
#> 192 192   2 57.4 12-02-1933 17-12-2025      1990-07-21 31.525139    0
#> 193 193   2 61.7 09-04-1942 24-11-2024      2003-12-12 45.725093    0
#> 194 194   2 27.4 12-03-1979 02-11-2025      2006-08-21  2.561534    0
#> 195 195   2 49.6 16-12-1957 19-06-2023      2007-07-27 15.497624    0
#> 196 196   1 68.6 04-10-1936 08-09-2025      2005-05-03 44.780247    2
#> 197 197   2 80.1 11-09-1923 18-10-2025      2003-11-01 32.678994    1
#> 198 198   1 54.6 31-01-1954 02-08-2021      2008-09-16  4.073206    1
#> 199 199   1 38.5 03-05-1971 29-03-2022      2009-11-09 29.516926    2
#> 200 200   2 34.9 23-04-1973 04-08-2025      2008-04-03  6.937575    1
#> 201 201   1 73.2 17-06-1928 05-03-2023      2001-09-12 29.328339    2
#> 202 202   1 72.8 01-01-1920 15-01-2024      1992-11-04  6.124003    2
#> 203 203   2 61.7 26-05-1929 07-08-2022      1991-02-18 19.575270    0
#> 204 204   2 15.4 15-02-1976 03-05-2025      1991-07-24 37.531273    1
#> 205 205   1 34.6 30-10-1970 18-09-2022      2005-06-10 10.494451    2
#> 206 206   1 32.5 18-02-1976 05-02-2023      2008-08-09 18.338486    1
#> 207 207   1 40.5 14-07-1958 08-02-2024      1999-01-19  4.633283    1
#> 208 208   1 32.7 20-09-1960 03-12-2022      1993-06-19  5.052925    1
#> 209 209   2 64.5 29-10-1943 10-04-2022      2008-04-11 12.732812    2
#> 210 210   1 19.6 30-12-1974 24-06-2025      1994-08-09 41.280741    2
#> 211 211   2 67.0 16-11-1927 08-01-2023      1994-11-27 41.567116    0
#> 212 212   2 48.0 01-09-1955 17-01-2025      2003-09-01 38.183014    2
#> 213 213   1 88.5 08-06-1921 22-12-2025      2009-12-02 30.757135    0
#> 214 214   1 56.4 26-08-1947 23-02-2023      2004-01-16 27.107597    2
#> 215 215   1 38.6 20-03-1965 22-03-2021      2003-10-11 22.630745    2
#> 216 216   1 35.8 09-10-1960 03-02-2025      1996-08-09 30.001050    2
#> 217 217   2 14.0 09-02-1979 23-04-2024      1993-01-22 16.934789    1
#> 218 218   2 85.6 10-10-1921 21-11-2022      2007-05-14  7.425056    1
#> 219 219   2 51.3 05-02-1958 16-05-2024      2009-06-05  4.688144    1
#> 220 220   1 62.0 27-03-1943 04-06-2023      2005-04-07  4.974151    1
#> 221 221   2 68.1 10-05-1940 04-09-2021      2008-06-01 46.761291    0
#> 222 222   2 82.9 27-06-1920 28-02-2025      2003-06-04 13.558375    1
#> 223 223   2 29.6 12-08-1972 03-04-2021      2002-03-15 30.416190    2
#> 224 224   2 27.6 04-08-1973 25-10-2025      2001-03-05 13.108065    1
#> 225 225   2 38.7 18-01-1957 22-01-2025      1995-09-21 40.134223    1
#> 226 226   2 64.9 27-09-1935 30-04-2021      2000-09-01  8.222808    0
#> 227 227   1 54.8 04-04-1944 16-04-2023      1999-01-06 46.080564    2
#> 228 228   2 71.4 01-05-1936 01-11-2024      2007-09-28 15.454254    1
#> 229 229   1 27.5 21-10-1978 21-03-2023      2006-04-11 20.015981    0
#> 230 230   1 31.3 05-04-1961 04-05-2021      1992-08-01 49.440135    1
#> 231 231   2 47.7 15-08-1946 30-01-2022      1994-04-24 35.423418    1
#> 232 232   1 38.8 18-01-1964 03-02-2024      2002-11-14 19.131462    1
#> 233 233   1 50.6 13-12-1955 21-10-2021      2006-07-30 27.928246    1
#> 234 234   2 27.1 13-06-1980 22-05-2024      2007-08-06 47.610987    1
#> 235 235   2 74.4 27-03-1924 26-12-2023      1998-08-24 28.112866    1
#> 236 236   2 46.3 09-07-1957 16-10-2024      2003-11-08 18.620882    1
#> 237 237   2 59.0 06-07-1937 14-10-2022      1996-06-28 25.378373    1
#> 238 238   2 30.7 08-10-1977 17-08-2025      2008-06-10 24.232235    2
#> 239 239   1 84.4 16-03-1923 14-11-2024      2007-07-25 16.418619    1
#> 240 240   2 31.1 17-12-1969 26-08-2021      2001-01-06 34.718146    1
#> 241 241   1 64.6 23-08-1926 18-02-2025      1991-04-06  2.572398    0
#> 242 242   2 47.3 05-01-1944 03-07-2022      1991-04-15 33.299757    2
#> 243 243   1 64.7 04-02-1931 21-11-2024      1995-10-07 32.477112    1
#> 244 244   2 71.6 31-12-1924 11-09-2025      1996-07-29 22.034645    1
#> 245 245   1 56.6 02-10-1940 20-04-2024      1997-05-03 12.257647    0
#> 246 246   2 68.3 18-02-1936 27-04-2025      2004-06-10 38.815631    1
#> 247 247   2 47.7 12-03-1949 28-11-2022      1996-11-18 46.674899    1
#> 248 248   2 20.5 09-10-1975 15-12-2022      1996-04-14  3.305915    0
#> 249 249   2 39.1 06-08-1968 18-01-2024      2007-09-17 14.181828    1
#> 250 250   2 73.1 07-12-1933 10-02-2025      2007-01-03 32.404326    1
#> 251 251   1 59.2 26-11-1947 01-09-2023      2007-02-25 32.090142    1
#> 252 252   1 16.3 05-10-1973 09-09-2025      1990-01-30 39.497356    1
#> 253 253   2 35.2 02-06-1967 18-03-2022      2002-08-01 38.097392    1
#> 254 254   1 44.2 26-04-1952 29-12-2021      1996-07-15 23.233426    1
#> 255 255   1 41.4 05-01-1964 15-09-2023      2005-05-24 11.370050    1
#> 256 256   1 77.3 10-02-1925 08-04-2021      2002-05-24 20.748838    1
#> 257 257   1 29.0 05-08-1979 24-07-2025      2008-08-08 35.822878    1
#> 258 258   2 59.3 12-03-1933 20-10-2023      1992-06-25 24.495849    1
#> 259 259   2 87.5 20-05-1921 01-05-2024      2008-11-10 42.190460    1
#> 260 260   2 86.6 05-01-1922 21-12-2021      2008-08-18  8.815360    1
#> 261 261   1 49.7 01-09-1956 11-07-2023      2006-05-16 24.196169    0
#> 262 262   2 65.4 20-04-1932 31-10-2021      1997-09-29 18.563115    2
#> 263 263   1 63.1 10-11-1941 28-10-2022      2004-12-30 13.334933    0
#> 264 264   1 49.9 06-06-1946 10-06-2025      1996-04-24 44.229678    1
#> 265 265   1 69.0 29-01-1932 19-01-2025      2001-02-12 17.275415    1
#> 266 266   1 58.7 11-01-1950 26-10-2021      2008-09-21 12.402864    1
#> 267 267   1 69.1 07-07-1930 27-11-2022      1999-08-17 42.863883    1
#> 268 268   1 63.2 07-06-1943 18-04-2021      2006-08-16 43.404145    2
#> 269 269   1 79.1 23-10-1921 21-12-2025      2000-11-20 24.754316    0
#> 270 270   2 58.0 11-11-1944 16-06-2021      2002-11-26 22.901190    2
#> 271 271   1 70.0 18-08-1937 05-11-2024      2007-08-12  3.702323    0
#> 272 272   2 26.1 12-11-1977 17-05-2024      2004-01-01 29.075060    0
#> 273 273   1 39.8 14-05-1955 15-05-2022      1995-03-15 12.703099    1
#> 274 274   1 41.1 17-06-1958 12-06-2021      1999-07-14 14.269082    1
#> 275 275   2 27.0 17-11-1971 28-04-2022      1998-11-16  7.712652    2
#> 276 276   2 27.1 12-11-1979 30-03-2022      2006-12-17 12.396125    2
#> 277 277   1 65.9 29-07-1943 26-02-2024      2009-06-28 40.228792    0
#> 278 278   1 77.6 27-03-1931 02-12-2024      2008-10-19 44.612082    1
#> 279 279   1 64.6 14-05-1929 03-08-2024      1993-12-01 13.432244    0
#> 280 280   2  9.6 12-06-1980 14-05-2023      1990-01-24 21.491707    0
#> 281 281   2 16.0 23-01-1977 28-05-2025      1993-01-15  4.268479    1
#> 282 282   2 50.4 25-12-1947 04-09-2023      1998-05-28 20.178944    2
#> 283 283   2 83.1 13-09-1926 08-05-2023      2009-10-07  9.445786    1
#> 284 284   1 66.2 28-01-1941 05-11-2022      2007-04-26 37.184825    2
#> 285 285   2 53.9 14-04-1950 14-12-2022      2004-03-25 15.399628    2
#> 286 286   2 39.1 28-06-1958 24-03-2025      1997-07-29 39.345413    0
#> 287 287   1 47.1 02-05-1944 26-03-2025      1991-05-22 39.609360    1
#> 288 288   2 62.5 03-08-1943 25-11-2023      2006-01-18 39.343185    0
#> 289 289   1 59.9 27-12-1940 15-09-2025      2000-11-24 36.551364    2
#> 290 290   2 69.2 15-02-1938 23-05-2025      2007-05-04 40.237984    0
#> 291 291   1 67.6 29-05-1942 04-07-2023      2009-12-25 14.108029    0
#> 292 292   2 32.4 21-03-1965 10-03-2023      1997-07-30 22.346934    0
#> 293 293   2 39.4 18-07-1954 14-09-2022      1993-12-22  5.529507    0
#> 294 294   1 25.6 28-01-1971 07-02-2024      1996-09-03 15.824783    1
#> 295 295   1 54.4 20-04-1938 16-07-2022      1992-09-18  6.331057    2
#> 296 296   2 72.6 02-02-1922 23-09-2023      1994-09-19  8.373282    1
#> 297 297   1 74.7 26-01-1935 03-04-2021      2009-10-21 40.825459    2
#> 298 298   2 51.3 01-01-1945 22-09-2021      1996-05-08 37.634904    1
#> 299 299   1 34.5 10-11-1962 22-11-2025      1997-04-25 22.895795    1
#> 300 300   1 60.7 31-10-1930 18-08-2022      1991-06-26 35.283275    1
#> 301 301   1 76.8 06-09-1924 04-07-2024      2001-06-15 38.386456    1
#> 302 302   2 40.1 13-05-1958 18-03-2021      1998-06-27 44.201934    2
#> 303 303   2 69.8 03-07-1928 25-03-2025      1998-04-14 47.650853    2
#> 304 304   1 34.5 01-12-1961 24-01-2022      1996-05-23 29.504219    1
#> 305 305   1 73.7 03-01-1936 14-06-2024      2009-10-01 23.028212    1
#> 306 306   2 28.2 22-07-1973 21-10-2021      2001-10-08 10.009555    2
#> 307 307   1 66.3 03-12-1935 19-05-2025      2002-03-13 21.320976    1
#> 308 308   2 33.3 18-01-1958 08-11-2025      1991-05-20 20.177488    1
#> 309 309   1 55.0 19-03-1953 03-04-2023      2008-03-23 28.535777    2
#> 310 310   2 39.4 07-01-1951 12-09-2023      1990-05-25  6.708386    0
#> 311 311   1 34.8 07-07-1958 13-06-2021      1993-05-07 14.080118    1
#> 312 312   2 74.1 16-12-1925 05-04-2023      2000-02-06 33.934595    1
#> 313 313   1 44.1 24-08-1956 07-07-2024      2000-09-12 26.935043    1
#> 314 314   2 36.4 19-03-1967 21-05-2024      2003-08-16  6.826432    2
#> 315 315   2 70.1 17-03-1937 27-08-2025      2007-04-14 27.016561    1
#> 316 316   1 70.9 15-07-1929 01-05-2024      2000-06-18 24.191551    1
#> 317 317   1 75.2 21-10-1933 13-09-2023      2008-12-16 44.820611    0
#> 318 318   2 47.7 29-10-1944 12-05-2022      1992-07-01 43.916547    0
#> 319 319   2 41.2 10-11-1954 01-03-2025      1996-01-15 21.888631    0
#> 320 320   2 30.3 10-04-1960 25-05-2025      1990-07-15 32.090954    1
#> 321 321   2 18.3 21-03-1977 12-08-2021      1995-07-09 17.721596    0
#> 322 322   2 36.1 19-05-1967 27-01-2022      2003-06-08 21.650189    2
#> 323 323   2 62.8 11-09-1929 14-09-2025      1992-06-27 15.517145    1
#> 324 324   2 72.1 27-06-1924 22-11-2024      1996-07-30 41.757409    1
#> 325 325   1 39.9 27-06-1961 03-05-2021      2001-06-04 34.324197    2
#> 326 326   2 35.6 16-04-1969 07-09-2022      2004-11-17 46.466182    1
#> 327 327   2 38.3 27-04-1963 14-10-2021      2001-08-08 14.620653    0
#> 328 328   2 69.9 03-09-1934 20-01-2022      2004-08-07 33.997850    1
#> 329 329   1 54.9 25-06-1942 07-08-2023      1997-05-23 44.648952    1
#> 330 330   1 52.1 02-01-1950 24-06-2024      2002-01-25 35.757442    1
#> 331 331   1 29.2 27-12-1972 26-08-2024      2002-03-01 15.120332    2
#> 332 332   2 63.4 14-11-1926 12-05-2021      1990-03-22 22.479712    0
#> 333 333   1 32.7 12-12-1975 19-05-2021      2008-09-04 28.051214    0
#> 334 334   1 65.5 16-03-1941 08-08-2022      2006-09-26  2.582333    1
#> 335 335   2 48.2 12-04-1944 14-06-2025      1992-06-29  2.910163    0
#> 336 336   2 77.4 06-01-1929 13-04-2022      2006-06-12 28.948585    0
#> 337 337   2 46.0 04-08-1957 28-11-2024      2003-07-19 11.320297    0
#> 338 338   2 37.3 14-07-1956 09-05-2023      1993-10-31 28.817539    2
#> 339 339   2 81.7 17-12-1922 19-06-2025      2004-08-23  9.163100    1
#> 340 340   2 36.7 11-01-1970 23-07-2025      2006-09-12 14.887834    0
#> 341 341   1 70.5 09-04-1928 08-08-2023      1998-09-22 37.037026    1
#> 342 342   2 36.1 06-10-1966 29-03-2022      2002-11-27 37.524841    1
#> 343 343   1 27.0 11-03-1972 09-01-2024      1999-03-01 36.468215    1
#> 344 344   1 64.2 07-05-1934 14-10-2022      1998-07-13 19.308197    0
#> 345 345   2 40.9 18-01-1954 31-12-2021      1994-12-21 16.694447    1
#> 346 346   1 65.5 05-08-1940 28-10-2024      2006-02-06 16.737381    0
#> 347 347   2 68.7 08-12-1923 13-09-2025      1992-09-05 35.619451    1
#> 348 348   1 59.9 14-03-1939 27-10-2025      1999-02-13 15.496035    1
#> 349 349   2 64.3 13-05-1944 17-04-2024      2008-09-01 42.301276    2
#> 350 350   2 22.5 25-11-1971 14-07-2021      1994-06-03  2.167653    1
#> 351 351   1 52.0 21-11-1955 24-11-2024      2007-11-09 43.247124    1
#> 352 352   2 18.7 10-08-1980 11-01-2023      1999-04-16 47.892419    1
#> 353 353   1 22.2 23-10-1968 25-12-2023      1991-01-13 30.941820    1
#> 354 354   2 72.9 22-06-1928 11-11-2022      2001-05-03 25.566152    0
#> 355 355   1 51.0 31-12-1946 02-01-2025      1997-12-20  6.027933    1
#> 356 356   2 72.6 07-04-1921 18-02-2024      1993-11-10 46.658650    1
#> 357 357   2 66.8 16-06-1935 28-01-2022      2002-04-05 35.484253    1
#> 358 358   2 39.6 28-04-1962 27-02-2023      2001-11-30 22.884500    2
#> 359 359   1 63.9 20-10-1927 17-12-2022      1991-09-08 14.085293    2
#> 360 360   2 59.6 08-05-1940 02-10-2024      1999-12-11 43.012223    2
#> 361 361   1 66.9 02-07-1933 26-08-2021      2000-06-10  3.338920    2
#> 362 362   2 60.3 25-04-1936 22-10-2023      1996-07-31 39.569425    2
#> 363 363   2 66.9 13-02-1939 19-01-2022      2006-01-05 34.461863    1
#> 364 364   1 58.8 24-05-1940 06-12-2021      1999-02-23 20.431819    1
#> 365 365   2 46.7 30-04-1957 23-07-2024      2004-01-03  9.778013    0
#> 366 366   2 19.2 10-05-1979 09-12-2022      1998-07-16 49.438063    1
#> 367 367   2 61.5 14-03-1943 26-03-2024      2004-09-10 34.554454    0
#> 368 368   1 23.5 26-07-1969 29-12-2025      1993-02-11 20.223153    1
#> 369 369   1 46.7 26-01-1953 05-03-2024      1999-10-09  3.112373    1
#> 370 370   1 27.9 01-04-1977 01-04-2023      2005-02-28 13.279068    2
#> 371 371   2 44.2 20-06-1962 11-07-2022      2006-09-06 28.125154    2
#> 372 372   2 65.3 14-11-1937 26-04-2024      2003-03-16 28.519341    0
#> 373 373   1 48.5 06-10-1943 13-09-2023      1992-03-26 29.015347    1
#> 374 374   2 49.7 27-04-1951 15-01-2024      2001-01-06 30.109183    0
#> 375 375   1 77.0 14-10-1931 05-11-2022      2008-10-29 37.312252    2
#> 376 376   2 40.7 19-09-1966 01-09-2023      2007-06-17 42.045809    1
#> 377 377   2 71.0 10-12-1932 21-09-2021      2003-12-14  3.245529    1
#> 378 378   1 36.8 25-02-1969 28-03-2025      2005-12-06 42.388344    0
#> 379 379   1 70.0 01-01-1934 09-02-2024      2004-01-13 46.940151    1
#> 380 380   2 51.3 16-06-1941 09-08-2025      1992-10-21 22.930561    2
#> 381 381   2 37.5 05-10-1955 09-02-2021      1993-03-28 31.865143    1
#> 382 382   1 60.6 06-04-1935 25-04-2022      1995-11-01 33.496668    1
#> 383 383   1 38.4 07-01-1956 08-01-2025      1994-06-03 20.691221    1
#> 384 384   2 41.6 28-03-1953 08-04-2024      1994-10-23 28.014357    2
#> 385 385   1 77.5 12-08-1927 04-04-2022      2005-02-28 23.472844    1
#> 386 386   1 74.7 29-03-1920 01-10-2023      1994-12-22 12.721391    0
#> 387 387   2 81.0 08-02-1926 19-04-2023      2007-02-15 33.839725    1
#> 388 388   2 81.9 12-04-1922 07-02-2021      2004-03-19 36.824085    2
#> 389 389   2 25.7 08-11-1970 14-02-2022      1996-08-08 23.511035    2
#> 390 390   2 33.1 11-02-1961 12-03-2022      1994-03-12 24.895779    1
#> 391 391   2 42.2 15-04-1958 25-01-2024      2000-06-26 48.821707    1
#> 392 392   1 63.5 22-01-1930 17-05-2024      1993-07-27  6.092811    1
#> 393 393   1 53.8 22-01-1940 26-05-2022      1993-10-26 14.638935    0
#> 394 394   1 29.7 01-03-1980 19-07-2022      2009-11-18 12.199885    0
#> 395 395   1 31.3 03-12-1970 14-10-2023      2002-04-04 27.519609    2
#> 396 396   1 58.9 06-01-1949 21-04-2025      2007-11-13 21.114700    0
#> 397 397   1 48.6 02-09-1949 16-01-2022      1998-04-19 38.743955    0
#> 398 398   1 39.3 16-12-1960 09-09-2024      2000-03-24 14.211998    2
#> 399 399   2 56.4 25-10-1944 05-05-2023      2001-03-28 29.171403    1
#> 400 400   2 55.1 23-07-1938 26-01-2022      1993-08-16 13.212920    1
#> 401 401   1 50.9 01-02-1945 05-08-2025      1995-12-12  2.747222    2
#> 402 402   2 37.0 09-01-1972 19-05-2021      2008-12-22  2.735424    2
#> 403 403   2 39.8 21-07-1959 16-07-2024      1999-05-24 44.329607    1
#> 404 404   2 38.3 23-05-1960 10-11-2023      1998-09-22 27.489250    0
#> 405 405   2 28.2 02-05-1965 28-04-2025      1993-07-29 26.583559    0
#> 406 406   1 33.0 11-10-1975 23-07-2021      2008-10-20  6.118043    1
#> 407 407   1 47.6 28-09-1951 30-10-2023      1999-04-23  4.571786    1
#> 408 408   2 29.1 01-04-1962 29-08-2021      1991-05-21  5.811235    1
#> 409 409   1 64.8 24-07-1941 14-10-2024      2006-05-05 13.259926    1
#> 410 410   1 54.7 10-02-1943 27-01-2023      1997-10-29  5.364529    1
#> 411 411   1 69.8 28-09-1932 22-03-2021      2002-07-15 11.298386    2
#> 412 412   1 71.1 09-04-1934 24-11-2023      2005-05-06 12.383440    0
#> 413 413   2 55.4 06-08-1950 20-07-2021      2005-12-28 39.923877    2
#> 414 414   2 63.2 26-10-1945 27-04-2025      2009-01-16  5.822879    1
#> 415 415   2 62.2 25-12-1939 05-11-2024      2002-02-26  4.897031    0
#> 416 416   2 51.5 21-05-1956 22-09-2021      2007-11-10 20.521254    2
#> 417 417   2 40.5 10-02-1960 10-06-2021      2000-08-12 31.198184    2
#> 418 418   1 61.4 22-04-1936 24-01-2023      1997-09-17 39.356157    1
#> 419 419   1 59.8 05-05-1931 17-05-2025      1991-02-10 18.038128    2
#> 420 420   1 26.9 08-08-1972 04-06-2025      1999-06-21 47.529826    0
#> 421 421   2 46.9 03-06-1954 30-05-2022      2001-04-25 41.305629    1
#> 422 422   2 58.6 12-03-1944 19-05-2022      2002-10-06  5.449983    1
#> 423 423   1 46.4 05-03-1960 01-11-2022      2006-07-23  9.863469    1
#> 424 424   2 71.1 09-11-1920 06-01-2021      1991-12-27 16.986610    1
#> 425 425   1 29.9 14-05-1961 24-09-2022      1991-04-02 28.939927    1
#> 426 426   1 64.2 08-08-1932 20-09-2021      1996-10-16 47.043829    1
#> 427 427   1 74.0 14-10-1920 06-08-2022      1994-10-31 13.252483    2
#> 428 428   2 81.5 17-05-1925 29-11-2022      2006-11-22 22.777111    2
#> 429 429   2 11.0 15-05-1979 03-11-2021      1990-05-11 15.211000    2
#> 430 430   2 33.2 07-02-1976 03-06-2021      2009-05-05 38.343348    1
#> 431 431   2 42.2 22-07-1953 29-04-2021      1995-10-09 33.025386    0
#> 432 432   1 88.0 06-09-1920 30-10-2025      2008-09-21 31.698237    1
#> 433 433   2 57.5 01-02-1944 30-11-2023      2001-07-23 33.663262    1
#> 434 434   1 52.0 12-06-1953 12-07-2025      2005-06-24 35.678343    1
#> 435 435   1 22.2 29-01-1970 06-08-2023      1992-03-31 10.907665    1
#> 436 436   2 44.8 18-01-1951 24-01-2021      1995-10-21 35.058535    1
#> 437 437   2 31.7 26-09-1970 30-05-2022      2002-05-27 33.702846    1
#> 438 438   2 48.9 12-05-1947 07-03-2021      1996-03-26  7.118338    0
#> 439 439   2 80.3 19-05-1923 29-08-2025      2003-09-05 19.611372    1
#> 440 440   1 22.7 09-05-1968 04-10-2021      1991-02-05 29.923951    1
#> 441 441   2 42.1 31-05-1967 05-10-2022      2009-06-26 49.518366    0
#> 442 442   2 47.4 20-06-1955 13-02-2024      2002-11-07 15.221481    0
#> 443 443   2 71.2 06-08-1931 21-06-2022      2002-10-31 22.956154    1
#> 444 444   1 69.8 15-02-1921 01-05-2022      1990-12-05  6.561065    2
#> 445 445   2 23.0 13-11-1980 26-11-2023      2003-11-27 22.502690    0
#> 446 446   1 31.2 02-04-1962 07-01-2024      1993-07-02  8.519479    1
#> 447 447   2 12.3 27-09-1977 24-08-2022      1990-01-23 22.656583    1
#> 448 448   2 64.8 24-01-1933 11-06-2025      1997-11-25 37.903845    1
#> 449 449   1 54.1 21-04-1946 07-05-2023      2000-05-09  9.354301    0
#> 450 450   2 23.9 14-11-1969 21-01-2021      1993-10-04 41.890955    1
#> 451 451   1 34.6 25-06-1972 21-03-2023      2007-01-30 39.608133    0
#> 452 452   1 41.4 22-01-1962 03-07-2022      2003-06-11 10.503435    2
#> 453 453   2 52.1 01-06-1956 22-04-2024      2008-06-22 29.129614    2
#> 454 454   2 38.8 10-05-1953 30-05-2022      1992-02-23  5.378042    1
#> 455 455   2 58.3 03-10-1946 09-03-2025      2005-01-09 41.401533    2
#> 456 456   2 77.0 03-01-1924 22-10-2021      2001-01-01 26.926516    2
#> 457 457   1 50.9 09-04-1950 25-11-2023      2001-03-06 31.443950    1
#> 458 458   1 44.6 31-07-1955 27-02-2022      2000-03-18 21.154268    1
#> 459 459   1 63.5 19-08-1942 23-03-2022      2006-02-13 17.972653    1
#> 460 460   2 32.2 18-06-1974 07-03-2024      2006-08-27 19.760554    2
#> 461 461   2 53.9 05-04-1951 23-05-2024      2005-03-12 35.416689    1
#> 462 462   2 37.8 30-07-1965 17-12-2023      2003-05-03 42.890156    1
#> 463 463   2 37.9 14-10-1965 08-11-2025      2003-09-17 24.432165    1
#> 464 464   1 67.5 12-04-1924 16-02-2024      1991-10-14 32.827315    0
#> 465 465   1 31.7 12-11-1960 03-06-2022      1992-07-30 35.277299    1
#> 466 466   2 29.2 03-09-1966 11-02-2022      1995-12-03 37.656455    2
#> 467 467   1 35.1 20-11-1955 08-12-2025      1990-12-24 34.533252    0
#> 468 468   1 49.2 01-11-1954 18-08-2022      2004-01-25 16.696638    1
#> 469 469   2 73.3 18-07-1936 07-07-2025      2009-11-16 40.441499    1
#> 470 470   2 65.5 14-04-1937 07-06-2022      2002-10-30 33.843292    1
#> 471 471   1 64.5 01-12-1931 16-12-2022      1996-05-29 47.800268    1
#> 472 472   2 57.5 21-08-1951 03-01-2024      2009-02-26 28.271668    0
#> 473 473   2 26.1 29-12-1966 03-08-2024      1993-01-31 32.724777    0
#> 474 474   1 44.4 06-09-1961 29-06-2021      2006-01-29 16.839593    0
#> 475 475   2 64.6 08-09-1936 19-05-2022      2001-04-11 29.423216    0
#> 476 476   2 45.1 02-03-1953 29-04-2025      1998-03-21  7.052066    2
#> 477 477   1 17.0 04-09-1973 01-01-2022      1990-09-20 25.006255    1
#> 478 478   2 45.8 19-03-1945 01-06-2024      1991-01-19 41.341311    1
#> 479 479   1 50.6 02-03-1946 20-08-2022      1996-09-30 14.201723    1
#> 480 480   2 25.1 15-03-1978 02-03-2023      2003-05-05 37.354688    2
#> 481 481   2 37.1 25-09-1957 23-01-2024      1994-11-03 32.839677    1
#> 482 482   1 60.5 16-04-1948 29-09-2022      2008-10-19 34.284582    0
#> 483 483   2 52.3 30-06-1956 30-11-2021      2008-11-04 44.061527    2
#> 484 484   2 48.0 04-09-1949 06-07-2024      1997-08-21 34.953729    0
#> 485 485   1 47.3 24-05-1950 27-01-2022      1997-09-16  8.386425    1
#> 486 486   1 56.2 04-04-1943 25-10-2022      1999-06-24 14.487544    2
#> 487 487   1 30.7 20-08-1966 03-09-2022      1997-05-15 26.478549    1
#> 488 488   1 23.8 10-03-1974 23-06-2021      1998-01-06 28.878952    0
#> 489 489   1 29.7 19-11-1973 25-07-2023      2003-07-31 47.837002    0
#> 490 490   1 28.1 16-05-1972 26-01-2025      2000-06-16 47.751265    0
#> 491 491   2 73.8 14-03-1929 01-07-2023      2002-12-21 20.759672    2
#> 492 492   2 31.8 05-05-1963 30-04-2024      1995-02-21 38.181773    2
#> 493 493   1 22.6 07-02-1978 28-06-2023      2000-08-31 43.328960    0
#> 494 494   2 72.4 20-05-1928 05-08-2025      2000-10-15 49.282483    1
#> 495 495   1 65.4 31-03-1936 15-10-2023      2001-08-12 29.067054    1
#> 496 496   1 47.7 29-04-1955 07-11-2022      2002-12-30 44.123152    2
#> 497 497   1 42.9 17-03-1956 16-06-2025      1999-02-18 41.460467    2
#> 498 498   1 29.2 29-07-1961 25-03-2024      1990-10-28 24.048037    0
#> 499 499   2 25.0 13-09-1980 25-09-2025      2005-08-26 49.166849    1
#> 500 500   2 51.4 29-05-1947 24-04-2022      1998-10-24 33.284086    1
#>     localisation necrosis cd10 sox10 ck death_date recurrence_date
#> 1              2        0   NA     1  0       <NA>            <NA>
#> 2              1        1    0     0  0       <NA>      2014-12-09
#> 3              4        0    0     1 NA       <NA>            <NA>
#> 4              1        1    1    NA  0 2019-07-27            <NA>
#> 5              3        0    0     0  0 2018-04-19            <NA>
#> 6              0       NA   NA    NA  0       <NA>            <NA>
#> 7              3        1    1     0 NA       <NA>      2011-10-10
#> 8              5       NA    1     0  0       <NA>            <NA>
#> 9              4        0    1     0  0       <NA>            <NA>
#> 10             1        0   NA     1  1       <NA>      2014-12-15
#> 11             2       NA   NA     1  0 2019-03-12            <NA>
#> 12             3        1   NA     1 NA 2019-02-16            <NA>
#> 13             2       NA    0    NA  0       <NA>      2011-09-02
#> 14             3        1    0     0  0 2017-07-18            <NA>
#> 15             2       NA    1     0 NA       <NA>            <NA>
#> 16             4        1    1    NA NA       <NA>      2014-05-27
#> 17             2        1    0     0  0 2017-09-28            <NA>
#> 18             2        1    0     1  0       <NA>            <NA>
#> 19             3       NA    0     0 NA       <NA>      2010-06-11
#> 20             4        0   NA     1  0 2020-02-17            <NA>
#> 21             2        0    1    NA  0 2019-04-27            <NA>
#> 22             3        1    1     0 NA       <NA>            <NA>
#> 23             5        1   NA     1  0 2018-03-01      2011-01-03
#> 24             3        0    1     1  0       <NA>            <NA>
#> 25             2       NA    1     0  1 2016-02-24            <NA>
#> 26             1        0   NA     0  0 2017-09-30            <NA>
#> 27             2        0    1     0 NA       <NA>            <NA>
#> 28             2        0   NA     1  1       <NA>      2012-01-18
#> 29             1        0   NA    NA  0 2017-05-28            <NA>
#> 30             1       NA    1     1  0       <NA>            <NA>
#> 31             1        0    0     0  1       <NA>      2012-07-13
#> 32             3        1   NA     1 NA       <NA>            <NA>
#> 33             1        1    0     1 NA 2018-02-01      2014-09-28
#> 34             1       NA    0     1  0 2019-04-04            <NA>
#> 35             3       NA   NA     0  1 2016-09-20            <NA>
#> 36             2       NA   NA     1 NA 2016-05-02            <NA>
#> 37             3       NA    1     1  1       <NA>            <NA>
#> 38             1        0    1    NA  0 2020-05-20      2013-10-26
#> 39             3       NA    0    NA  1       <NA>      2012-07-20
#> 40             3        1   NA    NA  0       <NA>            <NA>
#> 41             3        0    0     1 NA       <NA>      2012-06-29
#> 42             1       NA    0     0  1       <NA>      2013-06-04
#> 43             5       NA    1    NA  0       <NA>            <NA>
#> 44             4        0   NA     0  1 2019-06-30            <NA>
#> 45             2       NA    0     1  1       <NA>      2014-10-06
#> 46             3        1    1     1  1       <NA>            <NA>
#> 47             3       NA    1     1 NA       <NA>      2010-12-25
#> 48             0        1    0     1  0 2016-11-21      2012-03-18
#> 49             2       NA    0     0  0       <NA>            <NA>
#> 50             2        1    0     1  1       <NA>            <NA>
#> 51             4        1    1     0 NA 2018-08-01            <NA>
#> 52             1       NA    1    NA NA       <NA>      2012-09-10
#> 53             1        0    0     0  0       <NA>      2013-04-22
#> 54             2       NA    1     1  0       <NA>      2010-08-27
#> 55             3        1   NA     0  1 2020-08-31      2010-05-27
#> 56             2        0    1     1  0       <NA>            <NA>
#> 57             2        1    1     1  0 2016-02-10            <NA>
#> 58             3        0    1    NA NA 2020-07-02      2014-03-31
#> 59             2        1   NA     1  0       <NA>      2013-09-25
#> 60             4       NA    1     1 NA       <NA>      2014-05-26
#> 61             4        0    0     0  0       <NA>      2012-09-17
#> 62             3       NA   NA    NA NA 2016-10-03      2013-11-13
#> 63             1        0    1     0  1       <NA>            <NA>
#> 64             1        1    1     1  0 2016-07-28      2011-12-02
#> 65             2       NA    0     1  1       <NA>            <NA>
#> 66             3        1   NA     1  0       <NA>      2014-12-07
#> 67             2        0    0    NA  1       <NA>      2011-06-25
#> 68             2        0    0     0  1 2017-09-05      2015-03-26
#> 69             1        0    0     0  1       <NA>            <NA>
#> 70             2        1    1     0  0 2019-10-04            <NA>
#> 71             2       NA    0     0  1 2018-05-10      2012-09-03
#> 72             1       NA   NA    NA  0       <NA>      2015-03-10
#> 73             3        0    1     1  0       <NA>      2010-07-13
#> 74             2        1    0    NA  1       <NA>      2015-10-23
#> 75             3        0    0    NA NA       <NA>            <NA>
#> 76             0       NA    0     0  1       <NA>      2014-05-18
#> 77             2        0    0    NA  0       <NA>            <NA>
#> 78             3        0    1     1  0       <NA>            <NA>
#> 79             2        0   NA     0  1       <NA>            <NA>
#> 80             3        0    1    NA  1 2016-10-24            <NA>
#> 81             2        0    0    NA NA       <NA>            <NA>
#> 82             3       NA   NA     1  0       <NA>            <NA>
#> 83             2        0    1     0 NA 2020-08-20      2013-02-26
#> 84             2        0   NA    NA NA       <NA>      2011-04-29
#> 85             2        0   NA     0  0       <NA>            <NA>
#> 86             3        0    1     0 NA       <NA>            <NA>
#> 87             4        1    1    NA  1       <NA>      2015-10-29
#> 88             2       NA   NA    NA  0 2020-05-04            <NA>
#> 89             5        1    1     0  1       <NA>            <NA>
#> 90             2        1    0     1  0       <NA>            <NA>
#> 91             1       NA   NA     0  0 2019-04-07      2013-01-02
#> 92             4       NA    1     1  0       <NA>      2014-03-05
#> 93             1       NA    1     0  0       <NA>            <NA>
#> 94             3        1    0     1  1       <NA>      2012-01-28
#> 95             2       NA   NA     1 NA       <NA>      2012-08-15
#> 96             4       NA    1     1 NA       <NA>            <NA>
#> 97             1        1   NA    NA  0       <NA>      2010-12-02
#> 98             2       NA    0     0 NA 2017-07-14      2011-03-31
#> 99             2       NA    0    NA NA 2020-08-28      2012-04-01
#> 100            3       NA   NA    NA  1 2018-04-27            <NA>
#> 101            2        1   NA     0  1 2020-04-01      2013-03-09
#> 102            3        1   NA     0  1       <NA>      2011-05-13
#> 103            2        0   NA     0  1       <NA>            <NA>
#> 104            3        0    0    NA  1       <NA>            <NA>
#> 105            4        0   NA     1  1       <NA>      2013-04-05
#> 106            3       NA   NA    NA  1       <NA>      2014-03-22
#> 107            2       NA   NA     0  1       <NA>            <NA>
#> 108            2        1    0    NA  1       <NA>      2015-09-02
#> 109            2        1    0     1 NA       <NA>      2010-09-05
#> 110            4       NA   NA     1  1       <NA>            <NA>
#> 111            3        0   NA    NA NA       <NA>      2010-01-30
#> 112            0        0    1     1 NA       <NA>      2012-11-29
#> 113            2        1   NA     0 NA       <NA>      2010-05-29
#> 114            3       NA    1    NA NA 2019-04-03            <NA>
#> 115            0        0    0    NA NA       <NA>            <NA>
#> 116            1        0    1     1 NA 2017-06-16      2011-11-25
#> 117            2        1    0     0  0 2017-01-06            <NA>
#> 118            3        0    0     0  1       <NA>            <NA>
#> 119            3       NA    0     0 NA       <NA>      2010-05-15
#> 120            2        1    0     1 NA 2016-01-25      2015-04-03
#> 121            2        0    0    NA NA 2016-01-15      2015-10-11
#> 122            3        0    1     0  1       <NA>            <NA>
#> 123            3        0    0     1  1       <NA>            <NA>
#> 124            2       NA    0    NA NA       <NA>            <NA>
#> 125            2       NA    0     1  0       <NA>            <NA>
#> 126            3        0    0    NA  0       <NA>            <NA>
#> 127            3        1    0     1  0       <NA>            <NA>
#> 128            3        1   NA    NA  0       <NA>            <NA>
#> 129            3        1    0    NA  1       <NA>            <NA>
#> 130            1        1   NA    NA NA       <NA>      2010-09-08
#> 131            3        0    1    NA  1       <NA>      2015-02-16
#> 132            3        0   NA     0 NA 2016-03-24            <NA>
#> 133            3       NA    0     1  1       <NA>            <NA>
#> 134            3        1   NA     1  1       <NA>            <NA>
#> 135            3        1    1     1  1 2019-01-04            <NA>
#> 136            2        1    0    NA  0 2019-10-18            <NA>
#> 137            1        1    0     0 NA       <NA>            <NA>
#> 138            1       NA    0     1  0       <NA>      2010-01-01
#> 139            1       NA    0     1  1       <NA>      2015-02-14
#> 140            3        1   NA     0 NA       <NA>            <NA>
#> 141            2       NA   NA    NA  1       <NA>            <NA>
#> 142            1        0    1     0 NA       <NA>      2013-04-15
#> 143            4        1   NA     1  0       <NA>            <NA>
#> 144            3        1    1    NA  0 2017-06-23            <NA>
#> 145            5        1    0    NA  1       <NA>      2012-03-03
#> 146            2       NA   NA     0 NA       <NA>            <NA>
#> 147            3       NA   NA     0  0       <NA>      2012-12-01
#> 148            1        0    1     0 NA 2016-12-11      2010-12-19
#> 149            3        1    0    NA  0       <NA>            <NA>
#> 150            2        0   NA     1 NA 2018-01-01            <NA>
#> 151            2        0   NA     0  0       <NA>      2015-03-06
#> 152            2        1   NA    NA NA       <NA>            <NA>
#> 153            1       NA   NA    NA  0 2019-01-17      2014-08-03
#> 154            1        1    0    NA NA       <NA>            <NA>
#> 155            3       NA    1     1  0       <NA>      2013-06-01
#> 156            2       NA    0     0  1       <NA>            <NA>
#> 157            3        0    0    NA NA       <NA>            <NA>
#> 158            1        1    0    NA NA       <NA>      2010-07-07
#> 159            3        1    1     1  0       <NA>            <NA>
#> 160            2        0    0     0  1       <NA>      2012-03-13
#> 161            4       NA    1    NA NA       <NA>      2013-07-03
#> 162            2        1    0     0  0 2019-03-29            <NA>
#> 163            2        1    1     1  1       <NA>      2010-10-19
#> 164            3       NA    1    NA  1       <NA>            <NA>
#> 165            4       NA   NA     1  0 2018-02-13      2013-02-27
#> 166            0       NA    0     1 NA       <NA>            <NA>
#> 167            2       NA    1     1  0       <NA>      2012-11-18
#> 168            3       NA    1    NA  1       <NA>      2012-08-06
#> 169            4        1   NA    NA  0       <NA>            <NA>
#> 170            1        1    0     1 NA       <NA>      2010-01-17
#> 171            3        1    0     0 NA       <NA>      2012-01-15
#> 172            2        1    0     1  0       <NA>            <NA>
#> 173            3       NA    0     1  0 2019-02-05      2011-04-14
#> 174            5        1    0    NA NA       <NA>      2013-12-18
#> 175            3        0    1     1 NA       <NA>            <NA>
#> 176            3        0   NA     0  0 2020-08-12      2015-11-08
#> 177            4       NA    1     1  1       <NA>      2014-03-01
#> 178            3        0    0     1  1 2020-11-01      2013-03-15
#> 179            3        0    1     1  0 2020-03-07      2015-12-24
#> 180            2        0   NA     1  1 2019-12-27            <NA>
#> 181            3        0    1    NA  0       <NA>      2012-04-21
#> 182            2        0    0     0 NA       <NA>      2012-10-14
#> 183            2       NA    1     0  0       <NA>      2011-12-25
#> 184            5        1    0    NA NA       <NA>            <NA>
#> 185            5        1    1     1  0 2016-06-27            <NA>
#> 186            2        1   NA     1 NA 2020-06-14            <NA>
#> 187            3        0   NA    NA  0       <NA>            <NA>
#> 188            2       NA    1     1 NA       <NA>            <NA>
#> 189            2       NA    0     1  0 2017-09-21            <NA>
#> 190            5        0    1     1  0 2017-08-11      2011-11-18
#> 191            2        0   NA     1 NA       <NA>            <NA>
#> 192            4        0    1     1 NA 2018-05-26            <NA>
#> 193            3        0    0     1  1       <NA>            <NA>
#> 194            3       NA   NA     1  0       <NA>            <NA>
#> 195            1        0   NA     0  1       <NA>            <NA>
#> 196            2        1   NA    NA  0 2019-07-21      2010-05-30
#> 197            5        1   NA     1  1       <NA>      2011-01-12
#> 198            1       NA    0    NA NA 2016-04-26      2014-06-05
#> 199            4       NA    0     1 NA 2016-06-11      2010-10-21
#> 200            5       NA    0     1  0       <NA>      2014-02-23
#> 201            1       NA    0     1 NA 2017-07-16      2015-01-13
#> 202            3        0   NA    NA  0 2018-09-21            <NA>
#> 203            3        1    0     1 NA       <NA>      2010-09-09
#> 204            2       NA    1     1  1       <NA>            <NA>
#> 205            3        0   NA     0  0 2019-07-12      2013-02-10
#> 206            5        0   NA     1  1 2016-11-20      2015-05-13
#> 207            2        0    0     0 NA       <NA>      2010-11-28
#> 208            2       NA    1     1  1 2020-09-21            <NA>
#> 209            1        0    0     0  1       <NA>      2015-12-21
#> 210            3        1   NA     0  0       <NA>      2013-06-24
#> 211            2       NA    1    NA  0       <NA>            <NA>
#> 212            3        1   NA     0 NA       <NA>            <NA>
#> 213            2        1    1     1  0       <NA>            <NA>
#> 214            3        1   NA     1  1       <NA>      2015-11-13
#> 215            4       NA    0    NA NA       <NA>            <NA>
#> 216            3        0   NA    NA NA       <NA>            <NA>
#> 217            1        0    0     1  1 2018-08-07            <NA>
#> 218            1        0    1    NA NA       <NA>            <NA>
#> 219            2       NA   NA     0  0       <NA>      2013-01-05
#> 220            1        1    1     1 NA 2020-07-19      2012-05-13
#> 221            2       NA   NA    NA NA 2016-03-25      2013-04-11
#> 222            2        0    1     1  1       <NA>            <NA>
#> 223            1       NA   NA     1 NA       <NA>            <NA>
#> 224            4        0   NA     1  0       <NA>      2013-01-15
#> 225            4        0    0    NA  1 2018-09-30            <NA>
#> 226            4       NA   NA     1  0       <NA>      2011-08-03
#> 227            1        0   NA     1 NA 2020-11-23      2014-09-11
#> 228            3       NA    1    NA NA       <NA>            <NA>
#> 229            1       NA    1     1 NA       <NA>            <NA>
#> 230            3       NA    0     1 NA       <NA>      2011-01-25
#> 231            3       NA   NA     0 NA       <NA>            <NA>
#> 232            4       NA    1     1 NA 2016-11-14      2015-03-15
#> 233            3        0    0     0 NA 2018-10-24      2012-08-05
#> 234            4        0    1     0  0       <NA>            <NA>
#> 235            3        0   NA    NA  0       <NA>            <NA>
#> 236            2       NA   NA     1 NA       <NA>      2012-05-31
#> 237            2       NA    1     1 NA 2018-07-17      2011-06-30
#> 238            3       NA   NA     0 NA 2017-10-17            <NA>
#> 239            2        1    0    NA  0 2016-05-14      2013-01-25
#> 240            4        1    1     1 NA 2020-01-20      2012-09-04
#> 241            3       NA    0     1 NA       <NA>      2010-10-18
#> 242            3       NA    1     0  1       <NA>            <NA>
#> 243            2       NA   NA     1  1       <NA>            <NA>
#> 244            3        1    0     1  1 2018-10-09            <NA>
#> 245            2       NA   NA     0  1       <NA>      2010-09-18
#> 246            3        1    1     0  1       <NA>            <NA>
#> 247            3        1    1    NA  1 2016-04-23      2010-04-02
#> 248            3       NA    1     0  1       <NA>            <NA>
#> 249            2       NA    0     1  0 2016-07-05            <NA>
#> 250            2        1    1    NA NA       <NA>            <NA>
#> 251            3        1    0    NA  0       <NA>            <NA>
#> 252            1        1    0     1  0 2020-09-29      2013-12-22
#> 253            3       NA   NA     1  0       <NA>      2010-07-17
#> 254            1       NA    0    NA NA       <NA>            <NA>
#> 255            4        0    1     1  0 2017-10-26      2013-07-08
#> 256            3        0   NA    NA  1 2019-10-07            <NA>
#> 257            3        0    1     0  0       <NA>            <NA>
#> 258            2        0    1    NA  1       <NA>            <NA>
#> 259            1        1    0     0 NA       <NA>      2013-03-11
#> 260            3       NA   NA     1  0 2019-02-15            <NA>
#> 261            2       NA    1     1  0       <NA>      2012-12-12
#> 262            4       NA    1     0  1       <NA>            <NA>
#> 263            3        0    0     0  1       <NA>            <NA>
#> 264            1        0    0    NA  0 2019-08-08            <NA>
#> 265            2        0   NA    NA  0 2016-09-07            <NA>
#> 266            2       NA    0    NA NA       <NA>      2015-11-11
#> 267            3        1    1     1  1       <NA>            <NA>
#> 268            3        1    0     1  1       <NA>      2012-01-08
#> 269            3        1    1     1  0       <NA>      2012-09-20
#> 270            3       NA    0    NA NA 2019-12-31            <NA>
#> 271            4        0    1     1 NA 2016-05-01      2015-07-18
#> 272            4       NA   NA     1  0       <NA>            <NA>
#> 273            1       NA    1     0 NA       <NA>      2015-09-24
#> 274            3       NA    1    NA NA       <NA>      2015-04-09
#> 275            4       NA    1    NA  0       <NA>            <NA>
#> 276            2       NA   NA     1 NA       <NA>      2011-07-11
#> 277            0       NA    1     1  1 2017-03-07      2012-08-25
#> 278            2        1   NA     0  0       <NA>      2010-01-21
#> 279            1        1    0    NA NA 2019-07-09            <NA>
#> 280            4       NA    0     1 NA       <NA>      2015-08-03
#> 281            3       NA    1    NA NA 2016-02-19      2013-08-19
#> 282            2        1    1     0  1       <NA>            <NA>
#> 283            3        1    0    NA  0       <NA>            <NA>
#> 284            1        1   NA     1  0 2020-08-30      2010-08-29
#> 285            1        1    0     0 NA 2020-12-07            <NA>
#> 286            2       NA    1     0 NA       <NA>      2015-09-18
#> 287            2        0    0     0 NA       <NA>      2013-01-28
#> 288            2        1    0    NA  0       <NA>            <NA>
#> 289            4        0    1     1 NA       <NA>            <NA>
#> 290            4       NA    1     1  0       <NA>      2012-05-26
#> 291            1        0    0    NA NA       <NA>            <NA>
#> 292            1        1   NA     0  0       <NA>            <NA>
#> 293            3        1    0     0  1       <NA>            <NA>
#> 294            0        1    1    NA  1       <NA>      2014-06-18
#> 295            3        1   NA     1  1 2016-06-15            <NA>
#> 296            1       NA    0     0 NA 2019-06-08            <NA>
#> 297            3        0   NA    NA  1 2018-12-12            <NA>
#> 298            4        1   NA     0  0       <NA>            <NA>
#> 299            1       NA   NA     0  0 2019-03-06      2013-03-06
#> 300            4       NA    1     0 NA       <NA>            <NA>
#> 301            2       NA   NA     1 NA       <NA>            <NA>
#> 302            3        0   NA    NA NA 2017-05-20            <NA>
#> 303            3       NA    1     0 NA       <NA>      2010-09-02
#> 304            4        0    0     1  1       <NA>            <NA>
#> 305            2       NA   NA    NA  1       <NA>            <NA>
#> 306            1        0   NA     0  0 2018-10-17      2014-07-13
#> 307            3        1    1    NA NA       <NA>      2010-11-12
#> 308            2        0   NA    NA  1 2018-09-20            <NA>
#> 309            3       NA    1    NA  1       <NA>      2011-03-16
#> 310            2        1   NA     1 NA 2016-06-09      2014-07-03
#> 311            3        0    0     0 NA       <NA>            <NA>
#> 312            1        0   NA    NA  0       <NA>            <NA>
#> 313            3        0   NA     1  0 2018-03-22            <NA>
#> 314            1        1   NA     0  1       <NA>            <NA>
#> 315            3        0   NA     1  0 2020-01-21            <NA>
#> 316            3        1    1     1  0       <NA>            <NA>
#> 317            1        1    1     1 NA 2016-07-08      2011-08-23
#> 318            3        1    1     1 NA 2018-07-18      2012-03-15
#> 319            4        0    0     0  0       <NA>            <NA>
#> 320            2        0   NA     0 NA       <NA>            <NA>
#> 321            1       NA   NA    NA  1 2016-01-31            <NA>
#> 322            3       NA   NA     1 NA 2016-07-26            <NA>
#> 323            3        0    0    NA  0       <NA>      2015-04-16
#> 324            3        1    1     1 NA       <NA>            <NA>
#> 325            5       NA    1    NA  1       <NA>      2015-07-10
#> 326            2        0   NA     1  1 2018-06-24            <NA>
#> 327            2        0   NA     1 NA 2017-09-27            <NA>
#> 328            2       NA    0    NA  1       <NA>            <NA>
#> 329            3       NA    0     0  0       <NA>      2015-03-11
#> 330            3        1   NA     0 NA 2017-09-04            <NA>
#> 331            2        1    1     0 NA 2018-12-17            <NA>
#> 332            2       NA    1     1 NA 2016-04-19            <NA>
#> 333            1        0    0     1  1       <NA>            <NA>
#> 334            3        1   NA     1  0 2019-04-01            <NA>
#> 335            2        1    1    NA  0 2019-02-27            <NA>
#> 336            3       NA   NA    NA  0       <NA>            <NA>
#> 337            2        0    0     1 NA       <NA>      2014-05-23
#> 338            1       NA   NA     1  1 2018-09-13            <NA>
#> 339            1       NA   NA     0  0       <NA>      2010-03-29
#> 340            2        1   NA    NA  0 2016-03-17      2013-10-01
#> 341            2        0    1     0  0 2020-07-20            <NA>
#> 342            3        0    0    NA  0 2017-08-04      2010-09-22
#> 343            5        1    1    NA  1 2019-09-13            <NA>
#> 344            2        1    1    NA  0       <NA>      2012-10-19
#> 345            5        1   NA    NA  1       <NA>            <NA>
#> 346            3       NA    0     1 NA 2016-03-13            <NA>
#> 347            3       NA    0     0  0       <NA>            <NA>
#> 348            2       NA   NA     0 NA 2019-09-06            <NA>
#> 349            1        0   NA    NA  1       <NA>            <NA>
#> 350            1        1    1     1  1       <NA>      2010-11-09
#> 351            4        1    0    NA NA 2016-11-23      2015-06-23
#> 352            1        0    1     1 NA       <NA>            <NA>
#> 353            3        0    0     0  0       <NA>      2011-01-27
#> 354            4       NA   NA    NA  0 2016-07-09      2014-04-12
#> 355            2        0   NA     1 NA       <NA>            <NA>
#> 356            2       NA    0     1 NA       <NA>            <NA>
#> 357            3        1    1     0 NA 2017-08-19            <NA>
#> 358            2        1    1    NA NA 2016-08-06      2013-10-11
#> 359            2        1   NA    NA  0       <NA>      2012-04-24
#> 360            4        0    1     0  1       <NA>            <NA>
#> 361            2        0   NA    NA  0       <NA>      2013-05-19
#> 362            2       NA    1    NA  0       <NA>      2014-11-25
#> 363            3        0    0     0  1 2018-07-15      2015-01-27
#> 364            2       NA    0    NA NA       <NA>            <NA>
#> 365            3        1    1    NA  0 2019-07-24      2014-01-08
#> 366            3        1    0    NA NA 2020-09-26            <NA>
#> 367            2       NA    1     1 NA       <NA>      2010-03-24
#> 368            2        1   NA    NA NA       <NA>            <NA>
#> 369            4        0    0    NA  0       <NA>      2010-02-13
#> 370            3        1   NA     0  0       <NA>            <NA>
#> 371            0        0    1    NA  1 2017-05-14      2014-01-12
#> 372            5       NA    0     1  0 2019-03-07            <NA>
#> 373            3       NA   NA    NA  1       <NA>      2014-07-25
#> 374            2        0    0     1  0       <NA>      2015-09-30
#> 375            2        1    0     1  0       <NA>            <NA>
#> 376            3        1   NA     0  1 2019-01-07            <NA>
#> 377            1       NA   NA     1  0 2017-04-12            <NA>
#> 378            2        1   NA     0 NA 2018-04-26            <NA>
#> 379            2       NA    0     0  0       <NA>      2013-03-05
#> 380            3        1    0    NA  0 2019-02-24      2011-01-10
#> 381            5        0    0    NA  0       <NA>            <NA>
#> 382            1        0    1     0  0       <NA>            <NA>
#> 383            2        1   NA    NA NA       <NA>      2015-02-10
#> 384            1       NA    0     0  1       <NA>      2014-04-03
#> 385            2        1    0     0  0       <NA>            <NA>
#> 386            3        1    1    NA  0       <NA>      2011-01-22
#> 387            4        1    0     0 NA       <NA>      2014-05-03
#> 388            2        0    0     1  0       <NA>            <NA>
#> 389            3        0    1    NA  0 2017-04-25      2012-12-11
#> 390            3        0    1     0 NA 2016-12-16            <NA>
#> 391            0        0    1     1  0 2018-06-16      2015-01-05
#> 392            4        1   NA    NA  0       <NA>      2012-04-06
#> 393            3        1    0     1  0       <NA>      2011-05-08
#> 394            2       NA    1     0 NA 2018-05-16      2013-10-28
#> 395            4        1    0    NA  0 2019-04-08      2010-05-10
#> 396            2        1    0     1  0       <NA>      2012-02-17
#> 397            3        0    0     1 NA 2018-09-14      2012-02-25
#> 398            2        1    1     0  1       <NA>      2012-07-03
#> 399            5        1   NA     1  0 2019-07-07            <NA>
#> 400            2       NA    1     1  1 2017-11-28      2013-09-24
#> 401            2       NA    0    NA NA       <NA>            <NA>
#> 402            2       NA    1     0 NA       <NA>      2011-12-20
#> 403            3        0    1     0  1       <NA>            <NA>
#> 404            3        1   NA    NA  0 2020-12-01      2015-07-02
#> 405            3        1   NA     0  0 2017-08-23            <NA>
#> 406            3        1   NA     1 NA       <NA>            <NA>
#> 407            4       NA    0     1  1       <NA>      2013-12-27
#> 408            3        1   NA    NA  0 2018-04-13      2014-02-10
#> 409            2        0    0    NA NA 2018-06-25            <NA>
#> 410            2        0    1     0 NA       <NA>      2015-06-27
#> 411            2        1    0     0  1       <NA>      2010-06-13
#> 412            3        0   NA    NA  0       <NA>      2010-04-18
#> 413            1        0   NA     1  1 2019-10-17            <NA>
#> 414            2        1   NA    NA  0       <NA>      2011-05-24
#> 415            3        1    1     1  1       <NA>            <NA>
#> 416            1        1    0    NA  1 2019-05-18      2011-02-06
#> 417            2       NA    1    NA  0       <NA>            <NA>
#> 418            3       NA   NA     1  0       <NA>            <NA>
#> 419            2        1    1    NA NA 2020-06-15            <NA>
#> 420            1        1    0     0  1 2016-10-20            <NA>
#> 421            4       NA   NA    NA NA       <NA>      2014-04-02
#> 422            2        0    0    NA  0       <NA>      2014-10-26
#> 423            1       NA   NA    NA  1       <NA>      2011-03-19
#> 424            0        1   NA    NA  0 2018-01-14            <NA>
#> 425            2        0   NA     1 NA       <NA>      2011-08-17
#> 426            4        0    1    NA  0       <NA>            <NA>
#> 427            4        1    1     0 NA       <NA>      2010-08-16
#> 428            1        1   NA    NA  0 2018-04-04            <NA>
#> 429            0       NA    1    NA NA       <NA>      2013-03-31
#> 430            0        1    0     0  0 2017-05-02            <NA>
#> 431            3        1    0    NA  1 2018-11-20            <NA>
#> 432            2        0    1     1  0       <NA>      2011-09-06
#> 433            4       NA   NA    NA  0 2017-03-04            <NA>
#> 434            3        1   NA     1  1       <NA>      2013-07-16
#> 435            2        1    0     1 NA       <NA>            <NA>
#> 436            2        0   NA     1  1       <NA>      2011-09-20
#> 437            2        1    1    NA  0       <NA>      2012-01-03
#> 438            2       NA    0     0  1 2017-09-25            <NA>
#> 439            1        0    0     1  0 2019-05-04      2010-02-03
#> 440            1        0    1     1 NA       <NA>            <NA>
#> 441            2       NA    1    NA  0       <NA>      2011-07-17
#> 442            3        0   NA    NA NA       <NA>      2010-08-20
#> 443            2       NA    1     0 NA       <NA>      2014-05-04
#> 444            2        1    1    NA  0       <NA>      2011-03-03
#> 445            3        1   NA     1 NA 2017-11-15      2012-08-03
#> 446            2       NA    0     1 NA       <NA>      2010-08-23
#> 447            4        1    1     1  0       <NA>      2013-06-17
#> 448            4        0   NA     1 NA 2017-10-30      2012-02-26
#> 449            2       NA    1     1  1       <NA>            <NA>
#> 450            4       NA    0     0  1       <NA>            <NA>
#> 451            4       NA    1     1  1       <NA>      2013-05-23
#> 452            4        0    1    NA NA       <NA>      2011-09-14
#> 453            3        1    1     0  0       <NA>            <NA>
#> 454            3        1   NA     1 NA       <NA>      2015-01-09
#> 455            1        0    1    NA  1 2020-07-18      2014-09-18
#> 456            3        0   NA     1  0 2017-06-09      2013-05-16
#> 457            2        1   NA     0  1 2019-04-05      2010-08-15
#> 458            3        1    0    NA NA 2018-02-03      2014-01-25
#> 459            2        0    1    NA  0       <NA>            <NA>
#> 460            1        0   NA    NA  1 2019-01-16      2014-07-30
#> 461            4        1    0     0  0       <NA>      2014-04-11
#> 462            3       NA    0     0  1       <NA>            <NA>
#> 463            4        0    0    NA  0       <NA>            <NA>
#> 464            5       NA   NA     1 NA       <NA>      2014-08-06
#> 465            3        1   NA     1  1 2019-04-24      2015-03-09
#> 466            4       NA    0     0 NA 2018-06-06      2010-06-07
#> 467            1       NA    0    NA  0       <NA>      2013-06-25
#> 468            3        0   NA     0  0       <NA>            <NA>
#> 469            1        0    0     1  0       <NA>      2013-11-22
#> 470            4       NA   NA    NA  0 2020-05-22            <NA>
#> 471            2        0    1     1  0       <NA>            <NA>
#> 472            4       NA    1     1 NA 2019-11-16            <NA>
#> 473            2        1    0     1  1       <NA>            <NA>
#> 474            3       NA    1     0 NA 2018-12-26            <NA>
#> 475            4        0   NA     1  1       <NA>      2013-11-10
#> 476            3        0   NA     1 NA       <NA>      2013-04-03
#> 477            4        1    0     0  1 2020-09-02            <NA>
#> 478            3        0    0    NA  1 2020-10-20            <NA>
#> 479            3        1    0     0 NA       <NA>      2011-01-24
#> 480            4        0    1     1  0       <NA>            <NA>
#> 481            1        0    0    NA  0       <NA>            <NA>
#> 482            3       NA    0     1 NA 2016-10-28            <NA>
#> 483            3        0    1    NA  0       <NA>            <NA>
#> 484            3       NA    1     1  1       <NA>      2014-06-22
#> 485            2        1    0     0  1       <NA>            <NA>
#> 486            3       NA    0     1 NA 2017-01-02            <NA>
#> 487            0        1    1     0  0       <NA>      2010-04-09
#> 488            3       NA   NA    NA NA       <NA>            <NA>
#> 489            2       NA   NA     1 NA       <NA>      2010-09-12
#> 490            3       NA    0     0 NA       <NA>            <NA>
#> 491            3        0   NA     0  0       <NA>            <NA>
#> 492            3        1   NA     0  1 2019-06-20      2010-06-10
#> 493            3        0    0    NA  1       <NA>      2011-04-04
#> 494            2        0   NA     1 NA       <NA>      2010-12-03
#> 495            2        1   NA     1 NA       <NA>            <NA>
#> 496            3       NA   NA    NA  0       <NA>            <NA>
#> 497            2        1    0    NA  1       <NA>            <NA>
#> 498            2        0    1     1  0       <NA>      2013-11-04
#> 499            3       NA    1    NA NA 2016-03-26      2011-07-29
#> 500            1        1    1     1 NA 2020-04-18      2015-02-01
#>     metastasis_date recurrence_date_bin metastasis_date_bin
#> 1              <NA>                <NA>                <NA>
#> 2              <NA>           2010-2020                <NA>
#> 3              <NA>                <NA>                <NA>
#> 4        2012-04-04                <NA>           2010-2020
#> 5        2011-01-16                <NA>           2010-2020
#> 6              <NA>                <NA>                <NA>
#> 7              <NA>           2010-2020                <NA>
#> 8              <NA>                <NA>                <NA>
#> 9        2010-08-17                <NA>           2010-2020
#> 10             <NA>           2010-2020                <NA>
#> 11             <NA>                <NA>                <NA>
#> 12             <NA>                <NA>                <NA>
#> 13             <NA>           2010-2020                <NA>
#> 14             <NA>                <NA>                <NA>
#> 15       2015-09-01                <NA>           2010-2020
#> 16             <NA>           2010-2020                <NA>
#> 17       2012-08-18                <NA>           2010-2020
#> 18       2014-09-13                <NA>           2010-2020
#> 19       2015-10-01           2010-2020           2010-2020
#> 20             <NA>                <NA>                <NA>
#> 21             <NA>                <NA>                <NA>
#> 22             <NA>                <NA>                <NA>
#> 23       2010-04-11           2010-2020           2010-2020
#> 24             <NA>                <NA>                <NA>
#> 25             <NA>                <NA>                <NA>
#> 26             <NA>                <NA>                <NA>
#> 27             <NA>                <NA>                <NA>
#> 28       2015-11-02           2010-2020           2010-2020
#> 29       2011-11-24                <NA>           2010-2020
#> 30             <NA>                <NA>                <NA>
#> 31             <NA>           2010-2020                <NA>
#> 32       2011-04-26                <NA>           2010-2020
#> 33       2010-04-20           2010-2020           2010-2020
#> 34             <NA>                <NA>                <NA>
#> 35             <NA>                <NA>                <NA>
#> 36       2014-08-11                <NA>           2010-2020
#> 37             <NA>                <NA>                <NA>
#> 38             <NA>           2010-2020                <NA>
#> 39       2014-03-28           2010-2020           2010-2020
#> 40             <NA>                <NA>                <NA>
#> 41       2010-09-11           2010-2020           2010-2020
#> 42             <NA>           2010-2020                <NA>
#> 43             <NA>                <NA>                <NA>
#> 44             <NA>                <NA>                <NA>
#> 45       2014-09-05           2010-2020           2010-2020
#> 46             <NA>                <NA>                <NA>
#> 47             <NA>           2010-2020                <NA>
#> 48       2010-11-22           2010-2020           2010-2020
#> 49       2014-10-09                <NA>           2010-2020
#> 50             <NA>                <NA>                <NA>
#> 51       2015-11-27                <NA>           2010-2020
#> 52             <NA>           2010-2020                <NA>
#> 53             <NA>           2010-2020                <NA>
#> 54             <NA>           2010-2020                <NA>
#> 55             <NA>           2010-2020                <NA>
#> 56       2010-03-25                <NA>           2010-2020
#> 57       2013-11-25                <NA>           2010-2020
#> 58       2015-08-06           2010-2020           2010-2020
#> 59             <NA>           2010-2020                <NA>
#> 60             <NA>           2010-2020                <NA>
#> 61             <NA>           2010-2020                <NA>
#> 62             <NA>           2010-2020                <NA>
#> 63             <NA>                <NA>                <NA>
#> 64       2010-03-31           2010-2020           2010-2020
#> 65             <NA>                <NA>                <NA>
#> 66       2010-02-23           2010-2020           2010-2020
#> 67       2013-04-13           2010-2020           2010-2020
#> 68             <NA>           2010-2020                <NA>
#> 69             <NA>                <NA>                <NA>
#> 70       2011-02-11                <NA>           2010-2020
#> 71             <NA>           2010-2020                <NA>
#> 72       2010-12-14           2010-2020           2010-2020
#> 73       2011-11-12           2010-2020           2010-2020
#> 74       2014-08-27           2010-2020           2010-2020
#> 75             <NA>                <NA>                <NA>
#> 76             <NA>           2010-2020                <NA>
#> 77             <NA>                <NA>                <NA>
#> 78             <NA>                <NA>                <NA>
#> 79       2014-03-02                <NA>           2010-2020
#> 80             <NA>                <NA>                <NA>
#> 81             <NA>                <NA>                <NA>
#> 82       2015-10-20                <NA>           2010-2020
#> 83       2014-12-29           2010-2020           2010-2020
#> 84             <NA>           2010-2020                <NA>
#> 85             <NA>                <NA>                <NA>
#> 86             <NA>                <NA>                <NA>
#> 87             <NA>           2010-2020                <NA>
#> 88       2010-03-15                <NA>           2010-2020
#> 89             <NA>                <NA>                <NA>
#> 90             <NA>                <NA>                <NA>
#> 91       2012-06-23           2010-2020           2010-2020
#> 92             <NA>           2010-2020                <NA>
#> 93             <NA>                <NA>                <NA>
#> 94             <NA>           2010-2020                <NA>
#> 95             <NA>           2010-2020                <NA>
#> 96             <NA>                <NA>                <NA>
#> 97             <NA>           2010-2020                <NA>
#> 98             <NA>           2010-2020                <NA>
#> 99             <NA>           2010-2020                <NA>
#> 100            <NA>                <NA>                <NA>
#> 101      2013-08-29           2010-2020           2010-2020
#> 102            <NA>           2010-2020                <NA>
#> 103      2010-08-25                <NA>           2010-2020
#> 104      2013-08-13                <NA>           2010-2020
#> 105            <NA>           2010-2020                <NA>
#> 106            <NA>           2010-2020                <NA>
#> 107            <NA>                <NA>                <NA>
#> 108      2012-04-03           2010-2020           2010-2020
#> 109            <NA>           2010-2020                <NA>
#> 110            <NA>                <NA>                <NA>
#> 111            <NA>           2010-2020                <NA>
#> 112            <NA>           2010-2020                <NA>
#> 113            <NA>           2010-2020                <NA>
#> 114      2015-04-21                <NA>           2010-2020
#> 115      2014-11-13                <NA>           2010-2020
#> 116      2014-11-17           2010-2020           2010-2020
#> 117            <NA>                <NA>                <NA>
#> 118      2012-06-20                <NA>           2010-2020
#> 119            <NA>           2010-2020                <NA>
#> 120            <NA>           2010-2020                <NA>
#> 121            <NA>           2010-2020                <NA>
#> 122            <NA>                <NA>                <NA>
#> 123            <NA>                <NA>                <NA>
#> 124            <NA>                <NA>                <NA>
#> 125      2012-02-06                <NA>           2010-2020
#> 126            <NA>                <NA>                <NA>
#> 127            <NA>                <NA>                <NA>
#> 128            <NA>                <NA>                <NA>
#> 129            <NA>                <NA>                <NA>
#> 130            <NA>           2010-2020                <NA>
#> 131            <NA>           2010-2020                <NA>
#> 132            <NA>                <NA>                <NA>
#> 133      2014-07-23                <NA>           2010-2020
#> 134            <NA>                <NA>                <NA>
#> 135            <NA>                <NA>                <NA>
#> 136            <NA>                <NA>                <NA>
#> 137            <NA>                <NA>                <NA>
#> 138      2015-06-01           2010-2020           2010-2020
#> 139            <NA>           2010-2020                <NA>
#> 140      2010-12-18                <NA>           2010-2020
#> 141            <NA>                <NA>                <NA>
#> 142      2015-02-10           2010-2020           2010-2020
#> 143            <NA>                <NA>                <NA>
#> 144      2013-07-10                <NA>           2010-2020
#> 145            <NA>           2010-2020                <NA>
#> 146      2013-06-06                <NA>           2010-2020
#> 147            <NA>           2010-2020                <NA>
#> 148            <NA>           2010-2020                <NA>
#> 149      2013-07-18                <NA>           2010-2020
#> 150            <NA>                <NA>                <NA>
#> 151            <NA>           2010-2020                <NA>
#> 152            <NA>                <NA>                <NA>
#> 153      2011-08-20           2010-2020           2010-2020
#> 154      2011-01-18                <NA>           2010-2020
#> 155            <NA>           2010-2020                <NA>
#> 156            <NA>                <NA>                <NA>
#> 157            <NA>                <NA>                <NA>
#> 158            <NA>           2010-2020                <NA>
#> 159      2014-03-07                <NA>           2010-2020
#> 160            <NA>           2010-2020                <NA>
#> 161      2013-11-19           2010-2020           2010-2020
#> 162            <NA>                <NA>                <NA>
#> 163      2014-04-25           2010-2020           2010-2020
#> 164            <NA>                <NA>                <NA>
#> 165            <NA>           2010-2020                <NA>
#> 166            <NA>                <NA>                <NA>
#> 167      2011-04-01           2010-2020           2010-2020
#> 168            <NA>           2010-2020                <NA>
#> 169            <NA>                <NA>                <NA>
#> 170            <NA>           2010-2020                <NA>
#> 171            <NA>           2010-2020                <NA>
#> 172            <NA>                <NA>                <NA>
#> 173      2011-09-03           2010-2020           2010-2020
#> 174      2012-05-15           2010-2020           2010-2020
#> 175      2011-02-19                <NA>           2010-2020
#> 176            <NA>           2010-2020                <NA>
#> 177            <NA>           2010-2020                <NA>
#> 178            <NA>           2010-2020                <NA>
#> 179      2011-10-19           2010-2020           2010-2020
#> 180            <NA>                <NA>                <NA>
#> 181            <NA>           2010-2020                <NA>
#> 182      2015-06-30           2010-2020           2010-2020
#> 183            <NA>           2010-2020                <NA>
#> 184            <NA>                <NA>                <NA>
#> 185      2011-03-13                <NA>           2010-2020
#> 186            <NA>                <NA>                <NA>
#> 187            <NA>                <NA>                <NA>
#> 188      2010-07-04                <NA>           2010-2020
#> 189            <NA>                <NA>                <NA>
#> 190      2014-10-20           2010-2020           2010-2020
#> 191            <NA>                <NA>                <NA>
#> 192            <NA>                <NA>                <NA>
#> 193      2014-04-27                <NA>           2010-2020
#> 194            <NA>                <NA>                <NA>
#> 195            <NA>                <NA>                <NA>
#> 196            <NA>           2010-2020                <NA>
#> 197            <NA>           2010-2020                <NA>
#> 198      2012-11-18           2010-2020           2010-2020
#> 199            <NA>           2010-2020                <NA>
#> 200            <NA>           2010-2020                <NA>
#> 201            <NA>           2010-2020                <NA>
#> 202            <NA>                <NA>                <NA>
#> 203            <NA>           2010-2020                <NA>
#> 204            <NA>                <NA>                <NA>
#> 205      2010-10-30           2010-2020           2010-2020
#> 206            <NA>           2010-2020                <NA>
#> 207      2012-01-27           2010-2020           2010-2020
#> 208      2012-03-10                <NA>           2010-2020
#> 209            <NA>           2010-2020                <NA>
#> 210            <NA>           2010-2020                <NA>
#> 211            <NA>                <NA>                <NA>
#> 212            <NA>                <NA>                <NA>
#> 213      2013-08-03                <NA>           2010-2020
#> 214      2013-09-05           2010-2020           2010-2020
#> 215      2015-05-13                <NA>           2010-2020
#> 216      2015-01-05                <NA>           2010-2020
#> 217      2011-06-09                <NA>           2010-2020
#> 218            <NA>                <NA>                <NA>
#> 219            <NA>           2010-2020                <NA>
#> 220      2010-03-14           2010-2020           2010-2020
#> 221      2015-06-26           2010-2020           2010-2020
#> 222            <NA>                <NA>                <NA>
#> 223            <NA>                <NA>                <NA>
#> 224      2015-07-30           2010-2020           2010-2020
#> 225      2014-06-07                <NA>           2010-2020
#> 226      2010-06-29           2010-2020           2010-2020
#> 227            <NA>           2010-2020                <NA>
#> 228            <NA>                <NA>                <NA>
#> 229            <NA>                <NA>                <NA>
#> 230      2013-09-04           2010-2020           2010-2020
#> 231            <NA>                <NA>                <NA>
#> 232            <NA>           2010-2020                <NA>
#> 233      2014-04-21           2010-2020           2010-2020
#> 234            <NA>                <NA>                <NA>
#> 235            <NA>                <NA>                <NA>
#> 236      2011-04-13           2010-2020           2010-2020
#> 237            <NA>           2010-2020                <NA>
#> 238            <NA>                <NA>                <NA>
#> 239            <NA>           2010-2020                <NA>
#> 240            <NA>           2010-2020                <NA>
#> 241            <NA>           2010-2020                <NA>
#> 242      2010-10-23                <NA>           2010-2020
#> 243            <NA>                <NA>                <NA>
#> 244      2010-11-08                <NA>           2010-2020
#> 245      2015-07-11           2010-2020           2010-2020
#> 246            <NA>                <NA>                <NA>
#> 247      2010-01-29           2010-2020           2010-2020
#> 248            <NA>                <NA>                <NA>
#> 249      2015-06-05                <NA>           2010-2020
#> 250            <NA>                <NA>                <NA>
#> 251      2013-01-23                <NA>           2010-2020
#> 252            <NA>           2010-2020                <NA>
#> 253            <NA>           2010-2020                <NA>
#> 254            <NA>                <NA>                <NA>
#> 255            <NA>           2010-2020                <NA>
#> 256            <NA>                <NA>                <NA>
#> 257      2013-09-24                <NA>           2010-2020
#> 258            <NA>                <NA>                <NA>
#> 259      2011-09-08           2010-2020           2010-2020
#> 260            <NA>                <NA>                <NA>
#> 261            <NA>           2010-2020                <NA>
#> 262      2015-08-09                <NA>           2010-2020
#> 263            <NA>                <NA>                <NA>
#> 264      2010-04-13                <NA>           2010-2020
#> 265            <NA>                <NA>                <NA>
#> 266            <NA>           2010-2020                <NA>
#> 267            <NA>                <NA>                <NA>
#> 268            <NA>           2010-2020                <NA>
#> 269            <NA>           2010-2020                <NA>
#> 270            <NA>                <NA>                <NA>
#> 271      2013-04-25           2010-2020           2010-2020
#> 272            <NA>                <NA>                <NA>
#> 273            <NA>           2010-2020                <NA>
#> 274            <NA>           2010-2020                <NA>
#> 275      2011-02-06                <NA>           2010-2020
#> 276            <NA>           2010-2020                <NA>
#> 277      2011-10-28           2010-2020           2010-2020
#> 278      2011-04-12           2010-2020           2010-2020
#> 279            <NA>                <NA>                <NA>
#> 280            <NA>           2010-2020                <NA>
#> 281      2013-12-11           2010-2020           2010-2020
#> 282            <NA>                <NA>                <NA>
#> 283            <NA>                <NA>                <NA>
#> 284      2012-08-01           2010-2020           2010-2020
#> 285            <NA>                <NA>                <NA>
#> 286            <NA>           2010-2020                <NA>
#> 287            <NA>           2010-2020                <NA>
#> 288            <NA>                <NA>                <NA>
#> 289      2012-03-31                <NA>           2010-2020
#> 290            <NA>           2010-2020                <NA>
#> 291            <NA>                <NA>                <NA>
#> 292      2012-01-02                <NA>           2010-2020
#> 293            <NA>                <NA>                <NA>
#> 294            <NA>           2010-2020                <NA>
#> 295            <NA>                <NA>                <NA>
#> 296      2012-12-25                <NA>           2010-2020
#> 297            <NA>                <NA>                <NA>
#> 298      2014-04-04                <NA>           2010-2020
#> 299      2012-11-12           2010-2020           2010-2020
#> 300            <NA>                <NA>                <NA>
#> 301      2014-10-24                <NA>           2010-2020
#> 302            <NA>                <NA>                <NA>
#> 303      2012-04-13           2010-2020           2010-2020
#> 304      2015-11-19                <NA>           2010-2020
#> 305      2011-02-10                <NA>           2010-2020
#> 306            <NA>           2010-2020                <NA>
#> 307      2011-06-28           2010-2020           2010-2020
#> 308      2013-07-27                <NA>           2010-2020
#> 309            <NA>           2010-2020                <NA>
#> 310            <NA>           2010-2020                <NA>
#> 311      2015-11-06                <NA>           2010-2020
#> 312            <NA>                <NA>                <NA>
#> 313            <NA>                <NA>                <NA>
#> 314      2015-09-27                <NA>           2010-2020
#> 315      2012-02-01                <NA>           2010-2020
#> 316      2013-11-23                <NA>           2010-2020
#> 317      2013-01-31           2010-2020           2010-2020
#> 318      2010-04-12           2010-2020           2010-2020
#> 319            <NA>                <NA>                <NA>
#> 320      2010-09-18                <NA>           2010-2020
#> 321      2013-03-17                <NA>           2010-2020
#> 322            <NA>                <NA>                <NA>
#> 323            <NA>           2010-2020                <NA>
#> 324            <NA>                <NA>                <NA>
#> 325            <NA>           2010-2020                <NA>
#> 326            <NA>                <NA>                <NA>
#> 327      2015-07-28                <NA>           2010-2020
#> 328            <NA>                <NA>                <NA>
#> 329      2014-07-09           2010-2020           2010-2020
#> 330            <NA>                <NA>                <NA>
#> 331            <NA>                <NA>                <NA>
#> 332      2015-07-14                <NA>           2010-2020
#> 333            <NA>                <NA>                <NA>
#> 334            <NA>                <NA>                <NA>
#> 335      2014-07-01                <NA>           2010-2020
#> 336            <NA>                <NA>                <NA>
#> 337      2013-03-01           2010-2020           2010-2020
#> 338      2013-08-27                <NA>           2010-2020
#> 339      2013-09-12           2010-2020           2010-2020
#> 340            <NA>           2010-2020                <NA>
#> 341      2014-08-30                <NA>           2010-2020
#> 342            <NA>           2010-2020                <NA>
#> 343            <NA>                <NA>                <NA>
#> 344            <NA>           2010-2020                <NA>
#> 345            <NA>                <NA>                <NA>
#> 346            <NA>                <NA>                <NA>
#> 347      2015-07-19                <NA>           2010-2020
#> 348            <NA>                <NA>                <NA>
#> 349            <NA>                <NA>                <NA>
#> 350            <NA>           2010-2020                <NA>
#> 351            <NA>           2010-2020                <NA>
#> 352            <NA>                <NA>                <NA>
#> 353      2015-05-29           2010-2020           2010-2020
#> 354            <NA>           2010-2020                <NA>
#> 355            <NA>                <NA>                <NA>
#> 356      2010-01-07                <NA>           2010-2020
#> 357      2014-01-27                <NA>           2010-2020
#> 358            <NA>           2010-2020                <NA>
#> 359            <NA>           2010-2020                <NA>
#> 360            <NA>                <NA>                <NA>
#> 361            <NA>           2010-2020                <NA>
#> 362      2015-02-28           2010-2020           2010-2020
#> 363      2013-02-22           2010-2020           2010-2020
#> 364            <NA>                <NA>                <NA>
#> 365      2013-03-03           2010-2020           2010-2020
#> 366            <NA>                <NA>                <NA>
#> 367      2014-07-19           2010-2020           2010-2020
#> 368            <NA>                <NA>                <NA>
#> 369            <NA>           2010-2020                <NA>
#> 370            <NA>                <NA>                <NA>
#> 371            <NA>           2010-2020                <NA>
#> 372      2015-04-02                <NA>           2010-2020
#> 373            <NA>           2010-2020                <NA>
#> 374      2014-12-14           2010-2020           2010-2020
#> 375            <NA>                <NA>                <NA>
#> 376      2010-03-09                <NA>           2010-2020
#> 377      2012-12-29                <NA>           2010-2020
#> 378            <NA>                <NA>                <NA>
#> 379            <NA>           2010-2020                <NA>
#> 380      2014-04-12           2010-2020           2010-2020
#> 381            <NA>                <NA>                <NA>
#> 382            <NA>                <NA>                <NA>
#> 383      2012-11-08           2010-2020           2010-2020
#> 384            <NA>           2010-2020                <NA>
#> 385            <NA>                <NA>                <NA>
#> 386      2010-01-21           2010-2020           2010-2020
#> 387            <NA>           2010-2020                <NA>
#> 388            <NA>                <NA>                <NA>
#> 389            <NA>           2010-2020                <NA>
#> 390            <NA>                <NA>                <NA>
#> 391      2010-08-22           2010-2020           2010-2020
#> 392            <NA>           2010-2020                <NA>
#> 393            <NA>           2010-2020                <NA>
#> 394            <NA>           2010-2020                <NA>
#> 395            <NA>           2010-2020                <NA>
#> 396            <NA>           2010-2020                <NA>
#> 397      2010-05-13           2010-2020           2010-2020
#> 398            <NA>           2010-2020                <NA>
#> 399            <NA>                <NA>                <NA>
#> 400            <NA>           2010-2020                <NA>
#> 401            <NA>                <NA>                <NA>
#> 402            <NA>           2010-2020                <NA>
#> 403            <NA>                <NA>                <NA>
#> 404            <NA>           2010-2020                <NA>
#> 405      2011-08-29                <NA>           2010-2020
#> 406            <NA>                <NA>                <NA>
#> 407      2010-04-03           2010-2020           2010-2020
#> 408      2013-11-22           2010-2020           2010-2020
#> 409            <NA>                <NA>                <NA>
#> 410            <NA>           2010-2020                <NA>
#> 411            <NA>           2010-2020                <NA>
#> 412            <NA>           2010-2020                <NA>
#> 413      2010-02-28                <NA>           2010-2020
#> 414            <NA>           2010-2020                <NA>
#> 415            <NA>                <NA>                <NA>
#> 416      2010-07-28           2010-2020           2010-2020
#> 417            <NA>                <NA>                <NA>
#> 418            <NA>                <NA>                <NA>
#> 419      2013-11-08                <NA>           2010-2020
#> 420      2011-03-04                <NA>           2010-2020
#> 421            <NA>           2010-2020                <NA>
#> 422      2013-08-08           2010-2020           2010-2020
#> 423      2013-09-22           2010-2020           2010-2020
#> 424            <NA>                <NA>                <NA>
#> 425      2015-09-26           2010-2020           2010-2020
#> 426            <NA>                <NA>                <NA>
#> 427            <NA>           2010-2020                <NA>
#> 428      2010-05-26                <NA>           2010-2020
#> 429            <NA>           2010-2020                <NA>
#> 430            <NA>                <NA>                <NA>
#> 431            <NA>                <NA>                <NA>
#> 432            <NA>           2010-2020                <NA>
#> 433            <NA>                <NA>                <NA>
#> 434      2012-03-21           2010-2020           2010-2020
#> 435            <NA>                <NA>                <NA>
#> 436            <NA>           2010-2020                <NA>
#> 437            <NA>           2010-2020                <NA>
#> 438            <NA>                <NA>                <NA>
#> 439            <NA>           2010-2020                <NA>
#> 440            <NA>                <NA>                <NA>
#> 441            <NA>           2010-2020                <NA>
#> 442      2011-11-04           2010-2020           2010-2020
#> 443      2013-04-26           2010-2020           2010-2020
#> 444            <NA>           2010-2020                <NA>
#> 445            <NA>           2010-2020                <NA>
#> 446      2013-08-21           2010-2020           2010-2020
#> 447            <NA>           2010-2020                <NA>
#> 448      2014-02-08           2010-2020           2010-2020
#> 449            <NA>                <NA>                <NA>
#> 450            <NA>                <NA>                <NA>
#> 451      2011-06-21           2010-2020           2010-2020
#> 452            <NA>           2010-2020                <NA>
#> 453            <NA>                <NA>                <NA>
#> 454            <NA>           2010-2020                <NA>
#> 455            <NA>           2010-2020                <NA>
#> 456            <NA>           2010-2020                <NA>
#> 457            <NA>           2010-2020                <NA>
#> 458            <NA>           2010-2020                <NA>
#> 459            <NA>                <NA>                <NA>
#> 460            <NA>           2010-2020                <NA>
#> 461            <NA>           2010-2020                <NA>
#> 462            <NA>                <NA>                <NA>
#> 463      2010-08-15                <NA>           2010-2020
#> 464            <NA>           2010-2020                <NA>
#> 465            <NA>           2010-2020                <NA>
#> 466      2015-10-05           2010-2020           2010-2020
#> 467            <NA>           2010-2020                <NA>
#> 468            <NA>                <NA>                <NA>
#> 469            <NA>           2010-2020                <NA>
#> 470      2011-10-14                <NA>           2010-2020
#> 471            <NA>                <NA>                <NA>
#> 472      2015-03-16                <NA>           2010-2020
#> 473            <NA>                <NA>                <NA>
#> 474            <NA>                <NA>                <NA>
#> 475            <NA>           2010-2020                <NA>
#> 476            <NA>           2010-2020                <NA>
#> 477            <NA>                <NA>                <NA>
#> 478      2012-08-31                <NA>           2010-2020
#> 479      2014-02-17           2010-2020           2010-2020
#> 480      2015-05-09                <NA>           2010-2020
#> 481            <NA>                <NA>                <NA>
#> 482            <NA>                <NA>                <NA>
#> 483      2011-11-30                <NA>           2010-2020
#> 484      2014-05-30           2010-2020           2010-2020
#> 485            <NA>                <NA>                <NA>
#> 486            <NA>                <NA>                <NA>
#> 487            <NA>           2010-2020                <NA>
#> 488            <NA>                <NA>                <NA>
#> 489            <NA>           2010-2020                <NA>
#> 490            <NA>                <NA>                <NA>
#> 491            <NA>                <NA>                <NA>
#> 492      2014-01-18           2010-2020           2010-2020
#> 493            <NA>           2010-2020                <NA>
#> 494            <NA>           2010-2020                <NA>
#> 495      2011-05-22                <NA>           2010-2020
#> 496            <NA>                <NA>                <NA>
#> 497            <NA>                <NA>                <NA>
#> 498            <NA>           2010-2020                <NA>
#> 499      2010-10-20           2010-2020           2010-2020
#> 500            <NA>           2010-2020                <NA>
```

### Conversion to factor

In many cases it is of interest to specify a categorical variable with
certain levels or reference groups. This is done with factR(). If
nothing else is specified, factR() converts a character string into a
factor, with the level with most observations as reference. The function
str() is used to show the formatting of the type variable has changed to
factor.

``` r
redcap_df %>% 
  factR(type) %>% 
  str
#> 'data.frame':    500 obs. of  16 variables:
#>  $ id             : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ sex            : num  1 2 2 2 2 2 2 1 1 2 ...
#>  $ age            : num  40.9 57.8 32.2 58.7 29.3 18.3 52.7 36.2 42.6 55.8 ...
#>  $ birth          : chr  "26-04-1958" "18-01-1941" "30-07-1963" "31-12-1936" ...
#>  $ followup       : chr  "23-04-2021" "12-08-2024" "20-02-2022" "29-06-2024" ...
#>  $ date_of_surgery: chr  "1999-03-29" "1998-10-20" "1995-10-26" "1995-09-14" ...
#>  $ size           : num  3.99 48.69 11.25 11.61 6.98 ...
#>  $ type           : Factor w/ 3 levels "1","0","2": 2 2 2 3 2 3 2 3 3 1 ...
#>  $ localisation   : int  2 1 4 1 3 0 3 5 4 1 ...
#>  $ necrosis       : num  0 1 0 1 0 NA 1 NA 0 0 ...
#>  $ cd10           : num  NA 0 0 1 0 NA 1 1 1 NA ...
#>  $ sox10          : num  1 0 1 NA 0 NA 0 0 0 1 ...
#>  $ ck             : num  0 0 NA 0 0 0 NA 0 0 1 ...
#>  $ death_date     : chr  NA NA NA "2019-07-27" ...
#>  $ recurrence_date: chr  NA "2014-12-09" NA NA ...
#>  $ metastasis_date: chr  NA NA NA "2012-04-04" ...
```

The reference group is specified using the reference argument

``` r
redcap_df %>% 
  factR(type,
        reference = "0") %>% 
  str
#> 'data.frame':    500 obs. of  16 variables:
#>  $ id             : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ sex            : num  1 2 2 2 2 2 2 1 1 2 ...
#>  $ age            : num  40.9 57.8 32.2 58.7 29.3 18.3 52.7 36.2 42.6 55.8 ...
#>  $ birth          : chr  "26-04-1958" "18-01-1941" "30-07-1963" "31-12-1936" ...
#>  $ followup       : chr  "23-04-2021" "12-08-2024" "20-02-2022" "29-06-2024" ...
#>  $ date_of_surgery: chr  "1999-03-29" "1998-10-20" "1995-10-26" "1995-09-14" ...
#>  $ size           : num  3.99 48.69 11.25 11.61 6.98 ...
#>  $ type           : Factor w/ 3 levels "0","1","2": 1 1 1 3 1 3 1 3 3 2 ...
#>  $ localisation   : int  2 1 4 1 3 0 3 5 4 1 ...
#>  $ necrosis       : num  0 1 0 1 0 NA 1 NA 0 0 ...
#>  $ cd10           : num  NA 0 0 1 0 NA 1 1 1 NA ...
#>  $ sox10          : num  1 0 1 NA 0 NA 0 0 0 1 ...
#>  $ ck             : num  0 0 NA 0 0 0 NA 0 0 1 ...
#>  $ death_date     : chr  NA NA NA "2019-07-27" ...
#>  $ recurrence_date: chr  NA "2014-12-09" NA NA ...
#>  $ metastasis_date: chr  NA NA NA "2012-04-04" ...
```

Levels can be manually assigned

``` r
redcap_df %>% 
  factR(type,
        levels = c("2","1","0")) %>% 
  str
#> 'data.frame':    500 obs. of  16 variables:
#>  $ id             : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ sex            : num  1 2 2 2 2 2 2 1 1 2 ...
#>  $ age            : num  40.9 57.8 32.2 58.7 29.3 18.3 52.7 36.2 42.6 55.8 ...
#>  $ birth          : chr  "26-04-1958" "18-01-1941" "30-07-1963" "31-12-1936" ...
#>  $ followup       : chr  "23-04-2021" "12-08-2024" "20-02-2022" "29-06-2024" ...
#>  $ date_of_surgery: chr  "1999-03-29" "1998-10-20" "1995-10-26" "1995-09-14" ...
#>  $ size           : num  3.99 48.69 11.25 11.61 6.98 ...
#>  $ type           : Factor w/ 3 levels "2","1","0": 3 3 3 1 3 1 3 1 1 2 ...
#>  $ localisation   : int  2 1 4 1 3 0 3 5 4 1 ...
#>  $ necrosis       : num  0 1 0 1 0 NA 1 NA 0 0 ...
#>  $ cd10           : num  NA 0 0 1 0 NA 1 1 1 NA ...
#>  $ sox10          : num  1 0 1 NA 0 NA 0 0 0 1 ...
#>  $ ck             : num  0 0 NA 0 0 0 NA 0 0 1 ...
#>  $ death_date     : chr  NA NA NA "2019-07-27" ...
#>  $ recurrence_date: chr  NA "2014-12-09" NA NA ...
#>  $ metastasis_date: chr  NA NA NA "2012-04-04" ...
```

New labels can also be assigned and automatically specify levels
simultaneously

``` r
redcap_df %>% 
  factR(type,
        labels = c("benign" = "0",
                      "intermediate" = "1",
                      "malignant" = "2"),
        lab_to_lev = T) %>% 
  str
#> 'data.frame':    500 obs. of  16 variables:
#>  $ id             : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ sex            : num  1 2 2 2 2 2 2 1 1 2 ...
#>  $ age            : num  40.9 57.8 32.2 58.7 29.3 18.3 52.7 36.2 42.6 55.8 ...
#>  $ birth          : chr  "26-04-1958" "18-01-1941" "30-07-1963" "31-12-1936" ...
#>  $ followup       : chr  "23-04-2021" "12-08-2024" "20-02-2022" "29-06-2024" ...
#>  $ date_of_surgery: chr  "1999-03-29" "1998-10-20" "1995-10-26" "1995-09-14" ...
#>  $ size           : num  3.99 48.69 11.25 11.61 6.98 ...
#>  $ type           : Factor w/ 3 levels "benign","intermediate",..: 1 1 1 3 1 3 1 3 3 2 ...
#>  $ localisation   : int  2 1 4 1 3 0 3 5 4 1 ...
#>  $ necrosis       : num  0 1 0 1 0 NA 1 NA 0 0 ...
#>  $ cd10           : num  NA 0 0 1 0 NA 1 1 1 NA ...
#>  $ sox10          : num  1 0 1 NA 0 NA 0 0 0 1 ...
#>  $ ck             : num  0 0 NA 0 0 0 NA 0 0 1 ...
#>  $ death_date     : chr  NA NA NA "2019-07-27" ...
#>  $ recurrence_date: chr  NA "2014-12-09" NA NA ...
#>  $ metastasis_date: chr  NA NA NA "2012-04-04" ...
```

Lastly, all the arguments can be specified for multiple variables at
once

``` r
redcap_df %>% 
  factR(vars = c(type, sex),
        reference = list("sex" = "2"),
        levels = list("type" = c("0", "1", "2")),
        labels = list("sex" = c("f" = "1",
                                "m" = "2"),
                      "type" = c("benign" = "0",
                      "intermediate" = "1",
                      "malignant" = "2"))) %>% 
  str
#> 'data.frame':    500 obs. of  16 variables:
#>  $ id             : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ sex            : Factor w/ 2 levels "m","f": 2 1 1 1 1 1 1 2 2 1 ...
#>  $ age            : num  40.9 57.8 32.2 58.7 29.3 18.3 52.7 36.2 42.6 55.8 ...
#>  $ birth          : chr  "26-04-1958" "18-01-1941" "30-07-1963" "31-12-1936" ...
#>  $ followup       : chr  "23-04-2021" "12-08-2024" "20-02-2022" "29-06-2024" ...
#>  $ date_of_surgery: chr  "1999-03-29" "1998-10-20" "1995-10-26" "1995-09-14" ...
#>  $ size           : num  3.99 48.69 11.25 11.61 6.98 ...
#>  $ type           : Factor w/ 3 levels "benign","intermediate",..: 1 1 1 3 1 3 1 3 3 2 ...
#>  $ localisation   : int  2 1 4 1 3 0 3 5 4 1 ...
#>  $ necrosis       : num  0 1 0 1 0 NA 1 NA 0 0 ...
#>  $ cd10           : num  NA 0 0 1 0 NA 1 1 1 NA ...
#>  $ sox10          : num  1 0 1 NA 0 NA 0 0 0 1 ...
#>  $ ck             : num  0 0 NA 0 0 0 NA 0 0 1 ...
#>  $ death_date     : chr  NA NA NA "2019-07-27" ...
#>  $ recurrence_date: chr  NA "2014-12-09" NA NA ...
#>  $ metastasis_date: chr  NA NA NA "2012-04-04" ...
```

### Subset rows (filters)

If we want to keep only certain rows we use filter(). Here we limit the
dataset to patients without necrosis (necrosis = 0)

``` r
redcap_df %>% 
  filter(necrosis == 0) %>% 
  head
#>   id sex  age      birth   followup date_of_surgery      size type localisation
#> 1  1   1 40.9 26-04-1958 23-04-2021      1999-03-29  3.991349    0            2
#> 2  3   2 32.2 30-07-1963 20-02-2022      1995-10-26 11.245841    0            4
#> 3  5   2 29.3 12-01-1961 14-05-2023      1990-04-22  6.980464    0            3
#> 4  9   1 42.6 01-04-1962 25-01-2021      2004-11-09  8.699874    2            4
#> 5 10   2 55.8 04-01-1954 02-04-2024      2009-10-22 35.560789    1            1
#> 6 20   1 30.3 01-09-1978 08-03-2023      2008-12-01 25.246919    1            4
#>   necrosis cd10 sox10 ck death_date recurrence_date metastasis_date
#> 1        0   NA     1  0       <NA>            <NA>            <NA>
#> 2        0    0     1 NA       <NA>            <NA>            <NA>
#> 3        0    0     0  0 2018-04-19            <NA>      2011-01-16
#> 4        0    1     0  0       <NA>            <NA>      2010-08-17
#> 5        0   NA     1  1       <NA>      2014-12-15            <NA>
#> 6        0   NA     1  0 2020-02-17            <NA>            <NA>
```

In case of multiple conditions we use the `%in%` operator, as this
specified that the variables has one of the following values

``` r
redcap_df %>% 
  filter(localisation %in% c(1,2,3)) %>% 
  head
#>   id sex  age      birth   followup date_of_surgery      size type localisation
#> 1  1   1 40.9 26-04-1958 23-04-2021      1999-03-29  3.991349    0            2
#> 2  2   2 57.8 18-01-1941 12-08-2024      1998-10-20 48.688670    0            1
#> 3  4   2 58.7 31-12-1936 29-06-2024      1995-09-14 11.609164    2            1
#> 4  5   2 29.3 12-01-1961 14-05-2023      1990-04-22  6.980464    0            3
#> 5  7   2 52.7 14-04-1945 19-03-2024      1997-12-19 48.978832    0            3
#> 6 10   2 55.8 04-01-1954 02-04-2024      2009-10-22 35.560789    1            1
#>   necrosis cd10 sox10 ck death_date recurrence_date metastasis_date
#> 1        0   NA     1  0       <NA>            <NA>            <NA>
#> 2        1    0     0  0       <NA>      2014-12-09            <NA>
#> 3        1    1    NA  0 2019-07-27            <NA>      2012-04-04
#> 4        0    0     0  0 2018-04-19            <NA>      2011-01-16
#> 5        1    1     0 NA       <NA>      2011-10-10            <NA>
#> 6        0   NA     1  1       <NA>      2014-12-15            <NA>
```

For numerical variables we use \<, \>, ==, \>= and \<=

``` r
redcap_df %>% 
  filter(size > 20) %>% 
  head
#>   id sex  age      birth   followup date_of_surgery     size type localisation
#> 1  2   2 57.8 18-01-1941 12-08-2024      1998-10-20 48.68867    0            1
#> 2  6   2 18.3 10-12-1975 06-04-2025      1994-03-21 49.02917    2            0
#> 3  7   2 52.7 14-04-1945 19-03-2024      1997-12-19 48.97883    0            3
#> 4  8   1 36.2 01-09-1969 14-10-2024      2005-10-30 43.57744    2            5
#> 5 10   2 55.8 04-01-1954 02-04-2024      2009-10-22 35.56079    1            1
#> 6 11   1 67.4 26-09-1930 21-06-2023      1998-02-24 27.23407    0            2
#>   necrosis cd10 sox10 ck death_date recurrence_date metastasis_date
#> 1        1    0     0  0       <NA>      2014-12-09            <NA>
#> 2       NA   NA    NA  0       <NA>            <NA>            <NA>
#> 3        1    1     0 NA       <NA>      2011-10-10            <NA>
#> 4       NA    1     0  0       <NA>            <NA>            <NA>
#> 5        0   NA     1  1       <NA>      2014-12-15            <NA>
#> 6       NA   NA     1  0 2019-03-12            <NA>            <NA>
```

We can also use between() to specify an interval

``` r
redcap_df %>% 
  filter(between(size, 10,20)) %>% 
  head
#>   id sex  age      birth   followup date_of_surgery     size type localisation
#> 1  3   2 32.2 30-07-1963 20-02-2022      1995-10-26 11.24584    0            4
#> 2  4   2 58.7 31-12-1936 29-06-2024      1995-09-14 11.60916    2            1
#> 3 12   2 46.0 08-05-1962 17-02-2025      2008-04-23 11.06068    1            3
#> 4 15   1 70.7 26-03-1939 05-07-2023      2009-11-18 15.91012    0            2
#> 5 18   2 32.0 24-05-1972 12-08-2022      2004-05-31 15.68306    0            2
#> 6 19   1 23.4 27-11-1975 17-11-2024      1999-05-08 14.00870    2            3
#>   necrosis cd10 sox10 ck death_date recurrence_date metastasis_date
#> 1        0    0     1 NA       <NA>            <NA>            <NA>
#> 2        1    1    NA  0 2019-07-27            <NA>      2012-04-04
#> 3        1   NA     1 NA 2019-02-16            <NA>            <NA>
#> 4       NA    1     0 NA       <NA>            <NA>      2015-09-01
#> 5        1    0     1  0       <NA>            <NA>      2014-09-13
#> 6       NA    0     0 NA       <NA>      2010-06-11      2015-10-01
```

Multiple conditions can be combined using so-called “boolean” operators
whic are and/or/not etc. Here we keep rows where necrosis = 1 AND
localisation is 1, 2 or 3 OR size is larger than 10. Here necrosis needs
to be 1, but either of the conditions for localisation or size can be
satisified.

``` r
redcap_df %>% 
  filter(necrosis == 1 & (localisation %in% c(1,2,3) | size > 10)) %>% 
  head
#>   id sex  age      birth   followup date_of_surgery     size type localisation
#> 1  2   2 57.8 18-01-1941 12-08-2024      1998-10-20 48.68867    0            1
#> 2  4   2 58.7 31-12-1936 29-06-2024      1995-09-14 11.60916    2            1
#> 3  7   2 52.7 14-04-1945 19-03-2024      1997-12-19 48.97883    0            3
#> 4 12   2 46.0 08-05-1962 17-02-2025      2008-04-23 11.06068    1            3
#> 5 14   1 39.6 16-08-1950 24-10-2022      1990-03-12 32.02648    1            3
#> 6 16   1 58.8 01-07-1945 19-06-2024      2004-04-01 26.70794    1            4
#>   necrosis cd10 sox10 ck death_date recurrence_date metastasis_date
#> 1        1    0     0  0       <NA>      2014-12-09            <NA>
#> 2        1    1    NA  0 2019-07-27            <NA>      2012-04-04
#> 3        1    1     0 NA       <NA>      2011-10-10            <NA>
#> 4        1   NA     1 NA 2019-02-16            <NA>            <NA>
#> 5        1    0     0  0 2017-07-18            <NA>            <NA>
#> 6        1    1    NA NA       <NA>      2014-05-27            <NA>
```

### Arranging/sorting data

Sorting is done with the arrange(). If the variable needs to be in
descending order it is prefixed by a desc()

``` r
redcap_df %>% 
  arrange(size) %>% 
  head()
#>    id sex  age      birth   followup date_of_surgery     size type localisation
#> 1  28   2 51.5 17-06-1940 07-02-2021      1992-01-02 2.088167    0            2
#> 2 350   2 22.5 25-11-1971 14-07-2021      1994-06-03 2.167653    1            1
#> 3 189   2 55.5 28-10-1949 08-03-2024      2005-05-15 2.507209    2            2
#> 4 194   2 27.4 12-03-1979 02-11-2025      2006-08-21 2.561534    0            3
#> 5 241   1 64.6 23-08-1926 18-02-2025      1991-04-06 2.572398    0            3
#> 6 334   1 65.5 16-03-1941 08-08-2022      2006-09-26 2.582333    1            3
#>   necrosis cd10 sox10 ck death_date recurrence_date metastasis_date
#> 1        0   NA     1  1       <NA>      2012-01-18      2015-11-02
#> 2        1    1     1  1       <NA>      2010-11-09            <NA>
#> 3       NA    0     1  0 2017-09-21            <NA>            <NA>
#> 4       NA   NA     1  0       <NA>            <NA>            <NA>
#> 5       NA    0     1 NA       <NA>      2010-10-18            <NA>
#> 6        1   NA     1  0 2019-04-01            <NA>            <NA>
```

Descending size

``` r
redcap_df %>% 
  arrange(desc(size)) %>% 
  head
#>    id sex  age      birth   followup date_of_surgery     size type localisation
#> 1  93   2 79.7 30-05-1924 03-10-2021      2004-02-09 49.88995    0            1
#> 2 441   2 42.1 31-05-1967 05-10-2022      2009-06-26 49.51837    0            2
#> 3 230   1 31.3 05-04-1961 04-05-2021      1992-08-01 49.44014    1            3
#> 4 366   2 19.2 10-05-1979 09-12-2022      1998-07-16 49.43806    1            3
#> 5 133   1 65.1 02-02-1930 01-09-2022      1995-02-23 49.31010    1            3
#> 6 494   2 72.4 20-05-1928 05-08-2025      2000-10-15 49.28248    1            2
#>   necrosis cd10 sox10 ck death_date recurrence_date metastasis_date
#> 1       NA    1     0  0       <NA>            <NA>            <NA>
#> 2       NA    1    NA  0       <NA>      2011-07-17            <NA>
#> 3       NA    0     1 NA       <NA>      2011-01-25      2013-09-04
#> 4        1    0    NA NA 2020-09-26            <NA>            <NA>
#> 5       NA    0     1  1       <NA>            <NA>      2014-07-23
#> 6        0   NA     1 NA       <NA>      2010-12-03            <NA>
```

### Missing data

To get a quick overview of missing data in the dataset we use missR()

``` r
redcap_df %>% 
  missR()
#> Nas detected in the following variables:
#> 
#>          variable NAs
#> 1 metastasis_date 329
#> 2      death_date 322
#> 3 recurrence_date 261
#> 4              ck 174
#> 5        necrosis 169
#> 6            cd10 167
#> 7           sox10 164
```

Missing values can be dropped with drop_na()

``` r
redcap_df %>% 
  drop_na(metastasis_date, necrosis) %>% 
  head
#>   id sex  age      birth   followup date_of_surgery      size type localisation
#> 1  4   2 58.7 31-12-1936 29-06-2024      1995-09-14 11.609164    2            1
#> 2  5   2 29.3 12-01-1961 14-05-2023      1990-04-22  6.980464    0            3
#> 3  9   1 42.6 01-04-1962 25-01-2021      2004-11-09  8.699874    2            4
#> 4 17   1 56.8 27-09-1946 05-09-2022      2003-07-16 47.472564    1            2
#> 5 18   2 32.0 24-05-1972 12-08-2022      2004-05-31 15.683065    0            2
#> 6 23   1 64.5 30-03-1929 21-09-2022      1993-10-10 28.811464    1            5
#>   necrosis cd10 sox10 ck death_date recurrence_date metastasis_date
#> 1        1    1    NA  0 2019-07-27            <NA>      2012-04-04
#> 2        0    0     0  0 2018-04-19            <NA>      2011-01-16
#> 3        0    1     0  0       <NA>            <NA>      2010-08-17
#> 4        1    0     0  0 2017-09-28            <NA>      2012-08-18
#> 5        1    0     1  0       <NA>            <NA>      2014-09-13
#> 6        1   NA     1  0 2018-03-01      2011-01-03      2010-04-11
```

Sometimes we want to remove rows where at least one of the values are
NA. This is done with rowR() which is useful for rowwise operations

``` r
redcap_df %>% 
  rowR(vars = c(cd10, sox10, ck),
       type = "any.na",
       filter = "remove") %>% 
  head
#>   id sex  age      birth   followup date_of_surgery      size type localisation
#> 1  2   2 57.8 18-01-1941 12-08-2024      1998-10-20 48.688670    0            1
#> 2  5   2 29.3 12-01-1961 14-05-2023      1990-04-22  6.980464    0            3
#> 3  8   1 36.2 01-09-1969 14-10-2024      2005-10-30 43.577441    2            5
#> 4  9   1 42.6 01-04-1962 25-01-2021      2004-11-09  8.699874    2            4
#> 5 14   1 39.6 16-08-1950 24-10-2022      1990-03-12 32.026478    1            3
#> 6 17   1 56.8 27-09-1946 05-09-2022      2003-07-16 47.472564    1            2
#>   necrosis cd10 sox10 ck death_date recurrence_date metastasis_date
#> 1        1    0     0  0       <NA>      2014-12-09            <NA>
#> 2        0    0     0  0 2018-04-19            <NA>      2011-01-16
#> 3       NA    1     0  0       <NA>            <NA>            <NA>
#> 4        0    1     0  0       <NA>            <NA>      2010-08-17
#> 5        1    0     0  0 2017-07-18            <NA>            <NA>
#> 6        1    0     0  0 2017-09-28            <NA>      2012-08-18
```

We can also flag the rows into a new variable

``` r
redcap_df %>% 
  rowR(vars = c(cd10, sox10, ck),
       type = "any.na",
       new = "flag") %>% 
  head
#>   id sex  age      birth   followup date_of_surgery      size type localisation
#> 1  1   1 40.9 26-04-1958 23-04-2021      1999-03-29  3.991349    0            2
#> 2  2   2 57.8 18-01-1941 12-08-2024      1998-10-20 48.688670    0            1
#> 3  3   2 32.2 30-07-1963 20-02-2022      1995-10-26 11.245841    0            4
#> 4  4   2 58.7 31-12-1936 29-06-2024      1995-09-14 11.609164    2            1
#> 5  5   2 29.3 12-01-1961 14-05-2023      1990-04-22  6.980464    0            3
#> 6  6   2 18.3 10-12-1975 06-04-2025      1994-03-21 49.029171    2            0
#>   necrosis cd10 sox10 ck death_date recurrence_date metastasis_date flag
#> 1        0   NA     1  0       <NA>            <NA>            <NA>    1
#> 2        1    0     0  0       <NA>      2014-12-09            <NA>    0
#> 3        0    0     1 NA       <NA>            <NA>            <NA>    1
#> 4        1    1    NA  0 2019-07-27            <NA>      2012-04-04    1
#> 5        0    0     0  0 2018-04-19            <NA>      2011-01-16    0
#> 6       NA   NA    NA  0       <NA>            <NA>            <NA>    1
```

### Join multiple data frames

The terminology is that we have a dataset X and a dataset Y.There are
different types of joins, as joins can be side-by-side (such as merging
to datasets based on a key variable) and end-to-end where one dataside
is a continuation of another. In the side-by-side joins we need to
specify a “by/key” variable which specifies how we link the two
datasets. In the end-to-end join there is never a “key” as these are
extensions and should roughly have the same variables.

#### Left join

In a left_join, dataset X is complete and only observations from dataset
Y that are present in X are joined. For all

If we want to join two data frames, we use left_join().

``` r
left_join(redcap_df, analysis_df, by = "id") %>% 
  tail
#>       id sex  age      birth   followup date_of_surgery     size type
#> 1427 498   1 29.2 29-07-1961 25-03-2024      1990-10-28 24.04804    0
#> 1428 498   1 29.2 29-07-1961 25-03-2024      1990-10-28 24.04804    0
#> 1429 499   2 25.0 13-09-1980 25-09-2025      2005-08-26 49.16685    1
#> 1430 499   2 25.0 13-09-1980 25-09-2025      2005-08-26 49.16685    1
#> 1431 499   2 25.0 13-09-1980 25-09-2025      2005-08-26 49.16685    1
#> 1432 500   2 51.4 29-05-1947 24-04-2022      1998-10-24 33.28409    1
#>      localisation necrosis cd10 sox10 ck death_date recurrence_date
#> 1427            2        0    1     1  0       <NA>      2013-11-04
#> 1428            2        0    1     1  0       <NA>      2013-11-04
#> 1429            3       NA    1    NA NA 2016-03-26      2011-07-29
#> 1430            3       NA    1    NA NA 2016-03-26      2011-07-29
#> 1431            3       NA    1    NA NA 2016-03-26      2011-07-29
#> 1432            1        1    1     1 NA 2020-04-18      2015-02-01
#>      metastasis_date       X6       X7         X8          X9         X10 X1 X2
#> 1427            <NA> 61.70529 67.99959 -0.2146209  0.45695559 -0.63244990 T0  1
#> 1428            <NA> 70.45285 63.29462 -0.9465149 -0.03072066 -0.55711509 T0  1
#> 1429      2010-10-20 59.17862 66.46530  1.9006568  0.58998279 -0.79226076 T0  1
#> 1430      2010-10-20 55.40593 54.43776 -0.6452489  1.21994339  0.08777097 T0  1
#> 1431      2010-10-20 49.97634 59.53915 -0.3484089  1.32272643 -0.45083066 T0  2
#> 1432            <NA> 84.66726 70.04153 -1.1777740 -0.79106526  3.03003518 T1  1
#>      X3 X4 X5   ttt event     time2 event2 event3      X6_bin      X7_bin
#> 1427 T1  0  1 142.8     0 142.03070      0      1 59.21-69.28 63.27-78.64
#> 1428 T0  1  0 106.8     1 107.14123      1      3 69.28-111.5 63.27-78.64
#> 1429 T1  0  0  56.4     0  58.98477      0      2 49.26-59.21 63.27-78.64
#> 1430 T1  1  1  98.4     1  99.02183      1      3 49.26-59.21 42.59-56.53
#> 1431 T1  0  1  66.0     0  65.14302      1      2 49.26-59.21 56.53-59.87
#> 1432 T0  0  1  52.8     1  52.60404      1      2 69.28-111.5 63.27-78.64
#>               X8_bin         X9_bin         X10_bin
#> 1427 -0.6914--0.0393 -0.0061-0.6478 -0.7032--0.0023
#> 1428  -4.303--0.6914 -0.648--0.0061 -0.7032--0.0023
#> 1429    0.6773-3.248 -0.0061-0.6478  -3.147--0.7032
#> 1430 -0.6914--0.0393   0.6478-3.377  -0.0023-0.6628
#> 1431 -0.6914--0.0393   0.6478-3.377 -0.7032--0.0023
#> 1432  -4.303--0.6914   -3.45--0.648    0.6628-3.624
```

We see that some of the patients do not have any rows, as the redcap_df
only has ids up to 350.

We can left_join in a pipe. Here the current dataset we are working with
is symbolised by a dot. We can also use other functions with the pipe
operator inside the left_join.

``` r
analysis_df %>% 
  arrange(id) %>% 
  left_join(.,
            redcap_df %>% select(id, contains("date")),
            by = "id") %>% 
  tail
#>            X6       X7          X8         X9        X10 X1 X2 X3 X4 X5   ttt
#> 1995 74.98396 67.07087 -0.04531170 -0.5390216  0.3911101 T1  1 T0  1  0  26.4
#> 1996 38.76922 58.88108  0.21556532  1.2376595 -1.1185086 T0  1 T1  1  1 109.2
#> 1997 46.08183 54.86935 -0.02686586  0.6506245 -0.6798144 T2  1 T2  0  1 121.2
#> 1998 73.64600 58.98176 -1.22363094 -0.8991348  2.5642655 T2  1 T1  1  1  18.0
#> 1999 51.40115 56.47378  0.25786670  0.2150162  1.2424939 T0  1 T1  1  0  96.0
#> 2000 53.86347 59.64135 -4.30278144 -1.3047959  1.0786687 T0  1 T1  0  1  99.6
#>      event     time2 event2 event3  id      X6_bin      X7_bin          X8_bin
#> 1995     1  26.59780      0      3 698 69.28-111.5 63.27-78.64 -0.6914--0.0393
#> 1996     0 108.14881      0      3 698 13.04-49.26 56.53-59.87  -0.0393-0.6773
#> 1997     0 119.91584      0      2 698 13.04-49.26 42.59-56.53  -0.0393-0.6773
#> 1998     1  16.93386      0      1 699 69.28-111.5 56.53-59.87  -4.303--0.6914
#> 1999     0  95.84683      0      1 699 49.26-59.21 42.59-56.53  -0.0393-0.6773
#> 2000     0 101.65500      0      2 700 49.26-59.21 56.53-59.87  -4.303--0.6914
#>              X9_bin         X10_bin date_of_surgery death_date recurrence_date
#> 1995 -0.648--0.0061  -0.0023-0.6628            <NA>       <NA>            <NA>
#> 1996   0.6478-3.377  -3.147--0.7032            <NA>       <NA>            <NA>
#> 1997   0.6478-3.377 -0.7032--0.0023            <NA>       <NA>            <NA>
#> 1998   -3.45--0.648    0.6628-3.624            <NA>       <NA>            <NA>
#> 1999 -0.0061-0.6478    0.6628-3.624            <NA>       <NA>            <NA>
#> 2000   -3.45--0.648    0.6628-3.624            <NA>       <NA>            <NA>
#>      metastasis_date
#> 1995            <NA>
#> 1996            <NA>
#> 1997            <NA>
#> 1998            <NA>
#> 1999            <NA>
#> 2000            <NA>
```

#### Full join

In a full join, all rows are combined

``` r
analysis_df %>% 
  full_join(.,
            redcap_df %>% select(id, contains("date")),
            by = "id") %>% 
  tail
#>      X6 X7 X8 X9 X10   X1   X2   X3   X4   X5 ttt event time2 event2 event3  id
#> 2021 NA NA NA NA  NA <NA> <NA> <NA> <NA> <NA>  NA  <NA>    NA     NA     NA 439
#> 2022 NA NA NA NA  NA <NA> <NA> <NA> <NA> <NA>  NA  <NA>    NA     NA     NA 448
#> 2023 NA NA NA NA  NA <NA> <NA> <NA> <NA> <NA>  NA  <NA>    NA     NA     NA 468
#> 2024 NA NA NA NA  NA <NA> <NA> <NA> <NA> <NA>  NA  <NA>    NA     NA     NA 472
#> 2025 NA NA NA NA  NA <NA> <NA> <NA> <NA> <NA>  NA  <NA>    NA     NA     NA 474
#> 2026 NA NA NA NA  NA <NA> <NA> <NA> <NA> <NA>  NA  <NA>    NA     NA     NA 491
#>      X6_bin X7_bin X8_bin X9_bin X10_bin date_of_surgery death_date
#> 2021   <NA>   <NA>   <NA>   <NA>    <NA>      2003-09-05 2019-05-04
#> 2022   <NA>   <NA>   <NA>   <NA>    <NA>      1997-11-25 2017-10-30
#> 2023   <NA>   <NA>   <NA>   <NA>    <NA>      2004-01-25       <NA>
#> 2024   <NA>   <NA>   <NA>   <NA>    <NA>      2009-02-26 2019-11-16
#> 2025   <NA>   <NA>   <NA>   <NA>    <NA>      2006-01-29 2018-12-26
#> 2026   <NA>   <NA>   <NA>   <NA>    <NA>      2002-12-21       <NA>
#>      recurrence_date metastasis_date
#> 2021      2010-02-03            <NA>
#> 2022      2012-02-26      2014-02-08
#> 2023            <NA>            <NA>
#> 2024            <NA>      2015-03-16
#> 2025            <NA>            <NA>
#> 2026            <NA>            <NA>
```

Here we se missing data for dataset X and complete data for dataset Y as
all rows are kept.

#### Appending rows (bind rows)

To obtain a relevant case we split our dataset into two

``` r
r1 <- redcap_df %>% filter(id < 100)
r2 <- redcap_df %>% filter(id >= 100)

#Binding it back together
bind_rows(r1, r2) %>% head
#>   id sex  age      birth   followup date_of_surgery      size type localisation
#> 1  1   1 40.9 26-04-1958 23-04-2021      1999-03-29  3.991349    0            2
#> 2  2   2 57.8 18-01-1941 12-08-2024      1998-10-20 48.688670    0            1
#> 3  3   2 32.2 30-07-1963 20-02-2022      1995-10-26 11.245841    0            4
#> 4  4   2 58.7 31-12-1936 29-06-2024      1995-09-14 11.609164    2            1
#> 5  5   2 29.3 12-01-1961 14-05-2023      1990-04-22  6.980464    0            3
#> 6  6   2 18.3 10-12-1975 06-04-2025      1994-03-21 49.029171    2            0
#>   necrosis cd10 sox10 ck death_date recurrence_date metastasis_date
#> 1        0   NA     1  0       <NA>            <NA>            <NA>
#> 2        1    0     0  0       <NA>      2014-12-09            <NA>
#> 3        0    0     1 NA       <NA>            <NA>            <NA>
#> 4        1    1    NA  0 2019-07-27            <NA>      2012-04-04
#> 5        0    0     0  0 2018-04-19            <NA>      2011-01-16
#> 6       NA   NA    NA  0       <NA>            <NA>            <NA>
```
