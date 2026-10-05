# Directly standardized incidence rates using the WHO standard population

Directly standardized incidence rates inspired by the dsr package by
Matt Kumar. The incidence rates are standardized on 5-year age intervals
and sex for each year in the period 1990-2025 based on population tables
of the Danish population. Lastly the incidence rates are weighted based
on the WHO world population.

## Usage

``` r
incidencR(
  data,
  group,
  strata = list(c("year")),
  unit = 1e+05,
  reference = "full",
  index,
  age = age,
  sex = sex,
  dt = F
)
```

## Arguments

- data:

  data set containing age, sex and index date and group

- group:

  optional if incidence rates should be provided per group

- strata:

  list of vectors for which strata the incidence rates should be
  reported (e.g. per age-group and sex)

- unit:

  the unit of the incidence rate. Default is 100.000 person years

- reference:

  whether the reference population should include all possible
  age-sex-groups ("full" (default)) or only the age-groups present in
  data ("partial")

- index:

  variable name of the index data

- age:

  name of the age variable

- sex:

  name of the sex variable

## Value

a standardized incidence rate of the overall population and, if
specified in the strata-argument, stratified incidence rates

## Examples

``` r
(rates <-
   incidencR(redcap_df %>%
               recodR(list("sex" = list("Female" = 1,
                                        "Male" = 2))),
             index = date_of_surgery,
             group = type,
             unit = 100000,
             strata = list(c("year"),
                           c("age", "sex"),
                           c("year", "type"),
                           c("year", "type", "age"),
                           c("type", "age"),
                           c("year", "age", "sex", "type"))))
#> Error in map2(.x, vec_index(.x), .f, ...): ℹ In index: 3.
#> Caused by error in `map2()`:
#> ℹ In index: 1.
#> ℹ With name: crude.
#> Caused by error:
#> ! The `by` argument must be either:
#>   
#>   1. Character vector in which each element is part of: sex, age_group,
#>   year, type, count, total, who, euro
#>   
#>   2. A data frame with a `by` column of labels, and in which all other
#>   columns are elements of: sex, age_group, year, type, count, total, who,
#>   euro
#>   
#>   It can sometimes be useful to supply a data frame explicitly to the
#>   `newdata` argument in order to be able to group by different columns.


ggplot(rates$year_type, aes(x=year, y=weighted_rate, color = type, fill = type)) +
  geom_point() +
  geom_line() +
  #geom_ribbon(aes(ymin = weighted_lower, ymax = weighted_upper), alpha = 0.2, color = NA) +
  geom_smooth(se=F) +
  theme_classic()
#> Error: object 'rates' not found

```
