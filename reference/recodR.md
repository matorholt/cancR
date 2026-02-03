# Recode multiple variables

Recode multiple variables

## Usage

``` r
recodR(data, namelist, match = "exact", replace = F)
```

## Arguments

- data:

  dataframe

- namelist:

  list og variables with named vector(s) containing diagnosis codes
  (e.g. list(var1 = list(name = diagnosis codes)))

- match:

  Whether the provided diagnosis codes should be matched exactly, start
  with/end with, be bounded by or contain (default)

- replace:

  whether multiple matches should be replaced directly (such as 1,4 to
  head, arm)

## Value

the input data frame with recoded variables

## Examples

``` r
set.seed(1)
df <-
data.frame(diag = sample(c("DX123", "DC123", "DC234", "DG123", "DG234"), 20, replace=TRUE),
           type = sample(c("DY", "DY234", "DY123"), 20, replace=TRUE),
           type2 = sample(c("DC123", "DC234", "DG123"), 20, replace=TRUE),
           split = c("1,11", "2,10", "2,4", "2,15"))

            df %>%
  recodR(list("diag" = list("KOL" = "DX123",
                            "Astma" = c("DC123", "DC2"),
                            "AMI" = c("DG123", "DG234"))),
         match = "exact")
#>     diag  type type2 split
#> 1    KOL    DY DC234  1,11
#> 2    AMI DY234 DC234  2,10
#> 3    KOL DY234 DG123   2,4
#> 4  Astma DY234 DG123  2,15
#> 5    AMI    DY DC234  1,11
#> 6  DC234 DY123 DC234  2,10
#> 7  Astma    DY DC234   2,4
#> 8  DC234 DY123 DC234  2,15
#> 9  DC234 DY234 DC123  1,11
#> 10   KOL DY234 DC234  2,10
#> 11   AMI DY234 DC234   2,4
#> 12   AMI DY234 DC234  2,15
#> 13 Astma DY123 DC234  1,11
#> 14 Astma DY234 DC123  2,10
#> 15   KOL    DY DG123   2,4
#> 16   AMI DY123 DG123  2,15
#> 17   AMI DY234 DC234  1,11
#> 18   KOL    DY DG123  2,10
#> 19   KOL    DY DG123   2,4
#> 20   AMI DY123 DC234  2,15
df %>%
  factR(c(diag, type, type2)) %>%
  recodR(list("diag" = list("KOL" = "DX123",
                            "Astma" = c("DC123", "DC234"),
                            "AMI" = list("DG123", "DG234"))),
         match = "exact")
#>     diag  type type2 split
#> 1    KOL    DY DC234  1,11
#> 2    AMI DY234 DC234  2,10
#> 3    KOL DY234 DG123   2,4
#> 4  Astma DY234 DG123  2,15
#> 5    AMI    DY DC234  1,11
#> 6  Astma DY123 DC234  2,10
#> 7  Astma    DY DC234   2,4
#> 8  Astma DY123 DC234  2,15
#> 9  Astma DY234 DC123  1,11
#> 10   KOL DY234 DC234  2,10
#> 11   AMI DY234 DC234   2,4
#> 12   AMI DY234 DC234  2,15
#> 13 Astma DY123 DC234  1,11
#> 14 Astma DY234 DC123  2,10
#> 15   KOL    DY DG123   2,4
#> 16   AMI DY123 DG123  2,15
#> 17   AMI DY234 DC234  1,11
#> 18   KOL    DY DG123  2,10
#> 19   KOL    DY DG123   2,4
#> 20   AMI DY123 DC234  2,15

df %>%
  recodR(list("split" = list("one" = "1",
                             "four" = "4",
                             "ten" = "10",
                             "fifteen" = "15")),
         match = "boundary")
#>     diag  type type2   split
#> 1  DX123    DY DC234     one
#> 2  DG123 DY234 DC234     ten
#> 3  DX123 DY234 DG123    four
#> 4  DC123 DY234 DG123 fifteen
#> 5  DG234    DY DC234     one
#> 6  DC234 DY123 DC234     ten
#> 7  DC123    DY DC234    four
#> 8  DC234 DY123 DC234 fifteen
#> 9  DC234 DY234 DC123     one
#> 10 DX123 DY234 DC234     ten
#> 11 DG234 DY234 DC234    four
#> 12 DG234 DY234 DC234 fifteen
#> 13 DC123 DY123 DC234     one
#> 14 DC123 DY234 DC123     ten
#> 15 DX123    DY DG123    four
#> 16 DG234 DY123 DG123 fifteen
#> 17 DG234 DY234 DC234     one
#> 18 DX123    DY DG123     ten
#> 19 DX123    DY DG123    four
#> 20 DG234 DY123 DC234 fifteen

df %>%
  recodR(list("split" = list("one" = "1",
                             "two" = "2",
                            "eleven" = "11",
                             "four" = "4",
                             "ten" = "10",
                             "fifteen" = "15")),
         match = "boundary",
         replace=T)
#>     diag  type type2       split
#> 1  DX123    DY DC234  one,eleven
#> 2  DG123 DY234 DC234     two,ten
#> 3  DX123 DY234 DG123    two,four
#> 4  DC123 DY234 DG123 two,fifteen
#> 5  DG234    DY DC234  one,eleven
#> 6  DC234 DY123 DC234     two,ten
#> 7  DC123    DY DC234    two,four
#> 8  DC234 DY123 DC234 two,fifteen
#> 9  DC234 DY234 DC123  one,eleven
#> 10 DX123 DY234 DC234     two,ten
#> 11 DG234 DY234 DC234    two,four
#> 12 DG234 DY234 DC234 two,fifteen
#> 13 DC123 DY123 DC234  one,eleven
#> 14 DC123 DY234 DC123     two,ten
#> 15 DX123    DY DG123    two,four
#> 16 DG234 DY123 DG123 two,fifteen
#> 17 DG234 DY234 DC234  one,eleven
#> 18 DX123    DY DG123     two,ten
#> 19 DX123    DY DG123    two,four
#> 20 DG234 DY123 DC234 two,fifteen
```
