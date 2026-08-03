# joinR, t1

    Code
      joinR(df_list, by = "id", type = "left", dt = T)
    Output
            id   pnr     x  v1.x  pnr2    x2  v1.y     y  pnr3    x3    v1     z
         <num> <num> <num> <num> <num> <num> <num> <num> <num> <num> <num> <num>
      1:     1     1     1     1    NA    NA    NA    NA    NA    NA    NA    NA
      2:     2     2     1     1     2     1     1     2     2     1     1     3
      3:     3     3     2     2     3     1     1     2    NA    NA    NA    NA
      4:     3     3     2     2     3     2     2     2    NA    NA    NA    NA

# joinR, t2

    Code
      joinR(df_list[[1]], df_list[[3]], by = c("id", "v1"))
    Output
        id v1 pnr x pnr3 x3  z
      1  1  1   1 1   NA NA NA
      2  2  1   2 1    2  1  3
      3  3  2   3 2   NA NA NA

# joinR, t3

    Code
      joinR(df_list, by = c("id", "v1"), type = "left", dt = T)
    Output
            id    v1   pnr     x  pnr2    x2     y  pnr3    x3     z
         <num> <num> <num> <num> <num> <num> <num> <num> <num> <num>
      1:     1     1     1     1    NA    NA    NA    NA    NA    NA
      2:     2     1     2     1     2     1     2     2     1     3
      3:     3     2     3     2     3     2     2    NA    NA    NA

# joinR, t4

    Code
      joinR(df_list, by = list(c("pnr", "pnr2", "pnr3")), type = "full")
    Output
        pnr id.x  x v1.x id.y x2 v1.y  y id x3 v1  z
      1   1    1  1    1   NA NA   NA NA NA NA NA NA
      2   2    2  1    1    2  1    1  2  2  1  1  3
      3   3    3  2    2    3  1    1  2 NA NA NA NA
      4   3    3  2    2    3  2    2  2 NA NA NA NA
      5   4   NA NA   NA   NA NA   NA NA  4  3  3  3
      6   4   NA NA   NA   NA NA   NA NA  4  3  3  3

# joinR, t5

    Code
      joinR(df_list, by = list(c("pnr", "pnr2", "pnr3"), c("x", "x2", "x3")), type = "left",
      dt = T)
    Output
           pnr     x  id.x  v1.x  id.y  v1.y     y    id    v1     z
         <num> <num> <num> <num> <num> <num> <num> <num> <num> <num>
      1:     1     1     1     1    NA    NA    NA    NA    NA    NA
      2:     2     1     2     1     2     1     2     2     1     3
      3:     3     2     3     2     3     2     2    NA    NA    NA

# joinR, t6

    Code
      joinR(df_list, as.data.table(df_2), by = "id")
    Condition
      Warning in `merge.data.table()`:
      column names [v1.x, v1.y] are duplicated in the result
    Output
            id pnr.x   x.x  v1.x  pnr2    x2  v1.y     y  pnr3    x3  v1.x     z
         <num> <num> <num> <num> <num> <num> <num> <num> <num> <num> <num> <num>
      1:     1     1     1     1    NA    NA    NA    NA    NA    NA    NA    NA
      2:     2     2     1     1     2     1     1     2     2     1     1     3
      3:     3     3     2     2     3     1     1     2    NA    NA    NA    NA
      4:     3     3     2     2     3     2     2     2    NA    NA    NA    NA
         pnr.y   x.y  v1.y
         <num> <num> <num>
      1:    11    11    11
      2:    22    11    11
      3:    33    22    12
      4:    33    22    12

