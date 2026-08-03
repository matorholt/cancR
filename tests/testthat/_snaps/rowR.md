# rowR, NA2

    Code
      df %>% rowR(vars = c(y:z_z), type = "any.na", filter = "remove")
    Output
         x y z z_z
      1 NA 1 0  10
      2  2 2 4  10
      3  3 5 7  10

# rowR, leftright

    Code
      df %>% rowR(type = "fill", direction = "leftright")
    Output
         x  y  z z_z
      1  1  1  0  10
      2  2  2  4  10
      3  3  5  7  10
      4  3  3  4   4
      5 NA NA NA  NA

# rowR, paste

    Code
      df %>% rowR(vars = c(x, y), type = "paste", collapse = "|")
    Output
         x  y  z z_z paste
      1 NA  1  0  10     1
      2  2  2  4  10   2|2
      3  3  5  7  10   3|5
      4 NA  3  4  NA     3
      5 NA NA NA  NA      

