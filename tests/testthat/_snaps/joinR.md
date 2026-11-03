# basic joins

    Code
      joinR(x, y, by = id, type = left)
    Output
      Key: <id>
            id  grp.x   name score  grp.y   city active
         <int> <char> <char> <num> <char> <char> <lgcl>
      1:     1     g1      a    10   <NA>   <NA>     NA
      2:     2     g1      b    NA     g1      X   TRUE
      3:     3     g1      c    30     g2      Y  FALSE
      4:     3     g2      d    NA     g2      Y  FALSE

---

    Code
      joinR(x, y, by = c(id, grp), type = left)
    Output
      Key: <id, grp>
            id    grp   name score   city active
         <int> <char> <char> <num> <char> <lgcl>
      1:     1     g1      a    10   <NA>     NA
      2:     2     g1      b    NA      X   TRUE
      3:     3     g1      c    30   <NA>     NA
      4:     3     g2      d    NA      Y  FALSE

---

    Code
      joinR(x, y, by = id, type = full)
    Output
      Key: <id>
            id  grp.x   name score  grp.y   city active
         <int> <char> <char> <num> <char> <char> <lgcl>
      1:     1     g1      a    10   <NA>   <NA>     NA
      2:     2     g1      b    NA     g1      X   TRUE
      3:     3     g1      c    30     g2      Y  FALSE
      4:     3     g2      d    NA     g2      Y  FALSE
      5:     4   <NA>   <NA>    NA     g1      Z   TRUE

---

    Code
      joinR(x, y, by = c(id, grp), type = inner)
    Output
      Key: <id, grp>
            id    grp   name score   city active
         <int> <char> <char> <num> <char> <lgcl>
      1:     2     g1      b    NA      X   TRUE
      2:     3     g2      d    NA      Y  FALSE

# filter joins

    Code
      joinR(x, y, by = id, type = semi)
    Output
            id    grp   name score
         <int> <char> <char> <num>
      1:     2     g1      b    NA
      2:     3     g1      c    30
      3:     3     g2      d    NA

---

    Code
      joinR(x, y, by = c(id, grp), type = anti)
    Output
            id    grp   name score
         <int> <char> <char> <num>
      1:     1     g1      a    10
      2:     3     g1      c    30

# insert and update joins on id only

    Code
      joinR(x, new_rows, by = id, type = insert)
    Output
            id    grp   name score
         <int> <char> <char> <num>
      1:     1     g1      a    10
      2:     2     g1      b    20
      3:     3     g1      c    30
      4:     3     g2      d    35

---

    Code
      joinR(x, new_rows, by = id, type = update)
    Output
            id    grp   name score
         <int> <char> <char> <num>
      1:     1     g1      a    10
      2:     2     g1     b2    20
      3:     3     g1     c2    35
      4:     3     g2     c2    35

# update joins only the columns in `vars`

    Code
      joinR(x, new_rows, by = id, type = update, vars = score)
    Output
            id    grp   name score
         <int> <char> <char> <num>
      1:     1     g1      a    10
      2:     2     g1      b    20
      3:     3     g1      c    35
      4:     3     g2      d    35

# insert and update joins on id and grp

    Code
      joinR(x, new_rows_grp, by = c(id, grp), type = insert)
    Output
            id    grp   name score
         <int> <char> <char> <num>
      1:     1     g1      a    10
      2:     2     g1      b    20
      3:     3     g1      c    30
      4:     3     g2      d    35

---

    Code
      joinR(x, new_rows_grp, by = c(id, grp), type = update)
    Output
            id    grp   name score
         <int> <char> <char> <num>
      1:     1     g1      a    10
      2:     2     g1     b2    20
      3:     3     g1      c    30
      4:     3     g2     c2    35

---

    Code
      joinR(x, new_rows_grp, by = c(id, grp), type = update, vars = score)
    Output
            id    grp   name score
         <int> <char> <char> <num>
      1:     1     g1      a    10
      2:     2     g1      b    20
      3:     3     g1      c    30
      4:     3     g2      d    35

# keys with different names

    Code
      joinR(x, z, by = "id = pnr", type = left)
    Output
      Key: <id>
            id    grp   name score region
         <int> <char> <char> <num> <char>
      1:     1     g1      a    10      N
      2:     2     g1      b    NA   <NA>
      3:     3     g1      c    30      S
      4:     3     g2      d    NA      S

# joining a list of data.tables

    Code
      joinR(x, df_list, by = c("id = id2 = pnr = id"), type = left)
    Condition
      Warning in `merge.data.table()`:
      column names [grp.x, grp.y] are duplicated in the result
    Output
      Key: <id>
            id  grp.x   name score  grp.y  val_a  grp.x  val_b  grp.y  val_c
         <int> <char> <char> <num> <char> <char> <char> <char> <char> <char>
      1:     1     g1      a    10     g1     a1     g1     b1     g1     c1
      2:     2     g1      b    NA     g1     a2     g2     b2   <NA>   <NA>
      3:     3     g1      c    30     g2     a3   <NA>   <NA>     g2     c3
      4:     3     g2      d    NA     g2     a3   <NA>   <NA>     g2     c3

---

    Code
      joinR(x, y, df_list, by = list(c("id", "id", "id2", "pnr", "id"), "grp"), type = "left")
    Output
      Key: <id, grp>
            id    grp   name score   city active  val_a  val_b  val_c
         <int> <char> <char> <num> <char> <lgcl> <char> <char> <char>
      1:     1     g1      a    10   <NA>     NA     a1     b1     c1
      2:     2     g1      b    NA      X   TRUE     a2   <NA>   <NA>
      3:     3     g1      c    30   <NA>     NA   <NA>   <NA>   <NA>
      4:     3     g2      d    NA      Y  FALSE     a3   <NA>     c3

