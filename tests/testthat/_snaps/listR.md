# listR, t1

    Code
      listR(reverse_list, type = "reverse")
    Output
      $a1
      [1] "first"
      
      $b2
      [1] "second"
      

# listR, t2

    Code
      listR(vec, type = "vec2list")
    Output
      $a
      [1] "a"
      
      $b
      [1] "b"
      
      $c
      [1] "c"
      
      $d
      [1] "d"
      
      $e
      [1] "e"
      

# listR, t3

    Code
      listR(peel_list, "peel", layer = 2, collapse = T)
    Output
      $lpr
      $lpr$endo
      [1] "e"
      
      $lpr$immun
      [1] "a, d"
      
      $lpr$other
      [1] "b"
      
      $lpr$test
      [1] "c"
      
      

