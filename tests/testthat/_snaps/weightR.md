# weightR, t1

    Code
      weightR(analysis_df %>% factR(g2), model, treatment = g2, vars = c(X4, X5, X6,
        X7, X11))$table
    Condition
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(i)
      
        # Now:
        data %>% select(all_of(i))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(i)
      
        # Now:
        data %>% select(all_of(i))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    Output
        T0_crude T1_crude T0_weighted T1_weighted standardized.diff.unweighted
      1     0.58     0.63        0.59        0.59                         0.11
      2     0.42     0.37        0.41        0.41                        -0.11
      3     0.52     0.48        0.51        0.51                        -0.08
      4     0.48     0.52        0.49        0.49                         0.08
      5    59.44    59.83       59.51       59.44                         0.03
      6    60.06    59.60       59.97       59.97                        -0.10
      7     0.51     0.50        0.51        0.51                        -0.01
      8     0.49     0.50        0.49        0.49                         0.01
        standardized.diff.weighted var level
      1                      -0.01  X4     0
      2                       0.01  X4     1
      3                       0.00  X5     0
      4                       0.00  X5     1
      5                      -0.01  X6      
      6                       0.00  X7      
      7                      -0.01 X11    no
      8                       0.01 X11   yes

