# tablR, t1

    Code
      tablR(population_denmark, group = sex, vars = c(age_group, population))
    Condition
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(group_c)
      
        # Now:
        data %>% select(all_of(group_c))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(num_c)
      
        # Now:
        data %>% select(all_of(num_c))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(v)
      
        # Now:
        data %>% select(all_of(v))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(num_c)
      
        # Now:
        data %>% select(all_of(num_c))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    Output
                                                M (N=648)
      1  Age Group                                       
      2     0-5                       36 (5.6%)          
      3     10-15                     36 (5.6%)          
      4     15-20                     36 (5.6%)          
      5     20-25                     36 (5.6%)          
      6     25-30                     36 (5.6%)          
      7     30-35                     36 (5.6%)          
      8     35-40                     36 (5.6%)          
      9     40-45                     36 (5.6%)          
      10    45-50                     36 (5.6%)          
      11    5-10                      36 (5.6%)          
      12    50-55                     36 (5.6%)          
      13    55-60                     36 (5.6%)          
      14    60-65                     36 (5.6%)          
      15    65-70                     36 (5.6%)          
      16    70-75                     36 (5.6%)          
      17    75-80                     36 (5.6%)          
      18    80-85                     36 (5.6%)          
      19    85+                       36 (5.6%)          
      20 Population                                      
      21    Median (Q1, Q3) 169647.0 (129036.5, 189037.0)
      22    Range                23026.0 - 220477.0      
                             F (N=648)
      1                               
      2            36 (5.6%)          
      3            36 (5.6%)          
      4            36 (5.6%)          
      5            36 (5.6%)          
      6            36 (5.6%)          
      7            36 (5.6%)          
      8            36 (5.6%)          
      9            36 (5.6%)          
      10           36 (5.6%)          
      11           36 (5.6%)          
      12           36 (5.6%)          
      13           36 (5.6%)          
      14           36 (5.6%)          
      15           36 (5.6%)          
      16           36 (5.6%)          
      17           36 (5.6%)          
      18           36 (5.6%)          
      19           36 (5.6%)          
      20                              
      21 165007.5 (132838.0, 183598.5)
      22      54832.0 - 211165.0      

# tablR, t2

    Code
      redcap_df %>% mutate(margins = sample(c("0", "1"), nrow(redcap_df), replace = TRUE),
      w = runif(nrow(redcap_df), 1, 5)) %>% factR(c(type, sex, localisation, cd10,
        sox10, ck, margins, necrosis)) %>% tablR(group = type, numeric = c("meansd",
        "range"), vars = c(age, sex, localisation, cd10, sox10, ck, necrosis, margins),
      labs.groups = list(type = list(Benign = "0", `In situ` = "1", Malignant = "2")),
      reverse = T, labs.headings = list(`Age at Debut` = "age", gender = "sex",
        `Cluster of diff 10` = "cd10", SOX10 = "sox10"), labs.subheadings = list(sex = list(
        Female = "2", Male = "1"), localisation = list(Neck = "0", Head = "1", Trunk = "2",
        `Upper Extremity` = "3", `Lower Extremity` = "4", Unspecified = "5")),
      reference = list(sex = c("male")), simplify = list(Immunohistochemistry = c(
        "cd10", "sox10", "ck"), "necrosis", "margins"), print = F, weights = w)
    Condition
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(group_c)
      
        # Now:
        data %>% select(all_of(group_c))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(num_c)
      
        # Now:
        data %>% select(all_of(num_c))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(v)
      
        # Now:
        data %>% select(all_of(v))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(num_c)
      
        # Now:
        data %>% select(all_of(num_c))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(v)
      
        # Now:
        data %>% select(all_of(v))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(num_c)
      
        # Now:
        data %>% select(all_of(num_c))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    Output
                               Malignant (N=417) In situ (N=776) Benign (N=334)
      1  Age at Debut                                                          
      2     Mean (SD)                49.7 (18.4)     49.7 (18.4)    49.4 (19.5)
      3     Range                    11.2 - 86.3     10.8 - 88.8    12.0 - 84.6
      4  gender                                                                
      5     Male                      177 (42%)       385 (50%)      199 (60%) 
      6     Female                    240 (58%)       391 (50%)      135 (40%) 
      7  Localisation                                                          
      8     Trunk                     144 (35%)       240 (31%)      146 (44%) 
      9     Upper Extremity           105 (25%)       288 (37%)      94 (28%)  
      10    Lower Extremity           83 (20%)        138 (18%)      49 (14%)  
      11    Head                      76 (18%)        89 (12%)       26 (7.8%) 
      12    Neck                      3 (0.8%)        17 (2.2%)      19 (5.7%) 
      13    Unspecified               5 (1.2%)        5 (0.7%)       1 (0.4%)  
      14 Immunohistochemistry                                                  
      15    Cluster of diff 10        126 (46%)       301 (58%)      84 (36%)  
      16    SOX10                     126 (51%)       310 (54%)      118 (55%) 
      17    Ck                        145 (53%)       257 (47%)      123 (50%) 
      18 Necrosis                     129 (47%)       234 (46%)      110 (55%) 
      20 Margins                      210 (50%)       348 (45%)      158 (47%) 

