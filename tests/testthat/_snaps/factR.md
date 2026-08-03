# factR, already a factor

    Code
      df %>% factR(comorb) %>% factR(num.vars = comorb)
    Output
         v1 v2 v3    vnum comorb
      1   a  a  a   cci_0      0
      2   d  d  c   cci_0      0
      3   a  e  c 100-110      0
      4   b  a  c   cci_0      0
      5   e  a  c 100-110      1
      6   c  d  d   50-60      0
      7   b  e  a     <40      1
      8   c  e  a   10-20      0
      9   c  d  d     <40      0
      10  a  e  b   cci_0      0
      11  e  d  a     <40      0
      12  e  d  b 100-110      1
      13  b  a  c    >110      1
      14  b  e  d     <40      0
      15  a  e  a   10-20      0
      16  e  a  c 100-110      0
      17  e  a  e    >110      0
      18  a  c  c 100-110      1
      19  a  b  d     <40      1
      20  e  b  b     <40      1
      21  e  c  a    >110      0
      22  b  b  d    >110      0
      23  b  d  a     <40      1
      24  a  c  d    >110      1
      25  d  e  b 100-110      1
      26  a  b  e   50-60      1
      27  d  b  b   cci_0      1
      28  c  a  b   50-60      0
      29  b  c  b     <40      1
      30  b  c  c    >110      1
      31  d  b  a    >110      0
      32  d  b  b   cci_0      0
      33  d  e  c   cci_0      1
      34  b  b  c   50-60      0
      35  d  e  c   cci_0      0
      36  a  d  c   cci_0      0
      37  a  e  e    >110      0
      38  d  d  c   50-60      1
      39  a  a  a   cci_0      1
      40  b  c  b   cci_0      1
      41  c  b  d 100-110      0
      42  b  c  b   50-60      1
      43  b  c  b    >110      0
      44  e  a  d    >110      1
      45  b  e  e 100-110      0
      46  a  d  a 100-110      0
      47  c  d  e     <40      0
      48  c  a  e   50-60      0
      49  d  e  e 100-110      1
      50  c  e  d     <40      0

# factR, setting reference levels

    Code
      df %>% factR(vars = c(v1, v2, v3), reference = list(v1 = "c")) %>% str
    Output
      'data.frame':	50 obs. of  5 variables:
       $ v1    : Factor w/ 5 levels "c","b","a","d",..: 3 4 3 2 5 1 2 1 1 3 ...
       $ v2    : Factor w/ 5 levels "e","a","d","b",..: 2 3 1 2 2 3 1 1 3 1 ...
       $ v3    : Factor w/ 5 levels "c","b","a","d",..: 3 1 1 1 1 4 3 3 4 2 ...
       $ vnum  : chr  "cci_0" "cci_0" "100-110" "cci_0" ...
       $ comorb: int  0 0 0 0 1 0 1 0 0 0 ...

# factR, change labels without changing levels

    Code
      df %>% factR(c(v1, v2, v3), labels = list(v3 = list(alpha = "a")), lab_to_lev = F) %>%
        str
    Output
      'data.frame':	50 obs. of  5 variables:
       $ v1    : Factor w/ 5 levels "b","a","d","c",..: 2 3 2 1 5 4 1 4 4 2 ...
       $ v2    : Factor w/ 5 levels "e","a","d","b",..: 2 3 1 2 2 3 1 1 3 1 ...
       $ v3    : Factor w/ 5 levels "c","b","alpha",..: 3 1 1 1 1 4 3 3 4 2 ...
       $ vnum  : chr  "cci_0" "cci_0" "100-110" "cci_0" ...
       $ comorb: int  0 0 0 0 1 0 1 0 0 0 ...

# factR, Sort pseudo numeric character variable

    Code
      df %>% factR(num.vars = vnum, labels = list(vnum = c(test = "cci_0")))
    Output
         v1 v2 v3    vnum comorb
      1   a  a  a    test      0
      2   d  d  c    test      0
      3   a  e  c 100-110      0
      4   b  a  c    test      0
      5   e  a  c 100-110      1
      6   c  d  d   50-60      0
      7   b  e  a     <40      1
      8   c  e  a   10-20      0
      9   c  d  d     <40      0
      10  a  e  b    test      0
      11  e  d  a     <40      0
      12  e  d  b 100-110      1
      13  b  a  c    >110      1
      14  b  e  d     <40      0
      15  a  e  a   10-20      0
      16  e  a  c 100-110      0
      17  e  a  e    >110      0
      18  a  c  c 100-110      1
      19  a  b  d     <40      1
      20  e  b  b     <40      1
      21  e  c  a    >110      0
      22  b  b  d    >110      0
      23  b  d  a     <40      1
      24  a  c  d    >110      1
      25  d  e  b 100-110      1
      26  a  b  e   50-60      1
      27  d  b  b    test      1
      28  c  a  b   50-60      0
      29  b  c  b     <40      1
      30  b  c  c    >110      1
      31  d  b  a    >110      0
      32  d  b  b    test      0
      33  d  e  c    test      1
      34  b  b  c   50-60      0
      35  d  e  c    test      0
      36  a  d  c    test      0
      37  a  e  e    >110      0
      38  d  d  c   50-60      1
      39  a  a  a    test      1
      40  b  c  b    test      1
      41  c  b  d 100-110      0
      42  b  c  b   50-60      1
      43  b  c  b    >110      0
      44  e  a  d    >110      1
      45  b  e  e 100-110      0
      46  a  d  a 100-110      0
      47  c  d  e     <40      0
      48  c  a  e   50-60      0
      49  d  e  e 100-110      1
      50  c  e  d     <40      0

