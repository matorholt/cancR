# tumR, t1

    Code
      tumR(pato, tumor = tumor_list, verbose = F, loc.exact = F)
    Message
      
      -- Initializing tumR algorithm: <TIMESTAMP> --
      
    Condition
      Warning in `doTryCatch()`:
      restarting interrupted promise evaluation
      Warning in `t_codes$t.code == str_to_upper(.x)`:
      longer object length is not a multiple of shorter object length
      Warning in `t_codes$t.code == str_to_upper(.x)`:
      longer object length is not a multiple of shorter object length
      Warning in `t_codes$t.code == str_to_upper(.x)`:
      longer object length is not a multiple of shorter object length
      Warning in `t_codes$t.code == str_to_upper(.x)`:
      longer object length is not a multiple of shorter object length
      Warning in `t_codes$t.code == str_to_upper(.x)`:
      longer object length is not a multiple of shorter object length
      Warning in `t_codes$t.code == str_to_upper(.x)`:
      longer object length is not a multiple of shorter object length
      Warning in `doTryCatch()`:
      restarting interrupted promise evaluation
      Warning in `doTryCatch()`:
      restarting interrupted promise evaluation
    Message
      -- Tumor Mapping complete! 
      Total runtime:
      <RUNTIME>
      
    Output
         pnr t_id       date tumor           exact localisation          region
      1    1    1 2012-01-01   pcc            face         face   head and neck
      2    1    2 2013-04-04    mm    external ear          ear   head and neck
      3    1    3 2016-01-01   pcc popliteal fossa         knee lower extremity
      4    2    1 2012-01-01    mm popliteal fossa         knee lower extremity
      5    3    1 2012-01-01   pcc popliteal fossa         knee lower extremity
      6    4    1 2013-04-04    mm    external ear          ear   head and neck
      7    5    1 2001-01-01   lms    external ear          ear   head and neck
      8    6    1 2001-01-01   pcc popliteal fossa         knee lower extremity
      9    7    1 2001-01-01   ups popliteal fossa         knee lower extremity
      10   7    2 2003-02-01  dfsp    external ear          ear   head and neck
      11   7    3 2006-02-01   lms            face         face   head and neck
      12   8    1 2001-01-01   pcc    external ear          ear   head and neck
      13   9    1 2001-01-01   pcc            skin         skin            <NA>
      14  10    1 2001-01-01   pcc    external ear          ear   head and neck
      15  11    1 2001-01-01   pcc    external ear          ear   head and neck
      16  12    1 2001-01-01   bcc            skin         skin            <NA>
      17  12    2 2001-02-01   bcc            skin         skin            <NA>
      18  13    1 2001-01-01   bcc            skin         skin            <NA>
      19  13    2 2003-02-01   bcc            skin         skin            <NA>
      20  15    1 2001-01-01   pcc            skin         skin            <NA>
      21  15    2 2003-02-01   pcc            skin         skin            <NA>
      22  16    1 2001-01-01   pcc    external ear          ear   head and neck
      23  16    2 2003-02-01   pcc           cheek        cheek   head and neck
      24  18    1 2000-01-01   bcc    external ear          ear   head and neck
      25  18    2 2001-01-01   pcc    external ear          ear   head and neck
      26  18    3 2015-01-01   pcc           cheek        cheek   head and neck
      27  19    1 2001-01-01   pcc    external ear          ear   head and neck
      28  19    2 2001-02-01   pcc           cheek        cheek   head and neck
            op_date recurrence_date    distant      local   regional
      1  2012-02-01            <NA> 2012-08-31       <NA>       <NA>
      2  2013-04-04            <NA>       <NA>       <NA>       <NA>
      3  2016-01-01      2017-01-01       <NA>       <NA>       <NA>
      4  2012-01-01            <NA>       <NA>       <NA>       <NA>
      5  2012-01-01            <NA>       <NA>       <NA>       <NA>
      6  2013-04-04            <NA>       <NA>       <NA>       <NA>
      7  2001-03-01      2005-01-01       <NA>       <NA>       <NA>
      8  2001-02-01            <NA>       <NA>       <NA>       <NA>
      9  2001-01-01            <NA>       <NA>       <NA>       <NA>
      10 2003-02-01            <NA>       <NA>       <NA>       <NA>
      11 2006-02-01            <NA>       <NA>       <NA>       <NA>
      12 2001-02-01            <NA>       <NA>       <NA>       <NA>
      13 2001-01-01      2003-02-01       <NA>       <NA>       <NA>
      14 2001-02-01            <NA>       <NA>       <NA>       <NA>
      15 2001-01-01      2003-02-01       <NA>       <NA>       <NA>
      16 2001-01-01            <NA>       <NA>       <NA>       <NA>
      17 2001-02-01            <NA>       <NA>       <NA>       <NA>
      18 2001-01-01            <NA>       <NA>       <NA>       <NA>
      19 2003-02-01            <NA>       <NA>       <NA>       <NA>
      20 2001-01-01            <NA>       <NA>       <NA>       <NA>
      21 2003-02-01            <NA>       <NA>       <NA>       <NA>
      22 2001-01-01      2005-02-01       <NA>       <NA>       <NA>
      23 2003-02-01      2005-02-01       <NA>       <NA>       <NA>
      24 2000-01-01            <NA>       <NA>       <NA>       <NA>
      25 2001-01-01            <NA> 2003-01-01 2004-01-01 2003-02-05
      26 2015-01-01            <NA>       <NA>       <NA>       <NA>
      27 2001-01-01            <NA>       <NA>       <NA>       <NA>
      28 2001-02-01            <NA>       <NA>       <NA> 2001-07-01
                                                                                                                                                                                                                                                                                                   mets_data
      1                                                                                                                                                                                                                                                         15583, 1, lung, trunk, non-skin, NA, distant
      2                                                                                                                                                                                                                                                                                                 NULL
      3                                                                                                                                                                                                                                                                                                 NULL
      4                                                                                                                                                                                                                                                                                                 NULL
      5                                                                                                                                                                                                                                                                                                 NULL
      6                                                                                                                                                                                                                                                                                                 NULL
      7                                                                                                                                                                                                                                                                                                 NULL
      8                                                                                                                                                                                                                                                                                                 NULL
      9                                                                                                                                                                                                                                                                                                 NULL
      10                                                                                                                                                                                                                                                                                                NULL
      11                                                                                                                                                                                                                                                                                                NULL
      12                                                                                                                                                                                                                                                                                                NULL
      13                                                                                                                                                                                                                                                                                                NULL
      14                                                                                                                                                                                                                                                                                                NULL
      15                                                                                                                                                                                                                                                                                                NULL
      16                                                                                                                                                                                                                                                                                                NULL
      17                                                                                                                                                                                                                                                                                                NULL
      18                                                                                                                                                                                                                                                                                                NULL
      19                                                                                                                                                                                                                                                                                                NULL
      20                                                                                                                                                                                                                                                                                                NULL
      21                                                                                                                                                                                                                                                                                                NULL
      22                                                                                                                                                                                                                                                                                                NULL
      23                                                                                                                                                                                                                                                                                                NULL
      24                                                                                                                                                                                                                                                                                                NULL
      25 12053, 12088, 12113, 12418, 12449, 13209, 1, 2, 3, 4, 5, 6, lung, lymph node, spine, cheek, ear, spine, trunk, NA, trunk, head and neck, head and neck, trunk, non-skin, non-skin, non-skin, skin, skin, non-skin, NA, lymph, bone, NA, NA, bone, distant, regional, distant, local, local, distant
      26                                                                                                                                                                                                                                                                                                NULL
      27                                                                                                                                                                                                                                                                                                NULL
      28                                                                                                                                                                                                                                            11504, 1, head, head and neck, non-skin, lymph, regional

