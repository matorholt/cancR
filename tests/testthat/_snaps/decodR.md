# decodR, t1

    Code
      decodR(codelist_matching)
    Message
      ! 1 covariates containing case-codes:
      * infection
    Output
      $loadR
      $loadR$regs
      [1] "lpr"  "lmdb" "opr"  "pop"  "sc"   "meta" "dsd"  "pato"
      
      $loadR$pattern.list
      $loadR$pattern.list$lpr
      [1] "DC" "DD" "DE" "DF" "DG" "DA" "DB"
      
      $loadR$pattern.list$lmdb
      [1] "A" "B" "G"
      
      $loadR$pattern.list$opr
      [1] "KA" "KB"
      
      $loadR$pattern.list$pato
      [1] "M(83)"
      
      
      
      $searchR
      $searchR$search.list
      $searchR$search.list$lpr
      $searchR$search.list$lpr$immun_lpr
      [1] "DC" "DD" "DE"
      
      $searchR$search.list$lpr$leukemia
      [1] "DF" "DG"
      
      $searchR$search.list$lpr$case_lpr
      [1] "DA" "DB"
      
      $searchR$search.list$lpr$infection
       [1] "07002" "07003" "99929" "07004" "07005" "07006" "07007" "07008" "07009"
      [10] "07000" "07001" "07002" "07003" "07008" "07009" "07983" "DB16"  "DB17" 
      [19] "DB18"  "DB19"  "DB20"  "DB21"  "DB22"  "DB23"  "DB24" 
      
      
      $searchR$search.list$lmdb
      $searchR$search.list$lmdb$immun_atc
      [1] "A" "B"
      
      $searchR$search.list$lmdb$chemo
      [1] "G"
      
      
      $searchR$search.list$opr
      $searchR$search.list$opr$immun_opr
      [1] "KA" "KB"
      
      
      $searchR$search.list$pato
      $searchR$search.list$pato$case_pato
      [1] "M(83)"
      
      
      
      $searchR$exclusion.list
      $searchR$exclusion.list$lpr
      $searchR$exclusion.list$lpr$infection
      [1] "DA" "DB"
      
      
      
      
      $includR
      $includR$exclusion.ex
      [1] "immun_lpr"   "leukemia"    "immun_atc"   "chemo"       "immun_opr"  
      [6] "immigration"
      
      $includR$remove
      [1] "infection"
      
      $includR$exclusion.out
      [1] "sc_date"
      
      $includR$age.limit
      [1] 18
      
      $includR$period
      [1] "2000-01-01" "2022-12-31"
      
      $includR$look.back
      numeric(0)
      
      
      $matchR
      $matchR$look.back
      numeric(0)
      
      $matchR$td.vars
       [1] "education"  "income"     "degurba"    "martital"   "cancer"    
       [6] "cvd"        "connective" "endo"       "gi"         "hema"      
      [11] "infection"  "lungs"      "neuro"      "psych"      "skin"      
      [16] "urinary"   
      
      $matchR$exclude
      [1] "sc_date"        "exclusion_date"
      
      
      $updatR
      $updatR$vars
      [1] "infection"
      
      

