no = 40000
cno= 0.0025*no


set.seed(2)
(c <- data.frame(id = paste("pnr", rnorm(cno, 40000,1000)),
                 case = 1,
                 index = sample(c(sample(seq(as.Date('1990/01/01'), as.Date('2010/01/01'), by="day"))), size = cno, replace=TRUE),
                 follow = c(sample(seq(as.Date('2015/01/01'), as.Date('2020/01/01'), by="day"), cno, replace=T)),
                 birth = sample(c(sample(seq(as.Date('1958/01/01'), as.Date('1961/01/01'), by="day"))), size = cno, replace=TRUE),
                 byear = sample(c(seq(1958,1961)), cno, replace=T),
                 sex = sample(c("f","m"), cno, replace=T),
                 skinc = sample(c(sample(seq(as.Date('2011/01/01'), as.Date('2020/01/01'), by="day")),rep(as.Date(NA),8000)), size = cno, replace=TRUE),
                 imm_sup = sample(c(sample(seq(as.Date('2011/01/01'), as.Date('2020/01/01'), by="day")),rep(as.Date(NA),8000)), size = cno, replace=TRUE),
                 random1 = 1,
                 random2 = 2) %>%
    mutate(across(c(skinc, imm_sup), ~ if_else(. > follow | .< index, follow-100, .))))

(cnt <- data.frame(id = paste("pnr", rnorm(no, 40000,1000)),
                   case = 0,
                   follow = c(sample(seq(as.Date('1980/01/01'), as.Date('2020/01/01'), by="day"), no, replace=T)),
                   birth = sample(c(sample(seq(as.Date('1958/01/01'), as.Date('1961/01/01'), by="day"))), size = cno, replace=TRUE),
                   byear = sample(c(seq(1958,1961)), no, replace=T),
                   sex = sample(c("f","m"), no, replace=T),
                   skinc = sample(c(sample(seq(as.Date('1985/01/01'), as.Date('2000/01/01'), by="day")),rep(as.Date(NA),8000)), size = no, replace=TRUE),
                   imm_sup = sample(c(sample(seq(as.Date('1985/01/01'), as.Date('2000/01/01'), by="day")),rep(as.Date(NA),8000)), size = no, replace=TRUE),
                   random1 = 1,
                   random2 = 2) %>%
    mutate(across(c(skinc, imm_sup), ~ if_else(. > follow, follow-2000, .))))

match_df <- bind_rows(c, cnt)

usethis::use_data(match_df, overwrite = TRUE)
