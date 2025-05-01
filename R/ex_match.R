
no = 10000
cno= 2

set.seed(1)
(pop <- data.frame(pnr = paste("pnr", rnorm(no, 40000,1000)),
                   case = c(rep(1,cno), rep(0,(no-cno))),
                   index = sample(c(sample(seq(as.Date('1990/01/01'), as.Date('2010/01/01'), by="day"))), size = no, replace=TRUE),
                   fu = c(sample(seq(as.Date('2015/01/01'), as.Date('2020/01/01'), by="day"), no, replace=T)),
                    byear = sample(c(seq(1950,1955)), no, replace=T),
                    sex = sample(c("F","M"), no, replace=T),
                   skinc = sample(c(sample(seq(as.Date('1998/01/01'), as.Date('2010/01/01'), by="day"))), size = no, replace=TRUE)) %>%
    mutate(index = if_else(case == 0, as.Date(NA), index)))
set.seed(1)
mp <- 40
(ses <- data.frame(var = sample(c("married",
                                  "education",
                                  "income",
                                  "cci",
                                  "region"),
                                size = no*mp,
                                replace=TRUE),
           date = sample(c(sample(seq(as.Date('1980/01/01'), as.Date('2000/01/01'), by="day"))), size = no*mp, replace=TRUE),
           pnr = sample(pop$pnr, size = no*mp, replace=TRUE)) %>%
  arrange(pnr) %>%
  mutate(val = case_when(var %in% "income" ~ sample(paste("q", seq(1,4), sep=""), no*mp, prob = rep(0.25,4), replace=TRUE),
                         var %in% "education" ~ sample(c("education_low", "education_medium", "education_high"), no*mp, prob = rep(1/3,3), replace=TRUE),
                         var %in% "cci" ~ sample(as.character(seq(1,6)), no*mp, prob = rep(1/6,6), replace=TRUE),
                         var %in% "region" ~ sample(c("the_capital_region_of_denmark",
                                                      "region_zealand",
                                                      "the_north_denmark_region",
                                                      "central_denmark_region",
                                                      "the_region_of_southern_denmark"), no*mp, prob = rep(0.20,5), replace=TRUE),
                         var %in% "married" ~ "married")) %>%
  select(pnr, date, var, val))

ex_match <- function(data, pnr, case, index, date, fu,n_controls, cat, dat, seed=1, cores = 4) {

  cl <- makeCluster(cores)
  clusterEvalQ(cl, {
    library(tidyverse)
  })

  pnr_c <- data %>% select({{pnr}}) %>% names()

  #original
  df <- data
  # #Simplify data
  data <-
  data %>% select({{pnr}}, {{case}}, {{index}}, {{fu}}, {{cat}}) %>%
    mutate(id = row_number()) %>%
  left_join(., ses, by = pnr_c) %>%
    arrange(desc({{case}}), {{pnr}}, date)


  #Cases
  cases <-
  data %>% filter({{case}} == 1) %>%
    mutate(set = id) %>%
    filter(date <= index) %>%
    group_by({{pnr}}, var) %>%
    slice(n()) %>%
    ungroup() %>%
    select(-date) %>%
    pivot_wider(names_from=var, values_from = val)

  controls <-
    data %>% filter({{case}} == 0)

  #Split data in one case + all controls

  control_df <- as.data.frame(rbindlist(parLapply(cl, seq(1, nrow(cases)), function(x) {


    i <- cases[x,] %>% pull({{index}})
    f <- cases[x,] %>% pull({{fu}})
    s <- cases[x,] %>% pull(set)

    bind_rows(cases[x,],
              controls %>%
      filter({{fu}} >= f & date <= i) %>%
      group_by({{pnr}}, var) %>%
      slice(n()) %>%
      ungroup() %>%
      select(-date) %>%
      mutate(set = s) %>%
      pivot_wider(names_from = var, values_from = val)) %>%
      filter(if_all(c({{cat}}, {{dat}}), ~ (. == first(.) | is.na(.) & is.na(first(.))))) %>%
      filter(case == 0) %>%
      mutate(nrow = n())
  }))) %>%
    arrange(nrow, set)

  con_list <- control_df %>%
    select(id, set, pnr, nrow)

  #Choose unique matches
  set.seed(seed)

  #Order with fewest potential matches first
  con_list <- parLapply(cl, unique(con_list$set), function(x) {
    con_list %>% filter(set %in% x) %>% select(-nrow)
  })

  #Sample first element in list
  con_list[[1]] <- list(a<-con_list[[1]] %>% slice_sample(n=pmin(nrow(con_list[[1]]), n_controls)), a$id)

  #Rolling unique sampling
  matches <- as.data.frame(rbindlist(parLapply(cl, accumulate(con_list, function(acc, nxt) {
    id_sample <- nxt %>% filter(!(id %in% acc[[2]])) %>% slice_sample(n=pmin(nrow(nxt), n_controls))
    id_cnt <- c(acc[[2]], id_sample$id)
    list(id_sample, id_cnt)
  }), function(x) {x[[1]]})))

  #list(cases, control_df, matches)
  total <- bind_rows(cases, left_join(matches %>% select(pnr, set), control_df, by = c(pnr_c, "set"))) %>%
  select(-nrow, -id) %>%
   arrange(set) %>%
   group_by(set) %>%
   fill({{index}}, .direction = "updown") %>%
   ungroup()

  stopCluster(cl)

  return(total)

}

start <- Sys.time()

(t <- ex_match(data=pop, pnr=pnr, case=case, index=index, fu=fu, cat = c(sex, byear), dat = c(education, region, income, cci, married), n_controls=1))
Sys.time() - start



