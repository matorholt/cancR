n <- 2000
set.seed(1)

analysis_df <-
  data.frame(id = seq_len(n)) %>%
  mutate(g4 = sample(paste0("T",0:3), size = n, prob = c(0.4, 0.3,0.25,0.05), replace=TRUE),
         g3 = ifelse(g4 %in% c("T2", "T3"), "T2", g4),
         g2 = ifelse(g3 %in% c("T1", "T2"), "T1", g3),
         probs = case_when(g4 == "T3" ~ 0.5,
                           g4 == "T2" ~ 0.25,
                           g4 == "T1" ~ 0.1,
                           g4 == "T0" ~ 0.05),
         event = rbinom(n(), size = 1, prob = probs),
         event2 = ifelse(event == 0, ifelse(rbinom(n(), size = 1, prob = 0.5) == 1, 2, 0), event),
         event3 = ifelse(event2 == 0, ifelse(rbinom(n(), size = 1, prob = 0.2) == 1, 3, 0), event2),
         surv = case_when(g4 == "T3" ~ 36,
                           g4 == "T2" ~ 48,
                           g4 == "T1" ~ 60,
                           g4 == "T0" ~ 120),
         t_event = round(rexp(n(), rate = 1 / surv),3),
         x1 = sample(0:1, size = n(), replace = TRUE),
         x2 = sample(letters[1:4], size = n(), replace = TRUE),
         x3 = sample(c("yes", "no"), size = n(), replace=TRUE),
         x4 = ceiling(runif(n(), 0,100)),
         x5 = round(rnorm(n(), 50, 10),1),
         x6 = rexp(n(),   rate = 1 / 5)) %>%
  factR(c(g2, g3, g4, x1:x3)) %>%
  select(-surv, -probs)

usethis::use_data(analysis_df, overwrite = TRUE)
