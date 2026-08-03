# removR <- function(script) {
#   script <- str_replace_all(script, "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}", "<TIMESTAMP>")
#   script <- str_replace_all(script, "\\d+(\\.\\d+)?\\s*(secs?|mins?|ms)", "<RUNTIME>")
#   script
# }
#
# set.seed(1)
#
# n=1000
#
# reglist <- list(lpr = simulatR("lpr", n=n,lpr.diag.count = 10),
#                 opr = simulatR("opr", n=n, opr.diag.count = 10),
#                 lmdb = simulatR("lmdb", n = n),
#                 pop = simulatR("pop", n=3900),
#                 pato = simulatR("pato", n=n),
#                 immune = simulatR("immune", n=n),
#                 sc = data.frame(pnr = sample(seq(1,n*10), n*10*0.1, replace=F),
#                                 sc_date = sample(c(sample(seq(as.Date('1990/01/01'), as.Date('2020/01/01'), by="day"))), n*10*0.1, replace=TRUE),
#                                 meta_date = sample(c(sample(seq(as.Date('1990/01/01'), as.Date('2020/01/01'), by="day"))), n*10*0.1, replace=TRUE),
#                                 pato_supp = sample(c(sample(seq(as.Date('1990/01/01'), as.Date('2020/01/01'), by="day"))), n*10*0.1, replace=TRUE)))
#
# join_df <- joinR(reglist$pop, indices, reglist$sc,
#                  by = "pnr")
