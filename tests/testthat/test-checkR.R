removR <- function(script) {
  script <- str_replace_all(script, "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}", "<TIMESTAMP>")
  script <- str_replace_all(script, "\\d+(\\.\\d+)?\\s*(secs?|mins?|ms)", "<RUNTIME>")
  script
}

n=200
set.seed(1)
df <- data.frame(id=seq(1:n),
                 group=sample(c("pre", "sub"), n, replace=T),
                 sex=factor(sample(c("M","F"), n, replace=T)),
                 age_group=sample(c("<50",">50"),n,replace=T),
                 chemo = sample(c("yes","no"), n, replace=T),
                 age = sample(c(seq(50,60), 50), n, replace=TRUE),
                 hospital = sample(c("rh","herlev","roskilde"), n, replace=T)) %>%
  mutate(hospital = ifelse(group %in% "sub", "roskilde", hospital),
         chemo = ifelse(group %in% "pre", "yes", chemo),
         age_group = ifelse(group %in% "sub", "<50", age_group),
         hospital = as.factor(hospital))

#add random NA
df <- apply (df, 2, function(x) {x[sample( c(1:n), floor(n/10))] <- NA; x} ) %>%
  as_tibble()


# test_that("checkR, t1", {
#   expect_snapshot(checkR(df,
#                          treatment=group,
#                          vars=sex,
#                          levels=1),
#                   transform = removR)
# })
#
#
#
# test_that("checkR, t2", {
#   expect_snapshot(checkR(df, group, vars=c(sex, hospital), levels=2),
#                   transform = removR)
# })

