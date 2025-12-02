#' Directly standardized incidence rates using the WHO standard population
#'
#' @description
#' Directly standardized incidence rates inspired by the dsr package. The incidence rates are standardized on 5-year age intervals and sex for each year in the period 1990-2025 based on population tables of the Danish population. Lastly the incidence rates are weighted based on the WHO world population.
#'
#'
#' @param data data set containing age, sex and index date and group
#' @param group optional if incidence rates should be provided per group
#' @param strata list of vectors for which strata the incidence rates should be reported (e.g. per age-group and sex)
#' @param pyears the unit of the incidence rate. Default is per million person years
#' @param index variable name of the index data
#' @param age name of the age variable
#' @param sex name of the sex variable
#'
#' @returns a standardized incidence rate of the overall population and, if specified in the strata-argument, stratified incidence rates
#' @export
#'

# df <- redcap_df %>%
#   recodR(list("sex" = list("f" = 1,
#                            "m" = 2))) %>%
#   factR(vars=type)
#
# res <- ratR(df,
#             index = date_of_surgery,
#             strata = list(c("year")))
#
# res <- ratR(df,
#      group = type,
#      index = date_of_surgery,
#      strata = list(c("year", "age"),
#                    c("year", "age", "sex"),
#                    c("type", "year"),
#                    c("year")))

ratR <- function(data, group, strata = list(c("year")), pyears = 100000, index, age = age, sex = sex) {

  index_c <- data %>% select({{index}}) %>% names
  age_c <- data %>% select({{age}}) %>% names
  sex_c <- data %>% select({{sex}}) %>% names

  if(!missing(group)) {

    group_c <- data %>% select({{group}}) %>% names

  } else {

    group_c <- "grp"
    data <- data %>%
      mutate(grp = 1)

  }


  aggregate_df <-
    data %>%
    mutate(year = str_extract(!!sym(index_c), "\\d{4}")) %>%
    cutR(age_c,
         c(seq(0,85,5), 150),
         name.list = list("age" = "age_group")) %>%
    mutate(age_group = ifelse(age_group == "85-150", "85+", as.character(age_group))) %>%
    select(year, age_group, matches(paste0("\\b", c(group_c, sex_c), "\\b", collapse="|"))) %>%
    group_by(!!sym(group_c), year, age_group, sex) %>%
    summarise(count = n()) %>%
    left_join(., population_denmark, by = c("year", "age_group", "sex")) %>%
    rename(fu = population) %>%
    left_join(., population_who, by = c("age_group", "sex")) %>%
    group_by(!!sym(group_c), year, age_group, sex) %>%
    suppressWarnings(summarise(count = sum(count),
                               fu = mean(fu),
                               population = mean(population))) %>%
    ungroup() %>%
    mutate(year = as.numeric(year)) %>%
    rename(age = age_group)

  get_rates <- function(frame) {
    frame %>% mutate(count=sum(count),
                     pyears=sum(fu),
                     #WHO standard is 100000 in total
                     wts=population/1000000,
                     rate=sum(wts*(count/pyears)),
                     var=sum(as.numeric((wts^2)*(count/(pyears)^2))))
  }

  get_ci <- function(frame) {
    frame %>%
      mutate(lower=mp*(qgamma((1-0.95)/2, shape=rate^2/var)/(rate/var)),
             upper=mp*qgamma(1-((1-0.95)/2), shape=1+(rate^2/var))/(rate/var),
             rate=mp*rate) %>%
      select(!!sym(group_c), year, age, sex, count, fu, rate, lower, upper)
  }

  out.list <- list()

  #Overall
  out.list[["overall"]] <-
    aggregate_df %>%
    get_rates() %>%
    slice(1) %>%
    get_ci()

  if(!missing(group)) {

    out.list[[group_c]] <-
      aggregate_df %>%
      group_by(!!sym(group_c)) %>%
      get_rates() %>%
      distinct(!!sym(group_c), .keep_all = T) %>%
      get_ci() %>%
      select(-age, -sex, -year)


  }



  #Strata
  for(i in strata) {

    out.list[[paste0(i, collapse="_")]] <-
      aggregate_df %>%
      group_by(!!!syms(i)) %>%
      get_rates() %>%
      distinct(!!!syms(i), .keep_all = T) %>%
      get_ci() %>%
      select(matches(paste0(i, collapse="|")), count, fu, rate, lower, upper) %>%
      ungroup()

  }

  out.list

}
