#' Directly standardized incidence rates using the WHO standard population
#'
#' @description
#' Directly standardized incidence rates inspired by the dsr package by Matt Kumar. The incidence rates are standardized on 5-year age intervals and sex for each year in the period 1990-2025 based on population tables of the Danish population. Lastly the incidence rates are weighted based on the WHO world population.
#'
#'
#' @param data data set containing age, sex and index date and group
#' @param group optional if incidence rates should be provided per group
#' @param strata list of vectors for which strata the incidence rates should be reported (e.g. per age-group and sex)
#' @param pyears the unit of the incidence rate. Default is 100.000 person years
#' @param ci.method the method for derivation of confidence intervals. Default is "normal". If negative CIs are reported, use "lognormal"
#' @param index variable name of the index data
#' @param age name of the age variable
#' @param sex name of the sex variable
#'
#' @returns a standardized incidence rate of the overall population and, if specified in the strata-argument, stratified incidence rates
#' @export
#'

# df <- redcap_df %>%
#   recodR(list("sex" = list("Female" = 1,
#                            "Male" = 2))) %>%
#   factR(vars=type)
#
# (res2 <-
#     incidencR(df,
#               index = date_of_surgery,
#               group = type,
#               ci.method = "lognormal",
#               strata = list(c("year"),
#                             c("age", "sex"),
#                             c("year", "type"),
#                             c("year", "type", "age"),
#                             c("type", "age"),
#                             c("year", "age", "sex"))))

incidencR <- function(data, group, strata = list(c("year")), unit = 100000, ci.method = "normal", index, age = age, sex = sex) {

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
    mutate(year = str_extract(!!sym(index_c), "\\d{4}"),
           !!sym(sex_c) := str_to_lower(str_extract(!!sym(sex_c), "\\w"))) %>%
    cutR(!!sym(age_c),
         c(seq(0,85,5), 150),
         name.list = list("age_group") %>% set_names(age_c)) %>%
    mutate(age_group = ifelse(age_group == "85-150", "85+", as.character(age_group))) %>%
    select(year, age_group, matches(paste0("\\b", c(group_c, sex_c), "\\b", collapse="|"))) %>%
    group_by(!!sym(group_c), year, age_group, sex) %>%
    summarise(count = n(), .groups = "drop") %>%
    left_join(., population_denmark, by = c("year", "age_group", "sex")) %>%
    rename(fu = population) %>%
    left_join(., population_who, by = c("age_group", "sex")) %>%
    group_by(!!sym(group_c), year, age_group, sex) %>%
    ungroup() %>%
    mutate(year = as.numeric(year)) %>%
    rename(age = age_group)



  get_rates <- function(data, strata=NULL) {

    strata_v <- unique(c(strata, c("age","sex")))

    data <- data %>%
      group_by(!!!syms(strata_v)) %>%
      summarise(cases = sum(count),
                pyears = sum(fu),
                population = first(population), .groups="drop") %>%
      #Estimate standardized rates stratified on strata
      group_by(!!!syms(strata)) %>%
      mutate(crude_rate = cases/pyears,
             crude_var = cases / pyears ^2,
             #WHO standard is 100000 in total
             wts=population/sum(population),
             weighted_rate = sum(wts*(crude_rate)),
             weighted_var=sum(as.numeric((wts^2)*crude_var)),
             cases = sum(cases)) %>%
      ungroup() %>%
      distinct(!!!syms(strata), .keep_all = T)

    if(ci.method == "normal") {

      data <- data %>%
      mutate(crude_lower=unit*(crude_rate+qnorm((1-0.95)/2)*sqrt(crude_var)),
             crude_upper=unit*(crude_rate-qnorm((1-0.95)/2)*sqrt(crude_var)),
             weighted_lower=unit*(weighted_rate+qnorm((1-0.95)/2)*sqrt(weighted_var)),
             weighted_upper=unit*(weighted_rate-qnorm((1-0.95)/2)*sqrt(weighted_var)))
    }

    if(ci.method == "lognormal") {

      data <- data %>%
        mutate(crude_lower=unit*exp((log(crude_rate)+qnorm((1-0.95)/2)*sqrt(crude_var)/(crude_rate))),
               crude_upper=unit*exp((log(crude_rate)-qnorm((1-0.95)/2)*sqrt(crude_var)/(crude_rate))),
               weighted_lower=unit*exp((log(weighted_rate)+qnorm((1-0.95)/2)*sqrt(weighted_var)/(weighted_rate))),
               weighted_upper=unit*exp((log(weighted_rate)-qnorm((1-0.95)/2)*sqrt(weighted_var)/(weighted_rate))))

    }

    data %>%
      mutate(crude_rate=unit*crude_rate,
             weighted_rate = unit*weighted_rate) %>%
      select(!!!syms(strata), cases, pyears, crude_rate, crude_lower, crude_upper, weighted_rate, weighted_lower, weighted_upper)

  }



  out.list <- list()

  #Overall
  out.list[["overall"]] <-
    aggregate_df %>%
    get_rates() %>%
    slice(1)

  if(!missing(group)) {

    out.list[[group_c]] <-
      aggregate_df %>%
      get_rates(strata=group_c)



  }

  #Strata
  for(i in strata) {

    out.list[[paste0(i, collapse="_")]] <-
      aggregate_df %>%
      get_rates(i)

  }

  out.list



}


