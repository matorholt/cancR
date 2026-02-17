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
#' @param reference whether the reference population should include all possible age-sex-groups ("full" (default)) or only the age-groups present in data ("partial")
#' @param index variable name of the index data
#' @param age name of the age variable
#' @param sex name of the sex variable
#'
#' @returns a standardized incidence rate of the overall population and, if specified in the strata-argument, stratified incidence rates
#' @export
#'
#' @examples
#' (rates <-
#' incidencR(redcap_df %>%
#'            recodR(list("sex" = list("Female" = 1,
#'                                      "Male" = 2))),
#'           index = date_of_surgery,
#'           group = type,
#'           unit = 100000,
#'           strata = list(c("year"),
#'                         c("age", "sex"),
#'                         c("year", "type"),
#'                         c("year", "type", "age"),
#'                         c("type", "age"),
#'                         c("year", "age", "sex", "type"))))
#'
#' ggplot(rates$year_type, aes(x=year, y=weighted_rate, color = type, fill = type)) +
#'   geom_point() +
#'   geom_line() +
#'   #geom_ribbon(aes(ymin = weighted_lower, ymax = weighted_upper), alpha = 0.2, color = NA) +
#'   geom_smooth(se=F) +
#'   theme_classic()
#'
#'

# (rates <-
#     incidencR(redcap_df %>%
#                   recodR(list("sex" = list("Female" = 1,
#                                            "Male" = 2))),
#               index = date_of_surgery,
#               group = type,
#               unit = 100000,
#               reference = "full",
#               #reference = "partial",
#               ci.method = "lognormal",
#               strata = list(c("year"),
#                             c("age", "sex"),
#                             c("year", "type"),
#                             c("year", "type", "age"),
#                             c("type", "age"),
#                             c("year", "age", "sex", "type"))))
#
# ggplot(rates$year_type, aes(x=year, y=weighted_rate, color = type, fill = type)) +
#   geom_point() +
#   geom_line() +
#   #geom_ribbon(aes(ymin = weighted_lower, ymax = weighted_upper), alpha = 0.2, color = NA) +
#   geom_smooth(se=F) +
#   theme_classic()

incidencR <- function(data, group, strata = list(c("year")), unit = 100000, ci.method = "lognormal", reference = "full", index, age = age, sex = sex) {

  if(reference %nin% c("full", "partial")) cat("Error: Argument reference must be full or partial")

  data <- data %>%
    rename(sex = {{sex}},
           age = {{age}},
           index = {{index}})

  if(!missing(group)) {

    group_c <- data %>% select({{group}}) %>% names

  } else {

    group_c <- "grp"
    data <- data %>%
      mutate(grp = 1)

  }

aggregate_df <- data %>%
    mutate(year = str_extract(index, "\\d{4}"),
           sex = str_to_lower(str_extract(sex, "\\w"))) %>%
    cutR(age,
         c(seq(0,85,5), 150),
         name.list = list(age = "age_group")) %>%
    mutate(age_group = ifelse(age_group == "85-150", "85+", as.character(age_group)),
           !!sym(group_c) := as.character(!!sym(group_c))) %>%
    select(year, age_group, sex, !!sym(group_c)) %>%
    group_by(!!sym(group_c), year, age_group, sex) %>%
    summarise(count = n(), .groups = "drop") %>%
  ungroup

#Prep for expand.grid. If full all unique levels in pop_DK, if partial only unique levels in data.
grid_list <- c(lapply(c("sex", "age_group"), function(i) {

  if(reference == "full") levels <- unique(population_denmark[[i]])

  if(reference == "partial") levels <- unique(aggregate_df[[i]])

  levels

}),
list(as.character(do.call(seq, as.list(range(as.numeric(aggregate_df[["year"]])))))),
list(as.character(unique(aggregate_df[[group_c]])))) %>% set_names("sex", "age_group", "year", group_c)

full_data <-
  left_join(expand.grid(grid_list), aggregate_df, by = c("sex", "age_group", "year", group_c)) %>%
  left_join(., population_denmark %>% rename(fu = population), by = c("sex", "age_group", "year")) %>%
  left_join(., population_who, by = c("sex", "age_group"), relationship = "many-to-many") %>%
  mutate(year = as.numeric(year)) %>%
  rename(age = age_group) %>%
  mutate(count = ifelse(is.na(count), 0, count))


  get_rates <- function(data, strata=NULL) {

    strata_v <- unique(c(strata, c("age","sex")))

    data <- data %>%
      #Strata-specific counts and pyears
      group_by(!!!syms(strata_v)) %>%
      summarise(count = sum(count),
                pyears = sum(fu),
                population = first(population), .groups="drop") %>%

      #Estimate standardized rates stratified on strata
      group_by(!!!syms(strata)) %>%
      mutate(cases = sum(count),
             rate = count/pyears,
             rate_var = count / pyears ^ 2,
             crude_rate = sum(count)/sum(pyears),
             crude_var = sum(count) / sum(pyears) ^2,
             #WHO standard is 100000 in total
             wts=population/sum(population),
             weighted_rate = sum(wts*(rate)),
             weighted_var=sum(as.numeric((wts^2)*rate_var))
             ) %>%
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
             weighted_rate = unit*weighted_rate,
             across(contains(c("lower", "upper")), ~ ifelse(is.na(.), 0, .))) %>%
      select(!!!syms(strata), cases, pyears, crude_rate, crude_lower, crude_upper, weighted_rate, weighted_lower, weighted_upper)

  }



  out.list <- list()

  #Overall
  out.list[["overall"]] <-
    full_data %>%
    get_rates() %>%
    slice(1)

  if(!missing(group)) {

    out.list[[group_c]] <-
      full_data %>%
      get_rates(strata=group_c)



  }

  #Strata
  for(i in strata) {

    out.list[[paste0(i, collapse="_")]] <-
      full_data %>%
      get_rates(i)

  }

  out.list



}
