#' Directly standardized incidence rates using the WHO standard population
#'
#' @description
#' Directly standardized incidence rates inspired by the dsr package by Matt Kumar. The incidence rates are standardized on 5-year age intervals and sex for each year in the period 1990-2025 based on population tables of the Danish population. Lastly the incidence rates are weighted based on the WHO world population.
#'
#'
#' @param data data set containing age, sex and index date and group
#' @param group optional if incidence rates should be provided per group
#' @param strata list of vectors for which strata the incidence rates should be reported (e.g. per age-group and sex)
#' @param unit the unit of the incidence rate. Default is 100.000 person years
#' @param reference whether the reference population should include all possible age-sex-groups ("full" (default)) or only the age-groups present in data ("partial")
#' @param index variable name of the index data
#' @param age name of the age variable
#' @param sex name of the sex variable
#'
#' @returns a standardized incidence rate of the overall population and, if specified in the strata-argument, stratified incidence rates
#' @export
#'
#' @examples


#'(rates <-
#'    incidencR(redcap_df %>%
#'                recodR(list("sex" = list("Female" = 1,
#'                                         "Male" = 2))),
#'              index = date_of_surgery,
#'              group = type,
#'              unit = 100000,
#'              strata = list(c("year"),
#'                            c("age", "sex"),
#'                            c("year", "type"),
#'                            c("year", "type", "age"),
#'                            c("type", "age"),
#'                            c("year", "age", "sex", "type"))))
#'
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

incidencR <- function(data,
                      group,
                      strata = list(c("year")),
                      unit = 100000,
                      reference = "full",
                      index,
                      age = age,
                      sex = sex,
                      dt = F) {

  #Return DT if input is DT and dt is not specified
  if(is.data.table(data) & missing(dt)) dt <- T

  if(reference %nin% c("full", "partial", "male", "female")) cat("Error: Argument reference must be full, partial, male or female")

  dat <- as.data.table(data)

  setnames(dat,
           c(defusR(c(sex, age, index))),
           c("sex", "age", "index"))

  covs <- c("age_group", "sex", "year")

  if(!missing(group)) {

    group_c <- defusR(group)
    dat <- dat[, (group_c) := as.character(get(group_c))]
    covs <- c(covs, group_c)

  }

  #Removing NAs
  if(sum(is.na(data$sex)) > 0 | sum(is.na(data$age)) > 0) {
    cli::cli_alert_warning("NAs detected")
    cli::cli_ul(c(paste0("Age: ", sum(is.na(data$age))), paste0("Sex: ", sum(is.na(data$sex)))))
  }

  aggregate_df <-
    dat[!is.na(sex) & !is.na(age)] %>%
    cutR(age,
         c(seq(0,85,5), 150),
         "age_group",
         autoformat = F) %>%
    .[, `:=` (year = str_extract(index, "\\d{4}"),
              sex = str_to_lower(str_extract(sex, "\\w")),
              age_group = ifelse(age_group == "85-150", "85+", as.character(age_group)))] %>%
    .[, covs, with = FALSE] %>%
    .[, count := .N, by = covs]

  #Prep for expand.grid. If full all unique levels in pop_DK, if partial only unique levels in data.
  grid_list <-
    c(map(c("age_group", "sex"), function(i) {

      if(reference == "full") levels <- unique(population_denmark[[i]])

      if(reference == "partial") levels <- unique(aggregate_df[[i]])

      levels

    }),
    #Sequence of years in observed population
    list(as.character(do.call(seq, as.list(range(as.numeric(aggregate_df[["year"]])))))))

  if(!missing(group)) grid_list <- c(grid_list, list(as.character(unique(aggregate_df[[group_c]]))))

  grid <- do.call(CJ, grid_list %>% set_names(covs))

  full_data <-
    joinR(grid, aggregate_df, by = covs) %>%
    joinR(., population_denmark, by = c("sex", "age_group", "year")) %>%
    .[, count := ifelse(is.na(count), 0, count)] %>%
    .[, year := as.numeric(year)] %>%
    factR(vars = covs[covs != "year"])

  setnames(full_data, "population", "total")


  rhs <- paste0(c("age_group", "sex", "splines::ns(year, df = 4)", "offset(log(total))"), collapse = " + ")

  if(!missing(group)) {
    rhs <- paste0(group_c, " + ", rhs)
  }


  mod <- glm(as.formula(paste0("count ~ ", rhs)),
             data   = full_data,
             family = poisson(link = "log")
  )

  #Add standard populations
  pred_dat <-
    full_data %>%
    #Fix person years unit
    mutate(total = unit) %>%
    joinR(population_who, population_euro, by = c("sex", "age_group")) %>%
    rename(who = population.x,
           euro = population.y) %>%
    group_by(!!!syms(covs)) %>%
    #Split weight within groups (over years)
    mutate(across(c(who, euro), ~ . / n())) %>%
    ungroup()

  #Loop over overall + strata specifications
  res <- imap(c(list(overall = NULL), strata), function(svars, stratum) {

    #Loop over standard populations (NULL = Crude)
    imap(list(crude = NULL, euro = "euro", who = "who"), function(w, nm) {

      args <- list(model   = mod,
                   newdata = pred_dat,
                   by      = if (is.null(svars)) TRUE else svars)

      if (!is.null(w)) args$wts <- w

      out <- do.call(marginaleffects::avg_predictions, args) %>%
        as.data.frame() %>%
        select(any_of(svars), estimate, conf.low, conf.high) %>%
        rename_with(~ paste0(c("estimate", "lower", "upper"), "_", nm),
                    c(estimate, conf.low, conf.high))

      if(dt) as.data.table(out) else as.data.frame(out)

    }) %>% {
      if(is.null(svars)) bind_cols(.) else joinR(., by = svars)
    }

  }) %>% set_names(c("overall", map_chr(strata, ~ paste(.x, collapse = "_"))))

}
