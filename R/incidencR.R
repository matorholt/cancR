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
#' @param print.model whether the poisson model with results should be printed (default = F)
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
                      print.model = F,
                      age = age,
                      sex = sex,
                      dt = F) {

  #Return DT if input is DT and dt is not specified
  if(is.data.table(data) & missing(dt)) dt <- T

  if(reference %nin% c("full", "partial", "male", "female")) cli::cli_abort("Error: Argument reference must be full, partial, male or female")

  dat <- as.data.table(data)

  setnames(dat,
           c(defusR(c(sex, age, index))),
           c("sex", "age", "index"))

  covs <- c("age_group", "sex", "year")

  if(!missing(group)) {

    group_c <- defusR(group)
    dat <- dat[, (group_c) := as.character(get(group_c))]
    covs <- c(covs, group_c)

  } else {
    group_c <- NULL
  }

  if(any(unique(unlist(strata)) %nin% covs)) {
    cli::cli_abort("Error: {setdiff(unique(unlist(strata)), covs)} not present in data")
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
    .[, .(count = .N), by = covs]

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
    factR(vars = covs)

  setnames(full_data, "population", "total")


  rhs <- paste0(c("year", "age_group", "sex", "offset(log(total))"), collapse = " + ")

  mod_list <- list()

  if(!missing(group)) {

    #Overall model without group if specified
    overall_mod <- glm(as.formula(paste0("count ~ ", rhs)),
                       data   = copy(full_data)[, .(count = sum(count), total = first(total)), by = .(age_group, sex, year)],
                       family = poisson(link = "log")
    )

    mod_list <- list(overall_mod)

    rhs <- paste0(group_c, " * ", rhs)

  }

  mod <- glm(as.formula(paste0("count ~ ", rhs)),
             data   = full_data,
             family = poisson(link = "log")
  )

  mod_list <- c(mod_list, list(mod))


  if(print.model) {

    cli::cli_text("Reference groups")
    data.table(variable = covs,
               reference = map_chr(covs, ~ levels(full_data[[.x]])[1])) %>% print

    est <- coef(mod)
    ci  <- confint.default(mod)   # Wald 95% CI

    coefs <- data.frame(
      variable  = names(est),
      ratio    = exp(est),
      lower = exp(ci[, 1]),
      upper = exp(ci[, 2]),
      p.value = pvertR(summary(mod)$coefficients[, 4]),
      row.names = NULL
    ) %>% arrange(variable) %>% print
  }

  #Add standard populations
  pred_dat <-
    full_data %>%
    #Fix person years unit
    mutate(pop = total,
           total = unit) %>%
    joinR(population_who, population_euro, by = c("sex", "age_group")) %>%
    rename(who = population.x,
           euro = population.y) %>%
    group_by(!!!syms(covs[covs %nin% "year"])) %>%
    #Split weight within groups (over years)
    mutate(across(c(who, euro), ~ . / n())) %>%
    ungroup()

  #Loop over overall + strata specifications
  res <- imap(c(list(overall = NULL), strata), function(svars, stratum) {

    #Loop over standard populations (NULL = Crude)
    imap(list(crude = "pop", euro = "euro", who = "who"), function(w, nm) {

      #Use overall model when subgroup
      args <- list(model   = if(is.null(group_c) || is.null(svars) || group_c %nin% svars) mod_list[[1]] else mod_list[[length(mod_list)]],
                   newdata = pred_dat,
                   by      = if (is.null(svars)) TRUE else svars,
                   wts     = w,
                   type    = "response")

      out <- do.call(marginaleffects::avg_predictions, args) %>%
        as.data.frame() %>%
        mutate(se_log    = std.error / estimate,
               conf.low  = estimate * exp(-qnorm(0.975) * se_log),
               conf.high = estimate * exp( qnorm(0.975) * se_log)) %>%
        select(any_of(svars), estimate, conf.low, conf.high) %>%
        rename_with(~ paste0(c("estimate", "lower", "upper"), "_", nm),
                    c(estimate, conf.low, conf.high))

      if(dt) as.data.table(out) else as.data.frame(out)

    }) %>% {
      if(is.null(svars)) bind_cols(.) else joinR(., by = svars)
    }

  }) %>% set_names(c("overall", map_chr(strata, ~ paste(.x, collapse = "_"))))

}

