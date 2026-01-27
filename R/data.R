#' @title Simulated dataset for the matchR algorithm
#'
#' @description Simulation of a population data frame before using the matchR function
#'
#'
#' @format A data frame with 40,100 rows and 11 columns:
#' \describe{
#'   \item{pnr}{unique patient pnr}
#'   \item{case}{whether case = 1 or control = 0}
#'   \item{index}{index date for cases. NA for controls}
#'   \item{follow}{date of last follow-up which can be death, emigration or end of database}
#'   \item{birth}{date of birth}
#'   \item{sex}{patient sex, f = female, m = male}
#'   \item{skinc}{date of first skin cancer occurence}
#'   \item{imm_sup}{index of first diagnosis of immunosuppression}
#'   \item{random1, random2}{random variables}
#'
#'   ...
#' }
"match_df"


#' @title Simulated Redcap dataset
#'
#' @description Simulation of a redcap data frame for basic data management
#'
#'
#' @format A data frame with 500 rows and 15 columns:
#' \describe{
#'   \item{id}{unique patient pnr}
#'   \item{sex}{patient sex, 1 = female, 2 = male}
#'   \item{birth}{date of birth}
#'   \item{age}{age at surgery in years}
#'   \item{followup}{date of last follow-up which can be death, emigration or end of database}
#'   \item{date_of_surgery}{date of surgery}
#'   \item{size}{size of primary tumor in mm}
#'   \item{type}{type of tumor, 0 = benign, 1 = in situ, 2 = malignant}
#'   \item{localisation}{localisation of the primary tumor}
#'   \item{necrosis}{whether necrosis was present in the primary tumor}
#'   \item{margins}{whether there were positive or negative margins at last surgery}
#'   \item{cd10, sox10, ck}{presence or abscence of immunohistochemical markers}
#'   \item{death_date}{date of death}
#'   \item{recurrence_date}{date of first recurrence}
#'   \item{metastasis_date}{date of first metastasis}
#'   ...
#' }
"redcap_df"

#' @title Simulated dataset for model testing
#'
#' @description Simulation of a time-to-event data for the estimatR function
#'
#'
#' @format A data frame with 2000 rows and 14 columns:
#' \describe{
#'   \item{X1}{group variable with three levels (T0, T1 and T2)}
#'   \item{X2}{group variable with two levels 0/1}
#'   \item{X3}{group variable with three levels (T0, T1, T2 and T3)}
#'   \item{X4, X5}{binary random covariates}
#'   \item{X6, X7, X8, X9, X10}{continuous random covariates}
#'   \item{ttt, time2}{event times}
#'   \item{event}{binary status indicator (e.g. alive/death)}
#'   \item{event2}{status indicator where 0 is censoring, 1 is event of interest and 2 is death as a competing risk}
#'   \item{event3}{status indicator with multiple competing risks. 0=censorm, 1=event, 2=death, 3=competing event}
#'   \item{id}{patient id}
#'   \item{X6_bin, X7_bin, X8_bin, X9_bin, X10_bin}{X6-X10 binned based on quintiles}
#'   ...
#' }
"analysis_df"

#' @title Simulated time-dependent covariates dataset.
#'
#' @description Simulation of a time-dependent data frame with covariates from 4000 patients for the matchR function
#'
#'
#' @format A data frame with 84858 rows and 22 columns:
#' \describe{
#'   \item{pnr}{unique patient pnr}
#'   \item{from}{start of the covariate window}
#'   \item{to}{end of the covariate window}
#'   \item{cci}{Charlson Comorbidity Index binned into the groups: 0, 1, 2-3, 4-5 and 6+}
#'   \item{education}{Educational level according to ISCED groups: low, medium and high}
#'   \item{income}{Quantile of the equivalated disposable income: q1, q2, q3 and q4}
#'   \item{marital}{Marital status: Unmarried, married and divorced}
#'   \item{region}{Region of habitance: Capital, central, north, south and zealand}
#'   \item{degurba}{Degree of urbanization in the municipality: city, suburb and rural}
#'   \item{infection, cancer, hema, endo, psych, neuro, cvd, lungs, gi, skin, connective, urinary, congenital}{ICD main groups of covariates: 0 and 1}
#'   ...
#' }
"covariates_df"

#' @title Population table, Denmark
#'
#' @description Population tables for Denmark stratified on 5-year age groups and sex in the period 1990-2025
#'
#'
#' @format A data frame with 1296 rows and 4 columns:
#' \describe{
#'   \item{sex}{sex, m=male, f=female}
#'   \item{age_group}{age groups in 5-year intervals}
#'   \item{year}{year}
#'   \item{population}{number of inhabitants in Denmark in the given stratum}
#'
#'   ...
#' }
"population_denmark"

#' @title Population table, WHO
#'
#' @description WHO standard population in 5-year age groups and sex
#'
#'
#' @format A data frame with 36 rows and 3 columns:
#' \describe{
#'   \item{sex}{sex, m=male, f=female}
#'   \item{age_group}{age groups in 5-year intervals}
#'   \item{population}{number of inhabitants in Denmark in the given stratum}
#'
#'   ...
#' }
"population_who"
