#' followR
#'
#' @description
#' Function to calculate median follow-up time using the inverse Kaplan-Meier method
#'
#'
#' @param df Input dataframe
#' @param timevar Time to last follow-up
#' @param event Status indicator (censored=0 or dead=1)
#' @param group Optional group indicator if follow-up time should be reported separately
#'
#' @return Returns the median follow-up time with IQR for the whole sample or groupwise
#' @export
#'
#'
followR <- function(df, timevar, event, group) {

  #Formula generation
  lhs <- paste(c("Hist(", paste(substitute(timevar)), ",", paste(substitute(event)), ") ~"), collapse = "")
  rhs_null <- "1"
  form_null <- as.formula(paste(c(lhs, rhs_null), collapse = ""))
  t <- quantile(prodlim(form_null, data = df, reverse = TRUE))
  fu_tab <- data.frame(group  = "Overall",
                       fu = t$quantile[3],
                       q1 = t$quantile[4],
                       q3 = t$quantile[2])

  if(!missing(group)){
    rhs_g <- paste(substitute(group))
    form_g <- as.formula(paste(c(lhs, rhs_g), collapse = ""))
    pg <- prodlim(form_g, data=df)
    t <- quantile(prodlim(form_g, data = df, reverse = TRUE))
    fu_tab <- data.frame(group = t[[rhs_g]],
                         time = t$q,
                         fu = t$quantile) %>%
      filter(time %in% c(0.25,0.5,0.75)) %>%
      pivot_wider(names_from = time, values_from=fu) %>%
      rename(q1 = 4,
             fu = 3,
             q3 = 2) %>%
      select(fu, q1, q3, group) %>%
      bind_rows(fu_tab, .)
  }


  return(fu_tab)

}
