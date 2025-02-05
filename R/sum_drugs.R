#' Drug summary
#'
#' @description
#' Summary function for the map_drugs function
#'
#'
#' @param mobj map_drugs object created with the map_drugs function
#' @param pnr pnr
#' @param trend whether trend lines should be shown, defaults to FALSE
#' @param scale whether scaling should be applied (e.g daily to weekly dose)
#'
#' @return Returns summary plots of dosage trends
#' @export
#'
#'
sum_drugs <- function(mobj, pnr = pnr, trend=F,scale) {

  if (!inherits(mobj, "Mobj")) {
    stop(
      "The input is not of class Mobj created using the map_drugs function - maybe ask MØ??"
    )
  }

  df <- as.data.frame(mobj) %>% mutate(pnr = as.factor({{pnr}}))

  if(!missing(scale)) {
    df <- df %>% mutate(dose = dose * scale)
  }

  cum <- ggplot(df, aes(x=tstop, y=cumdose, group=pnr, col=pnr)) +
    geom_point() +
    geom_line() +
    labs(title = "Cumulative dose", x="Years") +
    scale_x_continuous(breaks=seq(0,20*365.25,365.25), label=seq(0,20,1)) +
    theme_classic() +
    theme(panel.grid.major = element_line(scales::alpha("grey92", .7),
                                          linewidth = 0.5),
          legend.position = "none")
  if(trend) {
    cum <- cum + geom_smooth(method="loess", aes(col = NULL, group=NULL), color = "Steelblue")
  }


  doses <- df %>% mutate(period = ifelse(tstart != lag(tstop) | tstop != lead(tstart), row_number(), NA)) %>%
    fill(period, .direction = "updown") %>%
    pivot_longer(cols=c(tstart, tstop), names_to = "start_stop", values_to = "time") %>%
    ggplot(aes(x=time, y=dose, group=as.factor(period), col = pnr)) +
    geom_step() +
    labs(title = "Dose over time", x="Years")+
    scale_x_continuous(breaks=seq(0,20*365.25,365.25), label=seq(0,20,1)) +
    theme_classic() +
    theme(panel.grid.major = element_line(scales::alpha("grey92", .7),
                                          linewidth = 0.5),
          legend.position = "none")
  if(trend) {
    doses <- doses + geom_smooth(method="loess", aes(col = NULL, group=NULL), color = "Steelblue")
  }

  dose_dist <- df %>% group_by(pnr, dose) %>% summarise(n = n()) %>%
    ungroup() %>%
    mutate(sum = sum(n)) %>%
    group_by(dose) %>%
    summarise(dosesum = sum(n),
              total = first(sum)) %>%
    mutate(pct = dosesum/total*100,
           dose = as.factor(dose)) %>%
    ggplot(aes(x=dose, y=pct, fill=dose)) +
    geom_col() +
    theme_classic() +
    theme(panel.grid.major = element_line(scales::alpha("grey92", .7),
                                          linewidth = 0.5)) +
    labs(title = "Distribution of doses", x="Dose") +
  scale_y_continuous(breaks=seq(0,100,10), labels = paste0(seq(0,100,10), "%"))

uni_dose <- df %>% distinct(pnr, dose) %>% group_by(pnr) %>% summarise(n = n()) %>%
  ungroup() %>%
  group_by(n) %>%
  summarise(n2 = n()) %>%
  mutate(tot = n_distinct(df$pnr),
         pct = n2 / tot * 100) %>%
  ggplot(aes(x=n2, y=pct)) +
  geom_col(fill = "Steelblue")  +
  theme_classic() +
  theme(panel.grid.major = element_line(scales::alpha("grey92", .7),
                                        linewidth = 0.5)) +
  labs(title = "Distinct doses", x="Unique number of doses")

  plot <- (cum + doses) / (dose_dist + uni_dose)
  return(plot)
}



