#' Map_drugs
#'
#' @description
#' Dynamic mapping of drug regimens in the National Danish Pharmaceutical Register.
#'
#'
#' @param data Dataframe, typically LMDB
#' @param drug Regex filtering drugs of interest (e.g. L04AD01 or L0[34])
#' @param windowsize Interval between two redeems that should be considered one
#' @param pnr pnr
#' @param atc atc
#' @param eksd eksd
#' @param apk apk
#' @param strnum strnum
#' @param packsize packsize
#' @param split whether the tablets can be split or dose every second day is an option
#'
#' @return A dataframe with dose, duration, cumulated dose as time-varying measures.
#' @export
#'
#'  @examples
#'  lmdb <- rbind(data.frame(pnr=c(1,1,1,1,1,1,1,1,1,1,1,2),
#'                   atc=rep("L04AD01", 12),
#'                   eksd = as.Date(c("2001-01-01",
#'                                  "2001-04-01",
#'                                  "2001-07-01",
#'                                  "2001-10-01",
#'                                  "2001-10-14",
#'                                  "2001-12-31",
#'                                  "2002-02-01",
#'                                  "2002-02-14",
#'                                  "2002-05-01",
#'                                  "2002-05-14",
#'                                  "2003-10-01",
#'                                  "2001-01-01")),
#'                 apk = c(2,2,2,1,1, 2, 1, 1,2,1,1,1),
#'                 strnum = c(50, 50, 50, 100, 100, 50, 50,100, 50, 100, 100,50),
#'                 packsize = c(50, 50, 50, 50, 50, 50, 50, 50, 50,50, 50,50)),
#'
#'            data.frame(pnr=c(3),
#'                       atc=rep("L04AD01", 1),
#'                       eksd = as.Date(c("2001-01-01")),
#'                       apk = c(6),
#'                       strnum = c(50),
#'                       packsize = c(50)),
#'            data.frame(pnr=rep(4, 3),
#'                       atc=rep("L04AD01", 3),
#'                       eksd = as.Date(c("2001-01-01",
#'                                        "2001-01-11",
#'                                        "2001-01-25")),
#'                       apk = c(3,3,2),
#'                       strnum = c(50,25,50),
#'                       packsize = c(50,50,50)))
#'
#' map_drugs(lmdb, drug = "L04AD01")

#'
#'
map_drugs <- function(data,
                      drug,
                      windowsize = 30,
                      doses=NULL,
                      pnr = pnr,
                      atc = atc,
                      eksd = eksd,
                      apk = apk,
                      strnum = strnum,
                      packsize = packsize,
                      split=T) {
  data %>%
    filter(str_detect({{atc}}, drug)) %>%
    group_by(pnr) %>%
    arrange(pnr, eksd) %>%
    mutate(window = row_number(),
           #Make windows of prescriptions with short intervals
           window = ifelse({{eksd}}- lag({{eksd}}) < windowsize & row_number() > 1, NA, window)) %>%
    fill(window, .direction = "down") %>%
    group_by({{pnr}}, window) %>%
    #Collapse short invervals to one
    mutate(str = case_when({{strnum}} == lead({{strnum}}) ~ first({{strnum}}),
                           {{strnum}} != lead({{strnum}}) & {{apk}} == lead({{apk}}) ~ sum({{strnum}}),
                           {{strnum}} != lead({{strnum}}) & {{apk}} != lead({{apk}}) ~ {{apk}}*{{strnum}},
                           T ~ {{strnum}}),
           packs = case_when({{strnum}} == lead({{strnum}}) ~ sum({{apk}}),
                             {{strnum}} != lead({{strnum}}) & {{apk}} == lead({{apk}}) ~ first({{apk}}),
                             {{strnum}} != lead({{strnum}}) & {{apk}} != lead({{apk}}) ~ max({{apk}}),
                             T ~ {{apk}}),
           num = case_when({{strnum}} == lead({{strnum}}) ~ sum({{packsize}}),
                           {{strnum}} != lead({{strnum}}) & {{apk}} == lead({{apk}}) ~ first({{packsize}}),
                           {{strnum}} != lead({{strnum}}) & {{apk}} != lead({{apk}}) ~ min({{packsize}}),
                           T ~ {{packsize}})) %>%
    slice(1) %>% ungroup() %>%
    select(-{{apk}}, -{{strnum}}, -window, -{{packsize}}) %>%
    group_by({{pnr}}) %>%
    #Find total doses and durations
    mutate(total = str*packs*num,
           mdose = total / as.numeric((lead({{eksd}}) - {{eksd}})),
           mdose = ifelse(is.na(mdose), str, mdose)) %>%
    rowwise() %>%
    # Find closest dose among defined doses
    mutate(dose = sapply(mdose, closest, str=str, vec=doses, split=split)) %>%
    group_by({{pnr}}) %>%
    #
    mutate(dose = ifelse(row_number() == n(), mode(dose[str == last(str)]), dose),
           duration = pmin(as.numeric(lead({{eksd}}) - {{eksd}}), total/dose),
           duration = ifelse(row_number() == n(), total/dose, duration))
    # #Time-varying stucture
    # mutate(tstart = as.numeric({{eksd}} - first({{eksd}})),
    #        tstop = tstart + duration,
    #        totaldose = duration *dose,
    #        cumdose = cumsum(totaldose)) %>%
    # select({{pnr}}, {{atc}}, dose, duration, tstart, tstop, totaldose, cumdose)
}

