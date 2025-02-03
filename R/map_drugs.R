#' Map_drugs
#'
#' @description
#' Dynamic mapping of drug regimens in the National Danish Pharmaceutical Register.
#'
#'
#' @param data Dataframe, typically LMDB
#' @param drug Regex filtering drugs of interest (e.g. L04AD01 or L0[34])
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
#'
map_drugs <- function(data,
                      drug,
                      pnr = pnr,
                      atc = atc,
                      eksd = eksd,
                      apk = apk,
                      strnum = strnum,
                      packsize = packsize,
                      split=T) {
  lmdb %>%
    filter(str_detect({{atc}}, drug)) %>%
    mutate(window = row_number(),
           #Make windows of prescriptions with short intervals
           window = ifelse({{eksd}}- lag({{eksd}}) < 30 & row_number() > 1, NA, window)) %>%
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
           mdose = total / as.numeric((lead({{eksd}}) - {{eksd}}))) %>%
    rowwise() %>%
    # Find closest dose among defined doses
    mutate(dose = sapply(mdose, closest, str=str, split=split)) %>%
    group_by({{pnr}}) %>%
    #
    mutate(dose = ifelse(row_number() == n(), mode(dose[str == last(str)]), dose),
           duration = pmin(as.numeric(lead({{eksd}}) - {{eksd}}), total/dose),
           duration = ifelse(row_number() == n(), total/dose, duration)) %>%
    #Time-varying stucture
    mutate(tstart = as.numeric({{eksd}} - first({{eksd}})),
           tstop = tstart + duration,
           totaldose = duration *dose,
           cumdose = cumsum(totaldose)) %>%
    select({{pnr}}, {{atc}}, dose, duration, tstart, tstop, totaldose, cumdose)
}
