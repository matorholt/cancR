#' Cut numeric variables into intervals
#'
#' @description
#' Wrapper for cut() with simplified labels and automatic conversion to factor based on most frequent levels
#'
#' @param data dataframe. Works with piping.
#' @param vars Vector of vars which should be cut
#' @param seq.list List of the split patterns for each var. E.g. list("v1" = seq(0,10), "v2" = round(quantile(df %>% pull(age), seq(0,1,0.1)),0)).\
#' Can be specified using only "median", "quantile", "quartile" etc which will be applied to all vars
#' @param name.list list of names for new variables in the format: list("new" = "old")
#' @param name.pattern naming pattern that should automatically be pasted on the end of all the specified variable names in "vars".
#' @param simplify whether date intervals should be simplified
#' @param digits number of digits for label formatting
#' @param dt whether a data.table should be returned (default = F)
#'
#'
#' @return The inputted dataframe with the cut variables
#' @export
#'
#' @examples
#' redcap_df %>%
#'   datR(c(birth, date_of_surgery)) %>%
#'   cutR(vars = c(age, birth, size, type, date_of_surgery),
#'        seq.list = list(age = "10y",
#'                        birth = "year",
#'                        size = "quartile",
#'                        type = c(0,2,10),
#'                        date_of_surgery = "quarter"))

# #Multiple seq.lists
# redcap_df %>%
#   datR(c(birth, date_of_surgery)) %>%
#   cutR(vars = c(age, birth, size, type, date_of_surgery),
#        seq.list = list(age = "10y",
#                        birth = "year",
#                        size = "quartile",
#                        type = c(0,2,10),
#                        date_of_surgery = "quarter"))
#
#
# #Recycle seq.list
# redcap_df %>%
#   datR(c(birth, date_of_surgery)) %>%
#   cutR(c(age, birth), "5y",
#        name.pattern = "_bin")
#
# #Quick format
# redcap_df %>%
#   cutR(c(age, size, type), "tertile")


cutR <- function(data,
                 vars,
                 seq.list,
                 name.list = list(),
                 name.pattern = NULL,
                 simplify = T,
                 digits = 0,
                 dt=F) {

  vars <-
    data %>% select({{vars}}) %>% names()

  #Format checking
  for(v in vars) {

    if(any(class(data[[v]]) %in% c("character", "logical"))) {
      return(cat(paste0("Error: ", v, " is ", class(data[[v]]), " and needs to be converted into numerical, date or integer")))
    }

  }

  dat <- copy(as.data.table(data))

  #Name.list
  if(!is.null(name.pattern)) {

    name.list <- as.list(paste0(vars, name.pattern)) %>% set_names(vars)

  } else {

    name.list.default <- as.list(vars) %>% set_names(vars)

    name.list <- modifyList(name.list.default, listR(name.list, "reverse"))

  }

  #Modify seq.list for simple inputs
  if(class(seq.list) != "list") {

    if(length(vars) == length(seq.list)) {
      seq.list <- as.list(seq.list) %>% set_names(vars)
    } else if(length(seq.list == 1)) {
      seq.list <- map(vars, ~ seq.list) %>% set_names(vars)
    } else {
      cat("Error: length of vars != length of seq.list")
    }


  }

  qlist <- list(percentile = 0.01,
                decile = 0.1,
                pentile = 0.2,
                quartile = 0.25,
                tertile = 1/3,
                median = 0.5)

  tlist <- list(year = list(date = as.Date(paste0(c(1800:2100), "-01-01")),
                            time = seq(0,150)),
                "5y" = list(date = as.Date(paste0(seq(1800,2100, 5), "-01-01")),
                            time = seq(0,150,5)),
                "10y" = list(date = as.Date(paste0(seq(1800,2100, 10), "-01-01")),
                             time = seq(0,150,10)),
                half = list(date = as.Date(c(paste0(c(1800:2100), "-01-01"),
                                             paste0(c(1800:2100), "-07-01")))),
                quarter = list(date = as.Date(c(paste0(c(1800:2100), "-01-01"),
                                                paste0(c(1800:2100), "-04-01"),
                                                paste0(c(1800:2100), "-07-01"),
                                                paste0(c(1800:2100), "-10-01")))),
                third = list(date = as.Date(c(paste0(c(1800:2100), "-01-01"),
                                              paste0(c(1800:2100), "-05-01"),
                                              paste0(c(1800:2100), "-09-01")))))

  nlist <- list(bmi = c(0,18, 25, 30, 35, 100))

  #Update seq.list
  seq.vecs <- map(vars, function(v) {

    if(any(seq.list[[v]] %in% names(qlist))) {

      quantile(dat[[v]], seq(0,1,qlist[[seq.list[[v]]]]))

    } else if(any(seq.list[[v]] %in% names(tlist))) {

      if(class(dat[[v]]) %in% c("Date")) {

        tlist[[seq.list[[v]]]][["date"]]

      } else {

        tlist[[seq.list[[v]]]][["time"]]
      }

    } else {

      seq.list[[v]]

    }

  }) %>% set_names(vars)

  map(vars, function(v) {

    label <- name.list[[v]]

    dat[, c(label) := str_replace(cut(round(dat[[v]], digits),
                                      breaks = unique(round(seq.vecs[[v]], digits)),
                                      include.lowest = TRUE,
                                      right = F,
                                      dig.lab = 4),
                                  "\\W(-?\\d+\\.?\\d*)\\,(-?\\d+\\.?\\d*)\\W",
                                  "\\1-\\2")]

    if(simplify) {
      #Year
      if(length(seq.vecs[[v]]) == length(tlist$year$date) & class(seq.vecs[[v]]) == "Date") {
        dat[, c(label) := str_extract(get(label), "\\d{4}")]
      }
      #5y
      if(length(seq.vecs[[v]]) %in% length(tlist[["5y"]]$date) & class(seq.vecs[[v]]) == "Date") {
        dat[, c(label) := paste0(str_extract(get(label), "\\d{4}"), "-", as.numeric(str_extract(get(label), "\\d{4}")) +5)]
      }
      #10y
      if(length(seq.vecs[[v]]) %in% length(tlist[["10y"]]$date) & class(seq.vecs[[v]]) == "Date") {
        dat[, c(label) := paste0(str_extract(get(label), "\\d{4}"), "-", as.numeric(str_extract(get(label), "\\d{4}")) +10)]
      }
      #half
      if(length(seq.vecs[[v]]) %in% length(tlist$half$date) & class(seq.vecs[[v]]) == "Date") {
        dat[, c(label) := ifelse(str_detect(get(label), "\\d{4}-07"), paste0(str_extract(get(label), "\\d{4}"), ", h1"), paste0(str_extract(get(label), "\\d{4}"), ", h2"))]
      }
      #third
      if(length(seq.vecs[[v]]) %in% c(length(tlist$third$date)) & class(seq.vecs[[v]]) == "Date") {
        dat[, c(label) := fcase(str_detect(get(label), "\\d{4}-09"), paste0(str_extract(get(label), "\\d{4}"), ", t3"),
                                str_detect(get(label), "\\d{4}-05"), paste0(str_extract(get(label), "\\d{4}"), ", t2"),
                                default = paste0(str_extract(get(label), "\\d{4}"), ", t1"))]
      }
      #quarter
      if(length(seq.vecs[[v]]) %in% c(length(tlist$quarter$date)) & class(seq.vecs[[v]]) == "Date") {
        dat[, c(label) := fcase(str_detect(get(label), "\\d{4}-10"), paste0(str_extract(get(label), "\\d{4}"), ", q4"),
                                str_detect(get(label), "\\d{4}-07"), paste0(str_extract(get(label), "\\d{4}"), ", q3"),
                                str_detect(get(label), "\\d{4}-04"), paste0(str_extract(get(label), "\\d{4}"), ", q2"),
                                default = paste0(str_extract(get(label), "\\d{4}"), ", q1"))]
      }
    }

    dat[, c(label) := ifelse(str_detect(get(label), "NA"), NA, get(label))]

    dat[, c(label) := fct_infreq(get(label))]

  })

  if(dt) return(dat) else return(as.data.frame(dat))

}
