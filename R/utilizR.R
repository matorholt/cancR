#' @title Pick the closest value from a range in vector.
#' @param x Input number
#' @param vec vector to search for closest value. If NULL, provide str which vec should be created from
#' @param str Multiplier for the generic vector
#' @param split whether the tablet can be split
#'
#'
#' @return Returns the dose closest to the estimated
#' @export

closR <- function(x, vec=NULL, str, split=T) {
  if(is.na(x)) {
    return(x)
  } else {
    if(is.null(vec)){
      if(split) {
        vec <- str * c(0.5, seq(1,6))
      } else {
        vec <- str * seq(1, 6)
      }
    }
  }
  vec[which.min(abs(x-vec))]
}

#' @title Collection of multiple plots into one. Wrapper for ggarrange. Input must be a list of plots.
#' @param plots List of plots to be collected
#' @param collect Whether labels should be collected
#' @param nrow Number of rows
#' @param ncol Number of colums
#' @param ... See ggarrange
#'
#' @return Collected plot
#' @export
#'
#'
# n <- 500
# set.seed(1)
# df <- riskRegression::sampleData(n, outcome="survival")
# df$time <- round(df$time,1)*12
# df$time2 <- df$time + rnorm(n)
# df$X1 <- factor(rbinom(n, prob = c(0.3,0.4) , size = 2), labels = paste0("T",0:2))
# df$X3 <- factor(rbinom(n, prob = c(0.3,0.4,0.3) , size = 3), labels = paste0("T",0:3))
# df$event2 <- rbinom(n, 2, prob=.3)
# df <- as.data.frame(df)
#
# df2 <- df %>% mutate(X2 = ifelse(row_number()==1, NA, X2),
#                      event = as.factor(event)) %>%
#   rename(ttt = time)
#
# t2 <- estimatR(df2, ttt, event2, X2, time = 60)
# t3 <- estimatR(df2, ttt, event2, X1, time = 60)
# t4 <- estimatR(df2, ttt, event2, X3, time = 60)
#
# #Padding for scale correction. Can also be table.space.
# p2 <- plotR(t2, table.padding = 1.85)
# p3 <- plotR(t3, y.title = "", table.padding=1.4)
# p4 <- plotR(t4, y.title = "")
#
# collectR(list(p2,p3,p4), legend.grob = get_legend(p4))

collectR <- function(plots, collect=T, ...) {

  ggpubr::ggarrange(plotlist = plots, common.legend=collect, ...)
}

#' @title Generate all possible cominations/permutations
#' @description
#' All possible combinations of a single vector (ABC: ABC, ACB, BAC, BCA, CAB, CBA) or two separate vectors (AB, CD: ABCD, CDAB)
#' The function does not allow for replacements.
#'
#' @param letters Vector either of length 1 (will be split for each subelement) og length >1
#' @param letters2 Optional second vector if chunks are to be combined
#' @param list Whether all combinations should be returned as a list (useful for looping with lapply)
#'
#' @return Returns all possible combinations of the input vector(s)
#' @export
#'
#'
combinR <- function(letters, letters2=NULL, list=F) {
  #Convenience split if only one combination is provided
  if(length(letters) == 1 & is.null(letters2)) {
    letters <- unlist(stringr::str_split(letters, ""))
  }
  #Assumes single vectorif two vectors both of length 1 is provided (e.g. "AB", "CD" -> c("AB", "CD"))
  if(length(letters2) == 1 ) {
    letters <- as.vector(c(letters, letters2))
    letters2 <- NULL
  }
  #If two vectors are provided, all possible combinations are found
  if(!is.null(letters2)) {
    as.vector(apply(expand.grid(letters, letters2), 1, function(x) paste0(x, collapse="")))
    #If only one vector is provided, all possible unique combinations are found
  } else if(!list){
    as.vector(na.omit(apply(expand.grid(mget(rep("letters", length(letters)))), 1, function(x) ifelse(length(unique(x)) == length(letters), paste0(x, collapse=""), NA ))))
  } else {
    (str_split(as.vector(na.omit(apply(expand.grid(mget(rep("letters", length(letters)))), 1, function(x) ifelse(length(unique(x)) == length(letters), paste0(x, collapse=";"), NA )))), ";"))
  }
}

#' @title Fix CPR numbers with removed leading zeros
#' @param data dataset
#' @param cpr cpr-column
#' @param extract TRUE if age and date of birth should be extracted
#' @param remove.cpr whether invalid CPRs should be removed, default = F
#' @param return.cpr whether the invalid CPRs should be returned as a vector, default = F
#'
#' @return Returns same dataset with correct CPR numbers and optionally age and date of birth. Invalid CPRs stops the function and returns the invalid CPRs as a vector.
#' @export
#'

# data.frame(cpr = c("010101-1234",
#                    "0101011234",
#                    "9999999999",
#                    "101011234"
# )) %>%
#   cpR(extract=T,
#       return.cpr=F,
#       remove.cpr=T)


cpR <- function(data, cpr=cpr,extract=F, remove.cpr = F, return.cpr = F) {

  data <- data %>% ungroup()

  cpr_c <- data %>% select({{cpr}}) %>% names

  errors <- data %>% filter(str_detect(data[[cpr_c]], "^\\d{9,10}$|^\\d{5,6}-?\\w{4}$", negate=T) |
                              str_count(data[[cpr_c]]) == 10 & str_sub(data[[cpr_c]], 1,2) %in% c("00", seq(32,99)) |
                              str_count(data[[cpr_c]]) == 10 & str_sub(data[[cpr_c]], 3,4) %in% c("00", seq(13,99)) |
                              str_count(data[[cpr_c]]) == 9 & str_sub(data[[cpr_c]], 2,3) %in% c("00", seq(13,99)) |
                              is.na(data[[cpr_c]]))

  if(nrow(errors) > 0) {

    if(return.cpr) {
      warning(paste0(nrow(errors), " invalid CPR", rep("s", nrow(errors)>1), " detected and returned as vector"))
      return(errors[[cpr_c]])
    } else {

      if(remove.cpr) {
        warning(paste0(nrow(errors), " invalid CPR", rep("s", nrow(errors)>1), " detected and removed"))
        data <- data %>% filter(!!sym(cpr_c) %nin% errors[[cpr_c]])
      } else {
        warning(paste0(nrow(errors), " invalid CPR", rep("s", nrow(errors)>1), " detected"))
      }
    }
  }


  data <- data %>%
    mutate(!!sym(cpr_c) := str_pad(str_remove_all(!!sym(cpr_c), "-"), width=10, pad="0"))

  if(extract) {
    data <- data %>%
      mutate(sex = case_when(str_sub({{cpr}}, 10) %in% seq(0,8,2) ~ "F",
                             str_sub({{cpr}}, 10) %in% seq(1,9,2) ~ "M"),
             birth = case_when(str_sub({{cpr}}, 5,6) %in% str_pad(seq(0,36), 2, pad="0") &
                                 str_sub({{cpr}}, 7,7) %in% c(4, 9) ~ as.Date(str_c("20", str_replace_all({{cpr}}, "(\\d{2})(\\d{2})(\\d{2})(\\w{4})", "\\3-\\2-\\1"), sep="")),
                               str_sub({{cpr}}, 5,6) %in% str_pad(seq(0,57), 2, pad="0") &
                                 str_sub({{cpr}}, 7,7) %in% seq(5,8) ~ as.Date(str_c("20", str_replace_all({{cpr}}, "(\\d{2})(\\d{2})(\\d{2})(\\w{4})", "\\3-\\2-\\1"), sep="")),
                               str_sub({{cpr}}, 5,6) %in% str_pad(seq(58,99), 2, pad="0") &
                                 str_sub({{cpr}}, 7,7) %in% seq(5,8) ~ as.Date(str_c("18", str_replace_all({{cpr}}, "(\\d{2})(\\d{2})(\\d{2})(\\w{4})", "\\3-\\2-\\1"), sep="")),

                               T ~ as.Date(str_c("19", str_replace_all({{cpr}}, "(\\d{2})(\\d{2})(\\d{2})(\\w{4})", "\\3-\\2-\\1"), sep=""))),
             across(c(sex, birth), ~ case_when(cpr %in% errors[[cpr_c]] ~ NA,
                                               T ~ .)))
  }

  return(data)
}




#' @title Convert dates from character to date format
#' @description
#' Convert dates easily without specifying format. The format is identified automatically and converted to standard Year-month-day.
#' @param data data frame
#' @param vars vector of character vars to convert to date format
#'
#' @returns the input data frame with correctly formatted date variables
#' @export
#'
#

# redcap_df %>%
#   datR(c(birth, date_of_surgery, followup, death_date)) %>%
#   str

datR <- function(data, vars) {

  vars_c <- data %>% select({{vars}}) %>% names()

  formats <- list()

  for(v in vars_c) {

    #Automatic formatting
    formats[[v]] <- case_when(str_detect(first(data %>% pull(!!sym(v)), na_rm=T), "\\b\\d{2}-\\d{2}-\\d{4}\\b") ~ "%d-%m-%Y",
                              T ~ "%Y-%m-%d")

    if(any(class(data[[v]]) %nin% "Date")) {
      data[[v]] <- as.Date(data[[v]], format = formats[[v]])

    }


  }

  data

}

#' @title Assessment of distribution of continuous variables with histograms, QQ-plots and the Shapiro-Wilks test
#' @param data dataframe
#' @param vars variables to test. If not specified all numeric variables with more than 5 unique values are assessed
#' @param bins binwidth
#' @param test whether the Shapiro-Wilks (default) or Kolmogorov-Smirnov test should be performed
#'
#' @return Combined plotframe of histograms, QQ-plots and the Shapiro-Wilks tests
#' @export
#'
#'

# n <- 500
# set.seed(1)
# df <- riskRegression::sampleData(n, outcome="survival")
# df$time <- round(df$time,1)*12
# df$time2 <- df$time + rnorm(n)
# df$X1 <- factor(rbinom(n, prob = c(0.3,0.4) , size = 2), labels = paste0("T",0:2))
# df$X3 <- factor(rbinom(n, prob = c(0.3,0.4,0.3) , size = 3), labels = paste0("T",0:3))
# df$event2 <- rbinom(n, 2, prob=.3)
# df <- as.data.frame(df)
#
# df <- df %>% mutate(X2 = ifelse(row_number()==1, NA, X2),
#                      event = as.factor(event)) %>%
#   rename(ttt = time)
#
# distributR(df, vars=c(X6, X7, X8))

distributR <- function(data, vars, bins = 1, test = "shapiro") {

  test <- match.arg(test, c("shapiro","kolmogorov"))

  if(missing(vars)) {
    vars_c <- data %>% select(where(~all(length(unique(.))>5 & is.numeric(.)))) %>% names()
  } else {
    vars_c <- data %>% select({{vars}}) %>% names()
  }

  plotlist <- list()

  for(v in 1:length(vars_c)) {

    c <- sample(c("#9B62B8", "#224B87", "#67A8DC", "#D66ACE", "orange"), 1)



    p1 <-
      ggplot(data, aes(x=!!sym(vars_c[v]))) +
      geom_histogram(fill=c, col="Black", binwidth = bins) +
      theme_classic() +
      labs(title = vars_c[v])

    p2 <-
      ggplot(data, aes(sample = !!sym(vars_c[v]))) +
      stat_qq(col = c) +
      stat_qq_line() +
      theme_classic()

    if(test == "shapiro") {
      p2 <- p2 +
        labs(title = paste0("Shapiro-Wilks test: ", pvertR(shapiro.test(data[, vars_c[v]])$p.value)))
    } else if(test == "kolmogorov") {
      p2 <- p2 +
        labs(title = paste0("Kolmogorov-Smirnov test: ", pvertR(ks.test(data[, vars_c[v]], "pnorm")$p.value)))
    }

    plotlist[[v]] <-
      ggpubr::ggarrange(p1, p2)

  }

  ggpubr::ggarrange(plotlist = plotlist, ncol = 1, nrow = length(vars_c))

}


#' @title Extract first n groups in data frame
#' @param data dataframe
#' @param grps grouping variable
#' @param n number of groups to extract
#'
#' @return filtered dataset including only n first groups
#' @export
#'
#'
groupR <- function(data, grps, n) {
  data %>% group_by({{grps}}) %>%
    filter(cur_group_id() <= n)
}


#' @title Get the mode (most common value) of a vector.
#' @param x Vector of values
#' @param ties If two or more values are equally common, which should be chosen. Default is first.
#' @param na.rm Whether NAs should be removed, defaults to TRUE
#'
#' @return The most common value of the vector excluding NAs
#' @export
#'
# nums1 <- c(1,1,2,3)
# nums2 <- c(1,2,3,3)
# nums3 <- c(1,2,3)
# char1 <- c("first", "first", "middle", "last")
# char2 <- c("first", "first", "middle", "last", "last")
# char3 <- c("first", "middle", "last", "hepto")
# char4 <- c("first", "middle", "last")
#
# mode(nums1)
# mode(nums2)
# mode(nums3, "last")
# mode(char1, "last")
# mode(char2, "first")
# mode(char3, "last")


modeR <- function(x, ties = "first", na.rm=T) {
  ties <- match.arg(ties, c("first", "last"))

  if(na.rm) {
    ux <- unique(x[!is.na(x)])
  } else {
    ux <- unique(x)
  }

  ux <- ux[which(tabulate(match(x, ux)) == max(tabulate(match(x, ux))))]

  switch(ties,
         "first" = {mode <- ux[1]},
         "last" = {mode <- tail(ux, n=1)}
  )
  return(mode)
}

#' @title Start a multisession with automatic reset
#'
#' @param cores number of cores/workers
#'
#' @returns starts a multisession and reverts to sequential plan on.exit of the parent function (outer function)
#' @export
#'

multitaskR <- function(cores) {
  # current plan
  current <- future::plan()

  future::plan(future::multisession, workers = cores)

  # Revert to original plan when outer function exits
  do.call(on.exit,
          list(substitute(future::plan(current)),
               add = TRUE),
          envir = parent.frame())
}

#' @title Format numeric vectors
#' @param numbers numeric value or vector for formatting
#' @param digits number of digits
#' @param nsmall number of zero-digits
#' @param ama whether the numbers should be printed according to AMA guidelines (no digits on values >= 10). Default = F.
#'
#' @return returns a vector of same lentgh with formatted digits
#' @export
#'
#' @examples
#' numbR(c(5,2,4,10,100, 41.2), ama=F)
#' numbR(c(5,2,4,10,100, 41.2), ama=T)
#'

numbR <- function(numbers, digits = 1, nsmall, ama = F) {
  if(missing(nsmall)) {
    nsmall <- digits
  }

  vals <- format(round(numbers, digits), nsmall = nsmall)



  if(ama) {

    vals <- ifelse(as.numeric(str_extract(vals, "\\d+\\.\\d*")) >= 10, str_replace(vals, "\\d{2,}\\.\\d*", as.character(round(as.numeric(str_extract(vals, "\\d{2,}\\.\\d*")),0))), vals)

  }

  vals

}


#' @title Assign rolling ID.
#' @description
#' Useful after sorting a dataset, which changes the ID order. Works with piping
#'
#'
#' @param data dataset
#' @param vars column(s) which unique ids should be based on
#' @param label label for new unique id column
#' @param dt whether a data.table should be returned
#'
#' @return adds a new column to the dataset with unique ids based on original id conditional on a sorting
#' @export
#'
#'
# data.frame(id=c(1,1,1,1,2,2,2,3,3,3,4,4,4,4),
#            x=c(5,5,6,6,7,8,9,1,2,1,4,1,2,4)) %>%
#   arrange(x) %>%
#   rollR()


rollR <- function(data, vars = id, label = order, dt = F) {

  vars <- names(dplyr::select(data, {{ vars }}))
  label_name <- deparse(substitute(label))

  if(dt) {
    return(as.data.table(data)[, (label_name) := .GRP, by = vars])
  } else {
    return(as.data.table(data)[, (label_name) := .GRP, by = vars] %>% as.data.frame)
  }
}

#' @title Format p-values to AMA manual of style
#' @param x A p-value
#' @param na the print of NA values, default = "NA.
#' @return Prints the raw p-value according to AMA manual of style
#' @export

pvertR <- function(x, na = "NA") {
  sapply(x, function(x) {
    if(is.na(x) | str_detect(x, "\\d", negate=T)){
      return(na)
    }

    if(is.character(x)) {
      if(str_detect(x, "\\<\\s?0.001")) {
        return("p < 0.001")
      } else {
        x <- as.numeric(x)
      }
    }

    if(x<0.001){
      y = "p < 0.001"
    }
    else if(x < 0.01){
      y = paste0("p = ", numbR(x,3,2))
    }
    else if(x>=0.045 & x < 0.05){
      y = paste0("p = ", numbR(x,3,2))
    }
    else if(x>=0.01 & x<1){
      y = paste0("p = ", numbR(x,2,2))
    }
    else if(x>=1){
      y = paste0("p = 1.00")
    }
    else {
      y = na
    }
    return(y)
  })
}

#' @title First timestamp for taking time
#' @description
#' tickR starts the clock by adding the timestamp "start" to global environment
#'
#'
#' @return A timestamp
#' @export
#'
#'

tickR <- function() {

  tickR.start <<- Sys.time()

  paste0(lubridate::round_date(Sys.time(), "second"), "\n")

}

#' @title Last timestamp for taking time
#'
#' @description
#' tockR stops the clock and prints either a date/time or a time difference
#'
#'
#' @param format Whether date and time or a time difference should be returned
#' @param digits Number of digits on time difference
#'
#' @return Date/time or time difference since tickR()
#' @export
#'
#'

tockR <- function(format = "diff", start, digits = 2) {

  if(format == "time") {

    out <- paste0(lubridate::round_date(Sys.time(), "second"))

  } else if(format == "diff") {

    if(!missing(start)) {

      t <- Sys.time() - start
    } else {

      t <- Sys.time() - tickR.start
    }



    out <- paste0(round(as.numeric(t), digits), " ", attr(t, "units"))


  }


  out

}
