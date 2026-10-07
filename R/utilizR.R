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
#' @param cpr name of the cpr-column
#' @param extract TRUE if age and date of birth should be extracted
#' @param remove.cpr whether invalid CPRs should be removed, default = F
#' @param return.cpr whether the invalid CPRs should be returned as a vector, default = F
#' @param dt whether the dataframe should be returned as a data.table
#'
#' @return Returns same dataset with correct CPR numbers and optionally age and date of birth. Invalid CPRs stops the function and returns the invalid CPRs as a vector.
#' @export
#'

# tdf <- data.table(cpr = c("010169-2234",
#                           "0101012234",
#                           "9999999999",
#                           "101113235",
#                           "231023",
#                           "210434-4529",
#                           "010203-1AB2",
#                           NA),
#                   test = 1)
#
# cpR(tdf, extract = T, return.cpr = F, dt = T, remove.cpr = T) %>% print
#
# cpR(tdf$cpr, extract = F, return.cpr = F, remove.cpr = F) %>% print


cpR <- function(data, cpr = cpr, extract = FALSE, remove.cpr = FALSE,
                return.cpr = FALSE, dt = NULL) {

  is_tbl <- is.data.frame(data)
  #Original class
  if(is.null(dt)) dt <- is.data.table(data)

  if(is_tbl) {
    cpr_c <- defusR(cpr)
    raw   <- data[[cpr_c]]
  } else {
    raw <- data
  }

  cpr <- as.character(raw)
  #Drop hyphen and add leading zero
  cpr <- str_replace(cpr, "^(\\d{5,6})-(\\d\\w{2}\\d)$", "\\1\\2")
  cpr <- str_replace(cpr, "^(\\d{5}\\d\\w{2}\\d)$", "0\\1")
  #Returns NA if not 10 digits
  cpr <- str_extract(cpr, "^\\d{6}\\d\\w{2}\\d$")

  sex <- if(extract) fifelse(as.integer(str_sub(cpr, 10L, 10L)) %% 2L == 0L, "F", "M")

  yy <- as.integer(str_sub(cpr, 5L, 6L))
  d7 <- as.integer(str_sub(cpr, 7L, 7L))

  century <- fifelse(d7 <= 3L, 1900L,
                     fifelse(d7 %in% c(4L, 9L),
                             fifelse(yy <= 36L, 2000L, 1900L),
                             fifelse(yy <= 57L, 2000L, 1800L)))

  birth <- as.Date(paste0(century + yy, str_sub(cpr, 3L, 4L), str_sub(cpr, 1L, 2L)),
                   format = "%Y%m%d")

  cpr[is.na(birth)] <- NA_character_
  check <- !is.na(cpr)

  if (return.cpr) {
    error <- raw[!check]
    if (length(error)) cli::cli_alert_info("{length(error)} CPR(s) returned")
    return(error)
  }

  if (!is_tbl) {
    out <- if(extract) data.table(cpr, sex, birth) else cpr
    if (remove.cpr) out <- out[check]
    return(out)
  }

  # --- data.frame / data.table input --------------------------------------
  out <- if(is.data.table(data)) copy(data) else as.data.table(data)
  set(out, j = cpr_c, value = cpr)
  if (extract) {
    set(out, j = "birth", value = birth)
    set(out, j = "sex",   value = sex)
  }
  if (remove.cpr) out <- out[check]

  if (dt) out else as.data.frame(out)
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

multitaskR <- function(cores, gb = NULL) {

  #Increase gb in future.globals
  if(!is.null(gb)) {

    options(future.globals.maxSize = gb * 1024^3)

  }

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
#' @param trim whether the leading padding should be trimmed (default = T)
#' @param sign whether the number should be rounded using significant figures specified using the digits argument (defualt = F)
#'
#' @return returns a vector of same lentgh with formatted digits
#' @export
#'
#' @examples
#' numbR(c(5,2,4,10,100, 41.2), ama=F)
#' numbR(c(5,2,4,10,100, 41.2), ama=T)
#'

numbR <- function(numbers, digits = 1, nsmall, ama = F, trim = T, sign = F) {
  if(missing(nsmall)) {
    nsmall <- digits
  }

  if(sign) return(signif(numbers, digits))

  if(ama) ama_digit <- 0 else ama_digit <- digits

  numbers <- ifelse(numbers > 10, format(round(numbers, ama_digit), nsmall = ama_digit), format(round(numbers, digits), nsmall = nsmall))

  if(trim) numbers <- str_trim(numbers)

  return(numbers)

}




#' @title Format p-values to AMA manual of style
#' @param x vector of p-values
#' @param na the print of NA values, default = "NA.
#' @param drop.p whether "p = " should be printed (default = F)
#' @param drop.zero whether the leading zero should be printed (default = F)
#' @param trim whether white spaces should be removed (default = F)
#' @param style style of the formatting (default = "ama")
#' @return Prints the raw p-value according to AMA manual of style
#' @export

# vals <- c(0.0005, 0.002, 0.03, 0.0491, 0.051, 2, NA)
# pvertR(vals,
#        trim = F,
#        drop.p = F,
#        drop.zero = T)

pvertR <- function(pval,
                   na = "NA",
                   drop.p = F,
                   drop.zero = F,
                   trim = F,
                   style = "ama") {

  if(is.character(pval)) {
    pval <- case_when(is.na(pval) | str_detect(pval, "\\d", negate=T) ~ NA,
                      str_detect(pval, "\\<\\s?0.001") ~ 0.000000001,
                      T ~ as.numeric(str_extract_all(pval, "\\d.*")))
  }

  if(style == "ama") {

    p_val <- case_when(is.na(pval) ~ na,
                       pval < 0.001 ~ "p < 0.001",
                       pval < 0.01 | (pval >= 0.045 & pval < 0.05) ~ paste0("p = ", numbR(pval, 3, 3)),
                       pval >= 0.99 ~ "p > 0.99",
                       T ~ paste0("p = ", numbR(pval, 2, 2)))

  }

  if(style == "lancet") {

    lancet_num <- function(x) {
      x   <- numbR(x, digits = 2, sign = T)
      dec <- pmin(1 - floor(log10(x)), 4)
      sprintf("%.*f", dec, x)

    }

    p_val <- case_when(
      is.na(pval)   ~ na,
      pval < 0.0001 ~ "p < 0.0001",
      pval >= 0.99 ~ "p > 0.99",
      TRUE          ~ paste0("p = ", lancet_num(pval))
    )

  }



  if(trim) p_val <- str_remove_all(p_val, "\\s")
  if(drop.p) p_val <- str_remove(p_val, "p.*.(?=(\\d\\.))")
  if(drop.zero) p_val <- str_remove(p_val, "0(?=(\\.))")


  return(p_val)
}


#' @title First timestamp for taking time
#' @description
#' tickR starts the clock by adding the timestamp "start" to global environment
#'
#' @param print whether the current time should be printet (default = F)
#' @param cli whether the output should be as cli_text (default = T)
#'
#' @return A timestamp
#' @export
#'
#'

tickR <- function(cli=T, print = F) {

  tickR.start <<- Sys.time()

  if(print) {

    out <- paste0(lubridate::round_date(Sys.time(), "second"))

    if(cli) cli::cli_text(out) else out

  }

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

#MANUAL FOR INSIDE FUTURE
# TickR: tickR.start <- Sys.time()
# paste0(lubridate::round_date(Sys.time(), "second"))
# TockR: paste0(round(as.numeric(Sys.time() - tickR.start), 2), " ", attr(Sys.time() - tickR.start, "units"))
# TockR: paste0(round(as.numeric(Sys.time() - tickR.start), 2), \' \', attr(Sys.time() - tickR.start, \'units\')) (cli)

tockR <- function(format = "diff", start, digits = 2, cli = T) {

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


  if(cli) cli::cli_text(out) else (out)

}


