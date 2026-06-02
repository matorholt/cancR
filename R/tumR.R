#' Map unique tumors
#'
#' @description
#' Customized function for the Danish Pathology Register to map unique tumors taking reexcisions and changes in diagnosis into account.
#'
#' @param data dataframe of data from the Danish National Pathology Register
#' @param tumor character vector containing prefixes for tumors of interest (e.g. m807)
#' @param loc.exact whether tumors should match on exact location to be considered linked
#' @param cores number of cores for parallel processing
#' @param dt whether a data.table should be returned (default = F)
#'
#' @returns a data.frame of unique tumors with date, diagnosis and location
#' @export
#'
#'

# t_codes <- readR("C:/Users/mnie0985/Desktop/t.codes.xlsx")
#
# pato <- tribble(
#   ~ id, ~ date,   ~ snomed,
#   #Three unique tumors and one
#   1, "2012-01-01", "T02120 M80703 M84003",
#   1, "2012-02-01", "T02121 M80704 M80003",
#   1, "2013-04-04", "T0220B M80903",
#   1, "2013-05-04", "T0220B M87003",
#   1, "2016-01-01", "T0282D TY9000 M80703",
#   1, "2017-01-01", "T0282C M80703",
#   2, "2012-01-01", "T0282C M87003",
#   3, "2012-01-01", "T0282C M87003",
#   4, "2013-04-04", "T0220B M80903",
#   4, "2013-05-01", "T0220B M87003"
# ) %>%
#   datR(date)
#
# df <- tumR(pato, tumor = c("m807", "m809", "m80", "m87", "m84"))

tumR <- function(data, tumor, loc.exact = F, cores = NULL, dt=F) {

  on.exit({
    cli::cli_h3("Tumor Mapping complete!")
    cli::cli_text("Total runtime:")
    cli::cli_text(tockR("diff", start))
  })

  cli::cli_h2("Initializing tumR algorithm: {tickR(cli=F)}")

  start <- tickR.start

  if(loc.exact) loc.exact <- "exact" else loc.exact <- "cluster"

  #Extract prefixes for tumors ending on 3,4,9,x
  tumor_regex <- paste0(map_chr(tumor, ~ paste0(.x, "(?=(", paste0(rep(".", 5-str_count(.x)), collapse=""), "[349x]))")), collapse = "|")


  if(all(class(data) %nin% "data.table")) setDT(data)

  dat <- copy(data)

  dat[, snomed := str_to_lower(snomed)]

  dat[, `:=`(
    t.code = str_extract_all(snomed, "\\bt.{5}"),
    tumor  = str_extract_all(snomed, tumor_regex)
  )]

  dat[, exact := map(t.code, ~ {

    #If more than 1 t-code
    if(length(.x) > 1) {
      #Find
      locs <- map_chr(.x, ~ t_codes$loc_spec[t_codes$t.code == str_to_upper(.x)])
      #Specific location?
      #lspec <-
      if("specific" %in% locs) {
        return(.x[which(locs == "specific")])}

    }

    .x

  })]

  dat[, cluster := map(t.code, ~
                           str_trim(
                             unique(
                               unlist(
                                 str_split(
                                   paste0(
                                     map_chr(.x, ~ t_codes$cluster[t_codes$t.code == str_to_upper(.x)]), collapse = ","), ","))))


  )]

  for(i in c("localisation", "loc_spec", "loc_skin", "region")) {

    dat[,(i) := map_chr(exact, ~ t_codes[[i]][t_codes$t.code == str_to_upper(.x)])]

  }

  #Parallellisation and progress bar
  if(!inherits(plan(), "multisession") & !is.null(cores)) {
    multitaskR(cores = cores)
    dat_list <- listR(split(dat, by = "id"), type = "chunk_outer", chunks = cores)
  } else {
    dat_list <- list(split(dat, by = "id"))
  }

  progressr::handlers(global = TRUE)
  progressr::handlers("cli")
  options(cli.progress_bar_style = "fillsquares")

  p <- progressr::progressor(along = seq_along(dat_list))


  ##########
  #  MAIN  #
  ##########

  main_out <- rbindlist(future_map(seq_along(dat_list), ~ {

    tickR.start <- Sys.time()

  tumor_out <-rbindlist(map(dat_list[[.x]], function(y) {


  tumor_frame <- data.table()

  for(x in 1:nrow(y)) {


    dfx <- y[x,]
    tumor_x <- unlist(dfx$tumor)
    loc_x <- unlist(dfx[[loc.exact]])
    date_x <- dfx$date
    spec_x <- dfx$loc_spec

    add <- 0

    #Allocate first tumor to tumor_frame and move on
    if(x == 1) {
      tumor_frame <- dfx
      next
    }

    #Inner loop
    for(i in 1:nrow(tumor_frame)) {

      Sys.sleep(1)

      tfx <- tumor_frame[i, ]
      tumor_i <- unlist(tfx$tumor)
      loc_i <- unlist(tfx[[loc.exact]])
      date_i <- tfx$date
      diff <- as.numeric(date_x - date_i)
      spec_i <- tfx$loc_spec

      #T=T
      if(any(tumor_x %in% tumor_i)) {

        #L=L
        if(any(loc_x %in% loc_i)) {

          #Non-specific primary location < 90 - update location
          if(spec_x == "specific" & spec_i == "non-specific" &  diff < 90) {

            loc_cols <- c("cluster", "localisation", "loc_spec", "loc_skin", "region")

            tumor_frame[i, c(loc_cols) := dfx[, c(loc_cols), with = FALSE]]
        }
          #L!=L
        } else {
          #Add tumor
          add <- add + 1
        }
        #T!=T
      } else {


        #Same location, <30 - Change diagnosis
        if(diff < 30 & any(loc_x %in% loc_i)) {

          tum_cols <- c("tumor", "snomed")

          tumor_frame[i, c(tum_cols) := dfx[, c(tum_cols), with = FALSE]]

        } else {
          #Add tumor
          add <- add + 1

        }
      }
    } #tumor_frame

    if(add == nrow(tumor_frame)) {
      tumor_frame <- rbind(tumor_frame, dfx)
    }


  } #rows

  #return tumor_frame
  tumor_frame

  })) #patient

    p(message = paste0("Chunk", .x, "/", length(dat_list),
                       " Completed ", paste0(lubridate::round_date(Sys.time(), "second")), " Runtime: ",
                       paste0(round(as.numeric(Sys.time() - tickR.start), 2), " ", attr(Sys.time() - tickR.start, "units"))))

    #Rbind all patients
    tumor_out

  }))[, c("snomed", "t.code", "exact", "cluster", "loc_spec", "loc_skin") := NULL] #future

  if(dt) return(main_out) else return(as.data.frame(main_out))
}

