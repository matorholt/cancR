#' Map unique tumors
#'
#' @description
#' Customized function for the Danish Pathology Register to map unique tumors and outcomes taking reexcisions and changes in diagnosis into account.
#'
#' @param data dataframe of data from the Danish National Pathology Register
#' @param tumor named list of tumors with vectors of snomed codes (e.g. list("pcc" = c("m0703", "m805"), "bcc" = "m809"))
#' @param loc.exact whether tumors should match on exact location to be considered linked
#' @param id name of the main identifier, default = pnr
#' @param dt whether a data.table should be returned (default = F)
#' @param verbose whether detailed status for each patient should be printed (for debugging)
#' @param tumor_distance the interval where a metastasis is considered relevant
#' @param meta_distance the distance between metastases that classifies a cluster
#'
#' @returns a data.frame of unique tumors with date, diagnosis and location
#' @export
#'
#'

# pato <- tribble(
#   ~ record_id, ~ index,   ~ snomed,
#   #Three unique tumors and one
#   1, "2012-01-01", "T02120 M80703 M84003",
#   1, "2012-02-01", "T02121 M80704 M80003",
#   1, "2012-08-31", "T29030 M80706",
#   1, "2013-04-04", "T0220B M80903",
#   1, "2013-05-04", "T0220B M87003",
#   1, "2013-06-04", "T29030 M80906",
#   1, "2016-01-01", "T0282D TY9000 M80703",
#   1, "2017-01-01", "T0282C M80703",
#   #Single tumor
#   2, "2012-01-01", "T0282C M87003",
#   #One skin cancer, one competing cancer and three metastases
#   3, "2012-01-01", "T0282C M80703",
#   3, "2015-01-01", "T29030 M80703" ,
#   3, "2016-01-01", "T29030 M80706" ,
#   3, "2016-03-01", "T0848F M80706",
#   3, "2016-04-01", "T29031 M80706",
#   #One skin cancer, change in diagnosis BCC -> MM
#   4, "2013-04-04", "T0220B M80903",
#   4, "2013-05-01", "T0220B M87003",
#   #One skin cancer, change in diagnosis UNS -> Sarcoma + UNS recurrence
#   5, "2001-01-01", "T0220B M88003",
#   5, "2001-03-01", "T0220B M88903",
#   5, "2005-01-01", "T0220B M88003",
#   #One skin cancer, specific to unspecific shift
#   6, "2001-01-01", "T0282C M80503",
#   6, "2001-02-01", "T0282C M80703",
#   #Different sarcomas to test m88 vs. m8830, m8832, m889
#   7, "2001-01-01", "T0282C M88303",
#   7, "2003-02-01", "T0220B M88323",
#   7, "2006-02-01", "T02120 M88903",
#   7, "2009-02-01", "T02120 M88933",
#   # Skin+Ear < 90
#   8, "2001-01-01", "T01000 M80503",
#   8, "2001-02-01", "T0220B M80503",
#   # Skin+Ear > 90
#   9, "2001-01-01", "T01000 M80503",
#   9, "2003-02-01", "T0220B M80503",
#   # Ear+Skin < 90
#   10, "2001-01-01", "T0220B M80503",
#   10, "2001-02-01", "T01000 M80503",
#   # Ear+Skin > 90
#   11, "2001-01-01", "T0220B M80503",
#   11, "2003-02-01", "T01000 M80503",
#   # Skin+Skin < 90 - BCC
#   12, "2001-01-01", "T01000 M80903",
#   12, "2001-02-01", "T01000 M80903",
#   # Skin+Skin > 90 - BCC
#   13, "2001-01-01", "T01000 M80903",
#   13, "2003-02-01", "T01000 M80903",
#   # Skin+Skin < 90 - PCC
#   14, "2001-01-01", "T00100 M80703",
#   14, "2001-02-01", "T01000 M80703",
#   # Skin+Skin > 90 - PCC
#   15, "2001-01-01", "T01000 M80703",
#   15, "2003-02-01", "T01000 M80703",
#   # Ear+leg+Skin > 90 - PCC
#   16, "2001-01-01", "T0220B M80703",
#   16, "2003-02-01", "T02121 M80703",
#   16, "2005-02-01", "T01000 M80703",
#   # Only mets - out
#   17, "2001-01-01", "T0220B M80706",
#   17, "2003-02-01", "T02121 M80706",
#   17, "2005-02-01", "T01000 M80706",
#   # BCC and metastasizing PCC. Change in first metastasis code from BCC to PCC. Multiple coupled metastasis + one late
#   18, "2000-01-01", "T0220B M80903",
#   18, "2001-01-01", "T0220B M80703",
#   18, "2003-01-01", "T29030 M80906",
#   18, "2003-02-01", "T29030 M80706",
#   18, "2003-02-05", "T08000 M80706",
#   18, "2004-01-01", "T02121 M80706",
#   18, "2004-02-01", "T0220B M80706",
#   18, "2003-03-02", "T10501 M80706",
#   18, "2006-03-02", "T10503 M80706",
#   18, "2015-01-01", "T02121 M80703",
#   # Patient with two primary PCCS and unallocable met
#   19, "2001-01-01", "T0220B M80703",
#   19, "2001-02-01", "T02121 M80703",
#   19, "2001-07-01", "T0810S M80706",
#   #Competing T-codes for both primary and lymph node
#   20, "2001-07-01", "T0220B M80703",
#   20, "2002-07-01", "T0220B M80706",
#   20, "2003-06-01", "T0810S TY0100 T28500 M80706"
# )
# #
# tumor_list <- list("pcc" = c("m807", "m805"),
#      "bcc" = "m809",
#      "mm" = "m87",
#      #"sarcoma" = "m88",
#      "ups" = "m8830",
#      "dfsp" = "m8832",
#      "lms" = "m889[02]"
#      )
#
# test <- tumR(#pato %>% filter(record_id == 20),
#   pato,
#   tumor = tumor_list,
#   verbose = F,
#   loc.exact = F,
#   pnr = record_id,
#   date = index)

tumR <- function(data,
                 tumor,
                 loc.exact = F,
                 pnr = pnr,
                 date = date,
                 dt=F,
                 verbose = F,
                 tumor_distance = c(-90, 365.25*5),
                 meta_distance = 365.25 * 2,
                 exclude = NULL) {

  cli::cli_h2("Initializing tumR algorithm: {tickR(print=T, cli=F)}")

  start <- tickR.start

  on.exit({
    cli::cli_h3("Tumor Mapping complete!")
    cli::cli_text("Total runtime:")
    cli::cli_text(tockR("diff", start))
  })

  #Return DT if input is DT and dt is not specified
  if(is.data.table(data) & missing(dt)) dt <- T

  pnr_c <- defusR(pnr)
  date_c <- defusR(date)

  if(loc.exact) loc.exact <- "localisation" else loc.exact <- "cluster"

  ##############################################  Custom functions  ##############################################
  #Add UNS codes
  add_uns <- function(codes) unique(map_chr(codes, ~ paste0(str_extract(.x, ".{3}"), "0")))
  #Convert to regex
  add_regex <- function(codes, suffix) paste0(map_chr(codes, ~ paste0(.x, paste0(rep(".", 5-str_count(str_remove_all(.x, "(?<=(\\[)).+]"))), collapse=""), suffix)), collapse = "|")
  #Collapse multiple to regex
  collapsR <- function(input, sublist) paste0(unlist(map(input, ~ .x[[sublist]])), collapse= "|")
  #Extract t.codes
  t.extract <- function(input, var) t_codes[[defusR(var)]][match(str_to_upper(as.character(input)), t_codes$t.code)]
  #Map t.codes
  t.map <- function(input, type, verbose = F) {

    map(input, ~ {

      t.code <- .x
      localisation <- t.extract(.x, localisation)
      spec <- t.extract(.x, loc_spec)
      skin <- t.extract(.x, loc_skin)

      if(type == "primary") vec <- pmap_vec(list(spec == "specific",
                                                 skin == "skin",
                                                 str_detect(.x, "ty", negate = T)), sum)

      if(type == "meta") vec <- pmap_vec(list(spec == "specific",
                                              str_detect(.x, "ty", negate = T)), sum)

      if(verbose) {
        exact <- t.extract(.x, exact)

        cli::cli_h1("")
        cli::cli_text("T-code: {t.code}")
        cli::cli_text("Exact: {exact}")
        cli::cli_text("Localisation: {localisation}")
        cli::cli_text("Specific: {spec}")
        cli::cli_text("Skin: {skin}")
        cli::cli_text("Points: {tab}")
      }

      #If T01000 (skin) non-skin codes are converted to first corresponding skin code
      if("t01000" %in% .x & all(str_detect(vec, "t02",negate=T))) return(t_codes$t.code[t_codes$localisation %in% localisation[localisation %nin% "skin"] & t_codes$loc_skin %in% "skin"][1])

      return(.x[vec == max(vec)])

    })

  }

  ##############################################  Prepare data  ##############################################

  tumor_map <- map(tumor, ~ {

    tumor <- .x
    uns <- add_uns(.x)
    tumor_uns <- c(tumor, uns)
    tumor_regex <- add_regex(.x, "[3479x]")
    tumor_uns_regex <- add_regex(tumor_uns, "[3479x]")
    meta_regex <- add_regex(.x, "[6]")
    meta_uns_regex <- add_regex(tumor_uns, "[6]")

    lst(tumor, uns, tumor_uns, tumor_regex, tumor_uns_regex, meta_regex, meta_uns_regex)
  })


  if(is.null(exclude)) {

    exclude <- c("m801[2-9]", "m804",
                 "m81[2-9]",
                 "m820[13]", "m82[1-35-9]",
                 "m83[0-2]", "m833[3-9]", "m83[4-8]",
                 "m85",
                 "m86")

  }

  exclude_c <- paste0(defusR(exclude), collapse = "|")

  #Labs for recodR (first specified tumors, then UNS alternatives)
  labs <- unlist(map(tumor_map, ~ .x$uns))

  labs <- labs %>%
    split(labs) %>%
    map(., ~ list(unique(.x)) %>% set_names(paste0(c(names(.x), "uns"), collapse = "_"))) %>% flatten

  labs <- append(tumor, labs)

  #Cleaning and extraction
  cli::cli_progress_message("Extraction:")
  tickR()

  if(all(class(data) %nin% "data.table")) setDT(data)

  dat <- copy(data)

  if(class(dat[[date_c]]) != "date") dat[[date_c]] <- datR(dat[[date_c]])

  #Split data at T-codes and ;
  dat <- dat[, snomed := str_to_lower(snomed)] %>%
    .[, .(snomed = unlist(str_split(snomed, "(?<=((?<=(t.{0,50}))[fjmpsæ].{5})).(?=(t))|;"))), by = c(pnr_c, date_c)] %>%
    .[str_detect(snomed, collapsR(tumor_map, "tumor_uns"))]

  if(nrow(dat) == 0) return({cli::cli_alert_danger("Error: No tumors found, check argument tumor"); invisible(NULL)})

  dat[, `:=`(
    t.code = str_extract_all(snomed, "\\bt.{5}"),
    tumor  = str_extract_all(snomed, collapsR(tumor_map, "tumor_uns_regex")),
    meta = str_extract_all(snomed, collapsR(tumor_map, "meta_uns_regex"))
  )] %>%
    recodR(list(tumor = labs,
                meta = labs), dt=T, match = "contains")

  #extract exact t-codes
  dat[, t.exact := t.map(t.code, type = "primary")]
  dat[, met.exact := t.map(t.code, type = "primary")]

  dat[, cluster := map(t.code, ~
                         str_trim(
                           unique(
                             unlist(
                               str_split(
                                 paste0(
                                   map_chr(.x, ~ {
                                     t_codes$cluster[match(str_to_upper(.x), t_codes$t.code)]
                                   }), collapse = ","), ","))))


  )]


  for(i in c("exact", "localisation", "loc_spec", "loc_skin", "region", "depth")) {

    dat[, (i) := lapply(t.exact, function(x) na.omit(unique(t.extract(x, var = i))))]

  }

  #If non-skin diagnosis code is in snomed, change to non-skin
  dat[, loc_skin := ifelse(unlist(loc_skin) %nin% "skin" & str_detect(snomed, exclude_c), "non-skin", loc_skin)]

  dat[, op_date := as.Date(NA)]
  dat[, recurrence_date := as.Date(NA)]

  dat_list <- split(dat, by = pnr_c)

  cli::cli_alert_success("Extraction: Complete {tockR(\'time\', cli=F)}, Runtime = {tockR(cli=F)}")


  cli::cli_progress_message("Tumor loop:")
  tickR()

  #Parallellisation and progress bar
  pb <- cli::cli_progress_bar(
    format = "{cli::pb_spin} Patients: {cli::pb_current}/{cli::pb_total} ({cli::pb_percent} ) ETA: {cli::pb_eta} Rate {cli::pb_rate}",
    total = length(dat_list),
    clear = T)

  ##############################################  MAIN  ##############################################


  main_out <-
    map(seq_along(dat_list), function(y) {

      cli::cli_progress_update(id = pb, set = y)

      if(verbose) cli::cli_h1("Patient: {y}")

      dat_y <- dat_list[[y]][lengths(tumor) > 0]

      ##############################################  TUMORS  ##############################################


      tumor_frame <- data.table()
      meta_frame <- data.table()


      if(nrow(dat_y) == 0) {
        if(verbose) cli::cli_alert_info("No primary tumors detected")
        return(tumor_frame)
      }

      #Outer loop
      for(x in 1:nrow(dat_y)) {

        dfx <- dat_y[x,]
        tumor_x <- unlist(dfx$tumor)
        loc_x <- unlist(dfx$localisation)
        tcode_x <- unlist(dfx$t.code)
        date_x <- dfx[[date_c]]
        spec_x <- dfx$loc_spec
        skin_x <- dfx$loc_skin

        add <- 0
        inc <- 0

        if(verbose) {
          cli::cli_h2("Index {x}")
          cli::cli_text("Tumor: {tumor_x}")
          cli::cli_text("Location: {loc_x} ({tcode_x})")
          cli::cli_text("Date: {as.character(date_x)}")
          cli::cli_text("Type: {as.character(skin_x)}")
          cli::cli_text("Spec: {as.character(spec_x)}")
        }

        #Allocate first tumor to tumor_frame and move on
        if(x == 1) {
          if(verbose) cli::cli_alert_info("Tumor added")
          tumor_frame <- dfx
          next
        }

        #Inner loop
        for(i in 1:nrow(tumor_frame)) {


          tfx <- tumor_frame[i, ]
          tumor_i <- unlist(tfx$tumor)
          loc_i <- unlist(tfx[[loc.exact]])
          tcode_i <- unlist(dfx$t.code)
          date_i <- tfx[[date_c]]
          diff <- as.numeric(date_x - date_i)
          spec_i <- tfx$loc_spec
          skin_i <- tfx$loc_skin
          unspec <- skin_x %in% skin_i & "non-specific" %in% c(spec_x, spec_i)

          if(verbose) {
            cli::cli_h3("Tumor {i}")
            cli::cli_text("Tumor: {tumor_i}")
            cli::cli_text("Location: {loc_i} ({tcode_x})")
            cli::cli_text("Date: {as.character(date_i)}")
            cli::cli_text("Type: {as.character(skin_i)}")
            cli::cli_text("Spec: {as.character(spec_i)}")
            cli::cli_text("T=T: {any(str_detect(tumor_i, tumor_x) | str_detect(tumor_x, tumor_i))}")
            cli::cli_text("L=L: {any(loc_x %in% loc_i)}")
            cli::cli_text("Unspec: {unspec}")
            cli::cli_text("Diff: {diff}")
          }



          #T=T
          #Reverse also to capture UNS-strings after primary specific
          if(str_detect(tumor_i, tumor_x) | str_detect(tumor_x, tumor_i)) {

            if(any((is.na(loc_x) | loc_x == poopNApoop) | loc_x == "skin") & any((is.na(loc_i) | loc_i == poopNApoop | loc_i == "skin"))) {


              if(tumor_x == "bcc" | diff > 90) {

                if(verbose) cli::cli_alert_info("BCC kept")
                tumor_frame <- rbind(tumor_frame, dfx)
                inc <- inc + 1
                next()
              } else {
                next()
              }
            }

            #L=L
            if(any(loc_x %in% loc_i) |
               #If any of the codes are non-specific but same type (skin or non-skin)
               unspec) {


              if(diff < 90) {

                #Non-specific primary location < 90 - update location
                if(spec_x == "specific" & spec_i == "non-specific") {
                  if(verbose) cli::cli_alert_info("Location updated")
                  loc_cols <- c("cluster", "exact", "localisation", "loc_spec", "loc_skin", "region")

                  tumor_frame[i, c(loc_cols) := dfx[, c(loc_cols), with = FALSE]]

                  inc <- inc + 1
                }

                if(str_detect(tumor_i, "uns") & str_detect(tumor_x, "uns", negate=T)) {
                  if(verbose) cli::cli_alert_info("Diagnosis updated")
                  tum_cols <- c("tumor", "snomed")

                  tumor_frame[i, c(tum_cols) := dfx[, c(tum_cols), with = FALSE]]

                  inc <- inc + 1
                }

                #Update OP-date
                tumor_frame[i, op_date := date_x]

              } else {

                #Recurrence if T=T, L=L and diff > 90. Unspecific code can give recurrence if not BCC.
                if(is.na(tumor_frame[i, recurrence_date]) & !(tumor_x == "bcc" & spec_x == "non-specific")) {

                  if(verbose) cli::cli_alert_info("Recurrence added")
                  tumor_frame[i, recurrence_date := date_x]

                  inc <- inc + 1

                }
              }

              #L!=L
            } else {
              #Add tumor
              add <- add + 1
              inc <- inc + 1
            }
            #T!=T
          } else {

            #Same location, <30 - Change diagnosis
            if(diff < 90 & any(loc_x %in% loc_i) & tumor_x != "bcc") {
              if(verbose) cli::cli_alert_info("Diagnosis updated")
              tum_cols <- c("tumor", "snomed")

              tumor_frame[i, c(tum_cols) := dfx[, c(tum_cols), with = FALSE]]
              inc <- inc + 1

            } else {
              #Add tumor
              add <- add + 1
              inc <- inc + 1
            }
          }
        } #Tumor inner loop

        if(add == nrow(tumor_frame)) {
          if(verbose) {
            cli::cli_alert_info("Tumor added")
          }
          tumor_frame <- rbind(tumor_frame, dfx)
        }

        if(inc == 0) if(verbose) cli::cli_alert_info("Tumor not included")


      } #Tumor outer loop

      ##############################################  METASTASES  ##############################################


      if(verbose) cli::cli_h2("Metastasis")

      mets <- dat_list[[y]][lengths(meta) > 0]

      #Only run for patients with mets
      if(nrow(mets) == 0) {
        if(verbose) cli::cli_alert_info("No metastases detected")
        return(list(tumors = tumor_frame,
                    mets = meta_frame))
      }

      #Outer loop
      for(x in 1:nrow(mets)) {

        mfx <- mets[x,]
        meta_x <- unlist(mfx$meta)
        loc_x <- unlist(mfx$localisation)
        date_x <- mfx[[date_c]]
        spec_x <- mfx$loc_spec

        add <- 0
        inc <- 0

        if(verbose) {
          cli::cli_h2("Index {x}")
          cli::cli_text("Metastasis: {meta_x}")
          cli::cli_text("Location: {loc_x}")
          cli::cli_text("Date: {as.character(date_x)}")
        }

        #Allocate first metastasis to meta_frame and move on
        if(x == 1) {
          if(verbose) cli::cli_alert_info("Metastasis added")
          meta_frame <- mfx
          next
        }

        #Inner loop
        for(i in 1:nrow(meta_frame)) {

          tfx <- meta_frame[i, ]
          meta_i <- unlist(tfx$meta)
          loc_i <- unlist(tfx[[loc.exact]])
          date_i <- tfx[[date_c]]
          diff <- as.numeric(date_x - date_i)

          if(verbose) {
            cli::cli_h3("Metastasis {i}")
            cli::cli_text("Tumor: {meta_i}")
            cli::cli_text("Location: {loc_i}")
            cli::cli_text("Date: {as.character(date_i)}")
            cli::cli_text("M=M: {any(str_detect(meta_i, meta_x) | str_detect(meta_x, meta_i))}")
            cli::cli_text("L=L: {any(loc_x %in% loc_i)}")
            cli::cli_text("Diff: {diff}")
          }

          #M=M
          if(any(str_detect(meta_i, meta_x) | str_detect(meta_x, meta_i))) {

            add <- add + 1
            inc <- inc + 1

          } else {

            if(any(loc_x %in% loc_i) & diff < 90) {

              if(verbose) cli::cli_alert_info("Diagnosis updated")
              meta_cols <- c("meta", "snomed")

              meta_frame[i, c(meta_cols) := mfx[, c(meta_cols), with = FALSE]]

              inc <- inc + 1

            }
          }

          if(add == nrow(meta_frame)) {
            if(verbose) {
              cli::cli_alert_info("Metastasis added")
            }
            meta_frame <- rbind(meta_frame, mfx)
          }

          if(inc == 0) if(verbose) cli::cli_alert_info("Metastasis not included")

        } #Mets inner loop

      } #Mets outer loop


      return(list(tumors = tumor_frame,
                  mets = meta_frame))

    })

  cli::cli_alert_success("Tumor mapping: Complete {tockR(\'time\', cli=F)}, Runtime = {tockR(cli=F)}")

  tumors <-
    rbindlist(map(main_out, ~ .x$tumors))[, c("meta", "snomed", "t.code", "t.exact", "loc_spec", "depth") := NULL][
      , op_date := fifelse(is.na(op_date), get(date_c), op_date)][, tumor := unlist(tumor)] %>%
    rollR(type = "count", by = pnr_c, order = c(pnr_c, date_c), label = t_id)

  renames <- c(date_c, "localisation", "region", "loc_skin")

  setnames(tumors,
           renames,
           paste0("t_", renames))

  mets <- map(main_out, ~ .x$mets)

  if(all(lengths(mets) == 0)) return(tumors)

  mets <- rbindlist(mets)[, .SD, .SDcols = c(pnr_c, date_c, "t.code", "meta", "localisation", "region", "loc_skin", "depth")][, meta := unlist(meta)] %>%
    rollR(type = "count", by = pnr_c, order = c(pnr_c, date_c), label = m_id) %>%
    rollR(type = "interval", by = pnr_c, label = m_fam, interval = c(meta_distance, Inf), vars = date_c)

  setnames(mets,
           renames,
           paste0("m_", renames))



  mt_frame <- joinR(tumors, mets, by = pnr_c)[, c("exact", "op_date", "recurrence_date") := NULL][, diff := as.numeric(get(paste0("m_", date_c)) - get(paste0("t_", date_c)))] %>%
    #Match on subtype and mets not preceeding tumor
    .[meta == tumor & diff > tumor_distance[1]] %>%
    #Point system:
    #ns: non-skin tumor within 5 years of metastasis wins
    #time: skin tumor within 5 years of metastasis wins
    #lr: exact localisation or skin/lymph in same region wins
    .[, `:=` (ns = ifelse(t_loc_skin == "non-skin" & diff %between% tumor_distance, 99, 0),
              time = ifelse(diff %between% tumor_distance, 1, 0),
              lr = ifelse(any(unlist(m_localisation) %chin% unlist(cluster)) | ((m_loc_skin %chin% "skin"|depth %chin% "lymph") & any(unlist(m_region) %chin% unlist(t_region))), 1, 0)
    )] %>%
    rowR(., vars = c(ns, time, lr), type = "sum") %>%
    #Link related metastases
    .[, sum := ifelse(sum != max(sum), max(sum), sum), by = c(pnr_c, "t_id", "m_fam")] %>%
    #Keep max for each tumor
    .[, .SD[sum == max(sum)], by = c(pnr_c, "m_id")] %>%
    #Remove mets allocated to non-skin cancers
    .[t_loc_skin != "non-skin"] %>% setorderv(c(pnr_c, "diff")) %>%
    #Keep tumor closest to met
    unique(by = c(pnr_c, "m_id")) %>%
    #Assign type of metastasis
   .[, local := ifelse(any(unlist(m_loc_skin) %chin% "skin") & any(unlist(m_region) == unlist(t_region)), 1, 0), by = c(pnr_c, "m_id")] %>%
    .[, regional := ifelse(any(unlist(depth) %chin% "lymph") & (any(unlist(m_region) == unlist(t_region)) | is.na(m_region)), 1, 0), by = c(pnr_c, "m_id")] %>%
    .[, distant := ifelse(any(unlist(m_region) != unlist(t_region)), 1, 0), by = c(pnr_c, "m_id")] %>%
    #Pivot longer
    melt(.,
         measure.vars  = c("local", "regional", "distant"),
         variable.name = "meta_type",
         value.name    = "value"
    ) %>%
   .[value == 1] %>%
    .[, .SD, .SDcols = c(pnr_c, paste0("m_", date_c), "t_id", "m_id", "m_localisation", "m_region", "m_loc_skin", "depth", "meta_type")]

  #If all metastases are ruled out
  if(nrow(mt_frame) == 0) return(tumors)


  #Pack all metastasis data into nested DTs
  mets_data <- mt_frame[, .(mets_data = list(.SD)), by = c(pnr_c, "t_id")]

  #Join Tumor, met_dates and nested DTs
  tm_frame <- mt_frame %>%
    setorderv(c(pnr_c, paste0("m_", date_c))) %>%
    #Get first meta-date for each tumor ID and meta_type
    unique(by = c(pnr_c, "t_id", "meta_type")) %>%
    # #pivot mets wider
    dcast(as.formula(paste0(pnr_c, "+ t_id ~ meta_type")), value.var = paste0("m_", date_c), fun.aggregate = function(x) if(length(x) == 0) NA else min(x)) %>%
    joinR(tumors, ., mets_data, by = c(pnr_c, "t_id")) %>%
    .[t_loc_skin != "non-skin",] %>%
    .[, c("cluster", "t_loc_skin") := NULL]

  setnames(tm_frame,
           paste0("t_", renames)[paste0("t_", renames) %in% names(tm_frame)],
           renames[paste0("t_", renames) %in% names(tm_frame)])

  if(dt) return(tm_frame) else return(as.data.frame(tm_frame))

}
