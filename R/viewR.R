#' viewR
#'
#' @description
#' Graphical overview of the codelist hierarchy
#'
#'
#' @param codelist A codelist of max depth 3
#'
#' @return graphical overview of the levels and groups in the codelist
#' @export
#'
#'

# codelist <- list("lpr_case" = list("abdomen" = list("kidney" = c("tx1a","tx1b","tx1c"),
#                                                     "liver" = c("tx2a", "tx2b", "tx2c")),
#                                    "thorax" = list("heart" = c("tx3a", "tx3b", "tx3c"),
#                                                    "lung" = c("tx4a", "tx4b", "tx3c"))),
#                  "lpr_ex" = list("immune_diag" = "a3",
#                                  "cll" = c("a4", "b4")),
#                  "lmdb_ex" = list("immune" = "a5"),
#                  "opr_ex" = list("trans" = "t5"),
#                  "pato_supp" = list("PCC" = "M80"),
#                  "labels" = list("lpr_case" = "SOTR",
#                                  "lpr_case_header" = "region",
#                                  "lpr_ex" = "immsup"),
#                  "exclusion" = c("z1","z2","z3"))
#
#
# decodR(codelist)



viewR <- function(codelist) {

  df <- rrapply::rrapply(codelist, how = "melt",
                         f = function(x) paste0(x, collapse = ",")) %>%
    rename(L4 = value) %>%
    rowR(type = "fill", direction = "left")


  names <- colnames(df)
  length <- length(colnames(df))

  level_list <- list()

  for(i in length:1) {

    level <- names[i]
    newnames <- paste0(names, "_y.axis")



    if(i == length) {

      df <- df %>% mutate(!!sym(newnames[i]) := as.numeric(row_number()))

      level_list[[names[i]]] <- df %>%
        mutate(parent = L1) %>%
        select(names[i], newnames[i], parent) %>%
        rename(label = !!sym(names[i]),
               y.axis = !!sym(newnames[i])) %>%
        mutate(x.axis = i) %>%
        as_tibble()

    } else {

      df <- df %>%
        group_by(!!sym(level)) %>%
        mutate(!!sym(newnames[i]) := median(!!sym(newnames[i+1])),
               parent = L1) %>%
        ungroup()

      level_list[[names[i]]] <- df %>%
        mutate(parent = L1) %>%
        select(names[i], newnames[i], parent) %>%
        drop_na(!!sym(names[i])) %>%
        rename(label = !!sym(names[i]),
               y.axis = !!sym(newnames[i])) %>%
        mutate(x.axis = i)

    }

  }

  lines <- df %>%
    select(parent, names[length], contains(newnames)) %>%
    pivot_longer(contains(newnames)) %>%
    mutate(x.axis = as.numeric(rep(seq(1,length), n()/length)))

  pdf <- bind_rows(level_list) %>% arrange(desc(parent)) %>% distinct(label, parent, .keep_all = T) %>%
    mutate(label = str_to_upper(label))

  ggplot(pdf, aes(x=x.axis, y=y.axis)) +
    geom_line(data=lines, aes(x=x.axis, y=value, group=!!sym(names[length]), color = parent)) +
    geom_label(aes(label = label, fill=parent), hjust = "left") +
    scale_fill_manual(values = cancR_palette) +
    scale_color_manual(values = cancR_palette) +
    coord_cartesian(xlim=c(1, pmax(length+0.5, (max(str_count(pdf$label))/15)))) +
    theme_classic() +
    theme(legend.position = "none",
          axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank())



}

