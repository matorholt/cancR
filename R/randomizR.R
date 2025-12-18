#' Generate stratified, block randomized allocation sequence
#'
#' @description
#' Stratified block randomization sequence with optional redcap API. Each stratum will have its own unique allocation sequence.
#'
#'
#' @param strata named list of strata levels (e.g. list("age" = c(1,2), "sex" = c("f","m"))). If missing, an API token can be provided (see "token")
#' @param block.size vector of block size(s).
#' @param block.probabilities vector of block size probabilities
#' @param replications length of the allocation sequence in each stratum
#' @param allocation.levels vector of the allocated treatment levels, default = c("1", "2")
#' @param allocation.name column name for the allocated treatment levels, default = "allocation"
#' @param block.delay number of blocks that is fixed to the first block.size to avoid large blocks in the beginning of the sequence
#' @param token token for redcap API to automatically extract strata levels from redcap. If strata is a vector of variables, token must be provided.
#' @param seed seed for reproducibility
#' @param print whether a full report for each stratum should be printed
#'
#' @returns a data frame containing rows equal to the product of unique strata and replications.
#' @export

# tickR()
# allocation <-
#   randomizR(strata=c("age_group_depth",
#                      "gender_depth",
#                      "ana_site_depth",
#                      "immuno_depth",
#                      "seq_width_depth",
#                      "seq_slnb_depth"),
#             block.size = c(2,4),
#             block.probabilities = c(0.9,0.1),
#             replications = 50,
#             block.delay = 10,
#             token = '859A8159EB61A5AD6ABB8DE0BC78E67F',
#             seed = 1,
#             print=T)
# tockR()

randomizR <- function(strata,
                      block.size,
                      block.probabilities = NULL,
                      replications = 50,
                      allocation.levels = c("1","2"),
                      allocation.name = "redcap_randomization_group",
                      block.delay = 10,
                      token,
                      seed = 1,
                      print=F) {

  if(class(strata) != "list") {

    #Use API to obtain levels for all strata
    rcon <- redcapAPI::redcapConnection(url='https://redcap.regionh.dk/api/', token=token)
    meta_data <- rcon$metadata()

    levels <- lapply(seq_along(strata), function(v) {

      unlist(str_extract_all(meta_data[meta_data$field_name == strata[v], "select_choices_or_calculations"], "\\d+(?=(,))"))

    }) %>% set_names(strata)

  } else {

    levels <- strata

  }

  if(any(unlist(map_depth(levels, 1, is.null)))) {

    return(cat("Error: Empty levels in one or more strata - check strata names"))

  }

  #All potential combinations of strata levels
  strata_grid <- expand.grid(levels)

  allocations <-
    #For each stratum level combination
    lapply(1:nrow(strata_grid), function(s) {

      set.seed(seed+s)

      #Avoid sequence containing only the first block size by inserting the second block size at a random point ranging between
      #block.delay:replications-max(block.size), but only if second block.size is not present
      permute_salvage <- ceiling(sample(block.delay:replications-block.size[2], 1))

      sequence <- c()
      #Continue until replications is reached
      while(sum(sequence) < replications) {

        #First n blocks or if last block equals first block size - assign first block (block delay)
        if(sum(sequence) <= block.delay | replications - sum(sequence) == block.size[1]) {
          seq_vec <- block.size[1]
          #If second block has not been assigned yet at the permute_salvage point
        } else if(sum(sequence) > permute_salvage & block.size[2] %nin% sequence) {
          seq_vec <- block.size[2]
          #Sample from block.size with block.probs
        } else{
          seq_vec <- sample(block.size, size = 1, prob=block.probabilities)
        }


        sequence <- c(sequence, seq_vec)

      }

      #Each block is sampled from the allocation.levels with size equal to the block size without replacement, then unlisted and colbinded to the strata_grid
      bind_cols(strata_grid %>%
                  slice(s),
                bind_rows(
                  lapply(seq_along(sequence), function(i) {
                    data.frame(allocation = unlist(sample(rep(c(allocation.levels), sequence[i]/2), size = sequence[i], replace=F)),
                               block = i,
                               block.size = sequence[i])
                  })) %>%
                  mutate(row = row_number()))

    }) %>% bind_rows %>%
    #Add stratum column
    unite("stratum", all_of(strata), remove=F, sep = " ") %>%
    #Empty column for redcap
    mutate(redcap_randomization_number = NA) %>%
    rename(!!sym(allocation.name) := allocation)

  #Strata report
  strata_report <- lapply(unique(allocations$stratum), function(s) {
    df <- allocations %>% filter(stratum == s)

    data.frame(
      "Stratum" = s,
      "Replications" = nrow(df),
      "Allocation ratio" = paste0(df %>% group_by(!!sym(allocation.name)) %>% count %>% pull(n), collapse="/"),
      "Block.Count" = max(df$block),
      "Block.Sizes" = paste0(unique(df$block.size), collapse= ",")) %>%
      bind_cols(.,
                df %>%
                  distinct(block, block.size) %>%
                  group_by(block.size) %>%
                  count %>%
                  pivot_wider(names_from=block.size, values_from=n) %>%
                  mutate(across(c(everything()), ~ round(./n_distinct(df$block)*100,1), .names = "{.col}(%)")))

  }) %>% bind_rows %>%
    tibble

  cat("Stratified block randomization complete\n\n")

  cat(paste0("Stratified on ", length(levels), " strata with the levels:\n"))
  print(levels)
  cat(paste0("Number of unique strata: ", n_distinct(allocations$stratum), "\n\n"))
  cat("Strata balances:\n")

  suppressWarnings(strata_report %>% ungroup() %>% summarise(across(c((ncol(.)-(length(block.size)-1)):ncol(.)), ~ list(
    min(.),
    quantile(., 0.25),
    median(.),
    quantile(., 0.75),
    max(.)))) %>%
    unnest(everything()) %>% print)

  if(print) {
    print(strata_report, n=Inf)
  }

  return(allocations %>% select(redcap_randomization_number, !!sym(allocation.name), !!!syms(names(levels))))

}
