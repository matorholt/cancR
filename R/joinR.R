#' Join two or more dataframes
#'
#' @description
#' Join two or more dataframes as a left, right, full or inner join. The dataframes can be provided separately or as a list.
#'
#'
#' @param ... dataframes or list of dataframes to be joined
#' @param by variable to join on. This can either be a vector of one or multiple keys (e.g. by = c("id", "group")) or a list of vectors containing key columns.
#' @param type type of join (see details)
#' @param vars vector of variables the join should update/insert
#' @param dt whether the returned dataframe should be a data.table
#'
#' @details
#' The join types fall into four groups.
#'
#' **Mutating joins** combine the columns of `x` and `y`:
#' \describe{
#'   \item{`left`}{Keeps all rows of `x`. Columns from `y` are added, and
#'     rows without a match get `NA`.}
#'   \item{`right`}{Keeps all rows of `y`. Columns from `x` are added, and
#'     rows without a match get `NA`.}
#'   \item{`full`}{Keeps all rows of both `x` and `y`. Unmatched rows on
#'     either side get `NA` in the other side's columns.}
#'   \item{`inner`}{Keeps only rows with a match in both `x` and `y`.}
#' }
#'
#' **Filtering joins** only filter `x` and never add columns from `y`:
#' \describe{
#'   \item{semi}{Keeps rows of `x` that have a match in `y`.}
#'   \item{`anti`}{Keeps rows of `x` that have **no** match in `y`.}
#' }
#'
#' **Row-update joins** modify `x` using rows from `y`. They use the columns
#' that `x` and `y` have in common, apart from the keys, or only the columns
#' named in `vars`. They do not add new columns to `x`:
#' \describe{
#'   \item{`insert`}{For matching keys,
#'     only `NA` values in `x` are filled with the values from `y`. Existing
#'     non-`NA` values are kept, and no new rows are added.}
#'   \item{`update`}{For matching keys,
#'     values in `x` are overwritten with the values from `y`, and rows in `y`
#'     with keys not found in `x` are added as new rows.}
#' }
#'
#' **Keys**
#'
#' Use `by = "id"` or `by = c("id", "grp")` when the key columns have the same
#' name in all tables. When the names differ, use the form
#' `by = "id = pnr = id2"`, corresponding to the same length as the number of
#' input datasets. Alternatively pass a list with one #' element per key set
#' e.g. `list(c("id", "pnr"), "grp")`, where a single element is recycled to the
#' length of the input studies.
#'
#'
#' @returns a single joined dataframe
#' @export
#'
#'
#' @examples
#' x <- data.table(id = c(1L, 2L, 3L, 3L), grp = c("g1", "g1", "g1", "g2"), name = c("a", "b", "c", "d"), score = c(10, NA, 30, NA))
#' y <- data.table(id = 2:4, grp = c("g1", "g2", "g1"), city = c("X", "Y", "Z"), active = c(TRUE, FALSE, TRUE))
#' new_rows <- data.table(id = 2:4, name = c("b2", "c2", "d"), score = c(20, 35, 40))
#' z <- data.table(pnr = c(1, 3, 5), region = c("N", "S", "E"))
#' df_list <- list(
#'   data.table(id2 = 1:3,        grp = c("g1", "g1", "g2"), val_a = c("a1", "a2", "a3")),
#'   data.table(pnr = c(1, 2, 4), grp = c("g1", "g2", "g1"), val_b = c("b1", "b2", "b4")),
#'   data.table(id  = c(1, 3, 4), grp = c("g1", "g2", "g1"), val_c = c("c1", "c3", "c4"))
#' )
#'
#' # Basic joins
#' joinR(x, y, by = id, type = left)
#' joinR(x, y, by = c(id, grp), type = left)
#' joinR(x, y, by = id, type = full)
#' joinR(x, y, by = c(id, grp), type = inner)
#'
#' # Filter-joins
#' joinR(x, y, by = id, type = semi)
#' joinR(x, y, by = c(id, grp), type = anti)
#'
#' # Update/insert joins
#' joinR(x, new_rows, by = id, type = insert)
#' joinR(x, new_rows, by = id, type = update)
#' # Update only score
#' joinR(x, new_rows, by = id, type = update, vars = score)
#'
#' # Different keys
#' joinR(x, z, by = "id = pnr", type = left)
#' joinR(x, df_list, by = c("id = id2 = pnr = id"), type = left)
#' joinR(x, y, df_list, by = list(c("id", "id", "id2", "pnr", "id"),
#'                                "grp"), type = "left")

# x <- data.table(id = c(1L, 2L, 3L, 3L), grp   = c("g1", "g1", "g1", "g2"), name = c("a", "b", "c", "d"), score = c(10, NA, 30, NA))
# y <- data.table(id = 2:4, grp = c("g1", "g2", "g1"), city = c("X", "Y", "Z"), active = c(TRUE, FALSE, TRUE))
# new_rows <- data.table(id = 2:4, name = c("b2", "c2", "d"), score = c(20, 35, 40))
# z <- data.table(pnr = c(1, 3, 5), region = c("N", "S", "E"))
# df_list <- list(
#   data.table(id2 = 1:3,          grp = c("g1", "g1", "g2"), val_a = c("a1", "a2", "a3")),
#   data.table(pnr = c(1, 2, 4),   grp = c("g1", "g2", "g1"), val_b = c("b1", "b2", "b4")),
#   data.table(id  = c(1, 3, 4),   grp = c("g1", "g2", "g1"), val_c = c("c1", "c3", "c4"))
# )
#
# #Basic joins
# joinR(x, y, by = id, type = left)
# joinR(x, y, by = c(id,grp), type = left)
# joinR(x, y, by = id, type = full)
# joinR(x, y, by = c(id,grp), type = inner)
#
# #Filter-joins
# joinR(x, y, by = id, type = semi)
# joinR(x, y, by = c(id,grp), type = anti)
#
# #Update/insert joins
# joinR(x, new_rows, by = id, type = insert)
# joinR(x, new_rows, by = id, type = update)
#Update only score
# joinR(x, new_rows, by = id, type = update, vars = score)
#
# #Different keys
# joinR(x, z, by = "id = pnr", type = left)
# joinR(x, df_list, by = c("id = id2 = pnr = id"), type = left)
# joinR(x, y, df_list, by = list(c("id", "id", "id2", "pnr", "id"),
#                                "grp"), type = "left")


joinR <- function(..., by, type = "left", vars = NULL, dt = NULL) {

  choices <- c("left", "right", "full", "inner", "anti", "insert", "update", "semi")

  type <- defusR(type)

  if(type %nin% choices) cli::cli_abort("{type} not supported. Choose from: {choices}")

  switch(type,
         "left" = {dx = TRUE; dy = FALSE},
         "full" = {dx = TRUE; dy = TRUE},
         "right" = {dx = FALSE; dy = TRUE},
         "inner" = {dx = FALSE; dy = FALSE})

  dfs <- list(...)

  vars <- defusR(vars, data = dfs[[1]])

  #Combine list and non-list inputs
  dfs <- imap(dfs, ~ {
    if(all(class(.x) %nin% "list")) list(.x) else .x
  }) %>% flatten

  imap(dfs, ~ if(nrow(.x) == 0) cli::cli_abort("Empty dataframe (df number {i})"))

  #Return DT if any input is DT and dt is not specified
  if(is.null(dt)) dt <- any(map_lgl(dfs, ~ is.data.table(.x)))

  by_check <- tryCatch({is.list(by)}, error = function(e) {NULL})

  if(is.null(by_check)) by <- defusR(by)

  by_list <- if(any(str_detect(unlist(by), "="))) list(defusR(by)) else as.list(by)

  by_list <- map(by_list, ~ {

    if(length(dfs) != length(.x) & any(str_detect(.x, "=", negate = T))) {

      if(length(dfs) %% length(.x) != 0) cli::cli_abort("Length of by-statement ({.x}) not recyclable with length of datasets ({length(dfs)})")

      rep(.x, length(dfs)/length(.x)) %>% defusR

    } else {
      defusR(.x)
    }

  })

  by_x <- map_chr(by_list, 1)

  by_list <- map(seq_along(dfs), ~ {
    #Get name conversions (for each by-element, the corresponding name for each data frame is extracted
    map(by_list, .x) %>% set_names(by_x)
  })

  by_y <- map(by_list[-1], ~ unlist(.x)[by_x])

  ##############################################  JOINS  ##############################################
  #Name check

  dfs <-
    map2(seq_along(dfs), by_list, ~ {
      .y
      #   print(dfs[[.x]])
      #   print(.y)
      #   #print(unlist(.y))
      #
      # if(any(unlist(.y) %nin% names(dfs[[.x]]))) cli::cli_abort("{setdiff(unlist(.y), names(dfs[[.x]]))} not present in dataset {.x}")
      #
      as.data.table(dfs[[.x]])

    })



  x0 <- dfs[[1]]
  ys      <- dfs[-1]
  on_list <- map(by_y, ~ setNames(unname(.x), unname(by_x)))

  if (type %in% c("insert", "update")) {
    patch_cols <- purrr::map2(ys, on_list, function(y, on) {
      # vars or else all vars (must exist in both datasets)
      setdiff(intersect(vars %||% names(y) , intersect(names(x0), names(y))),
              c(names(on), unname(on)))
    })
  }

  joined_data <- switch(type,

                        anti = Reduce(function(x, k) x[!ys[[k]], on = on_list[[k]]],
                                      seq_along(ys), init = x0),

                        semi = Reduce(function(x, k) {
                          on     <- on_list[[k]]
                          y_keys <- unique(ys[[k]], by = unname(on))

                          idx <- x[y_keys, on = on, which = TRUE, nomatch = NULL]
                          x[sort(idx)]
                        }, seq_along(ys), init = x0),

                        insert = Reduce(function(x, k) {
                          cols <- patch_cols[[k]]
                          if (length(cols))
                            x[ys[[k]], on = on_list[[k]],
                              (cols) := Map(data.table::fcoalesce, mget(cols), mget(paste0("i.", cols)))]
                          x[]
                        }, seq_along(ys), init = x0),

                        update = Reduce(function(x, k) {
                          cols <- patch_cols[[k]]
                          if (length(cols))
                            x[ys[[k]], on = on_list[[k]], (cols) := mget(paste0("i.", cols))]
                          x[]
                        }, seq_along(ys), init = x0),

                        # left, right, full, inner
                        Reduce(function(x, k)
                          merge(x, ys[[k]],
                                by.x = unname(by_x), by.y = unname(by_y[[k]]),
                                all.x = dx, all.y = dy),
                          seq_along(ys), init = x0)
  )

  if(!dt) return(as.data.frame(joined_data)) else return(joined_data)

}
