#' Join two or more dataframes
#'
#' @description
#' Join two or more dataframes as a left, right, full or inner join. The dataframes can be provided separately or as a list.
#'
#'
#' @param ... dataframes or list of dataframes to be joined
#' @param by variable to join on. This can either be a vector of one or multiple keys (e.g. by = c("id", "group")) or a list of vectors containing key columns.
#' @param type type of join (e.g. left (default), right, full and inner)
#' @param dt whether the returned dataframe should be a data.table
#'
#' @returns a single joined dataframe
#' @export
#'
#'
#'

# df_list <- list(data.frame(id = c(1,2,3),
#                            pnr = c(1,2,3),
#                            x = c(1,1,2),
#                            v1 = c(1,1,2)),
#                 data.frame(id = c(2,3,3),
#                            pnr2 = c(2,3,3),
#                            x2 = c(1,1,2),
#                            v1 = c(1,1,2),
#                            y = c(2,2,2)),
#                 data.frame(id = c(2,4,4),
#                            pnr3 = c(2,4,4),
#                            x3 = c(1,3,3),
#                            v1 = c(1,3,3),
#                            z = c(3,3,3)))
#
# joinR(df_list, by = "id", type = "left", dt=T)
# joinR(df_list[[1]], df_list[[3]], by=c("id", "v1"))
# joinR(df_list, by = c("id", "v1"), type = "left", dt=T)
# joinR(df_list, by = list(c("pnr", "pnr2", "pnr3")), type = "full")
# joinR(df_list, by = list(c("pnr", "pnr2", "pnr3"),
#                          c("x", "x2", "x3")), type = "left", dt=T)
# joinR(df_list[[1]], df_list[[3]], by = list(c("pnr", "pnr3")), type = "anti")



joinR <- function(..., by, type = "left", dt = F) {


  switch(type,
         "left" = {dx = TRUE; dy = FALSE},
         "full" = {dx = TRUE; dy = TRUE},
         "right" = {dx = FALSE; dy = TRUE},
         "inner" = {dx = FALSE; dy = FALSE})

  dfs <- list(...)

  if(length(dfs) == 1) {
    dfs <- dfs[[1]]
  }

  if(class(by) == "list") {

    by_vec <- map_chr(by, 1)

    by_list <- lapply(seq_along(dfs), function(i) {
      #Get name conversions (for each by-element, the corresponding name for each data frame is extracted
      map(by, i) %>% set_names(by_vec)
    })

    } else {
    by_vec <- by
    }

  #Aligning by names and set as key
  dfs <- lapply(seq_along(dfs), function(i) {

  dt <- as.data.table(dfs[[i]])

  if(class(by) == "list") {

  setnames(dt, unlist(by_list[[i]]), names(by_list[[i]]))
  }

  setkeyv(dt, by_vec)

    dt
  })



  if(type == "anti") {

    joined_data <- dfs[[1]][!dfs[[2]], on = by_vec]



  } else {

  joined_data <- Reduce(function(x,y) #Running full-join merge
    merge(x, y, by = by_vec, all.x = dx, all.y = dy), dfs)
  }

  #Remove key
  setkey(joined_data, NULL)

  if(!dt) return(as.data.frame(joined_data))

  return(joined_data)

}
