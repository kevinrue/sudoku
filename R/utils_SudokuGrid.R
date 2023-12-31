#' Convert Between Row and Tile Indices
#'
#' @param index For `get_tile_index()`, an integer scalar from 1 to 9.
#' For `get_tile_indices`, an integer scalar from 1 to 3.
#'
#' @return For `get_tile_index()`, an integer scalar from 1 to 3,
#' indicating the tile corresponding to that row.
#' For `get_tile_indices`, an integer scalar from 1 to 9,
#' indicating the row corresponding to that tile.
#' 
#' @rdname INTERNAL_get_tile_index
get_tile_index <- function(index) {
  as.integer((index-1) %/% 3 + 1)
}

#' @rdname INTERNAL_get_tile_index
get_tile_indices <- function(index) {
  as.integer(1:3 + (index-1)*3)
}

#' @importFrom dplyr n ungroup
n_cells_to_fill <- function(x) {
  x %>% group_by(
    grid_row, grid_column) %>%
    summarise(status = unique(status)) %>%
    filter(status == "candidate") %>%
    ungroup() %>%
    summarise(n=n())
}
