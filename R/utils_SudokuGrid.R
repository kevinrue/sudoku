#' Convert Between Row and Tile Indices
#'
#' @param index For `get_tile_index()`, an integer scalar from 1 to 9.
#' For `get_tile_indices`, an integer scalar from 1 to 3.
#'
#' @return For `get_tile_index()`, an integer scalar from 1 to 3,
#' indicating the tile corresponding to that row.
#' For `get_tile_indices`, an integer scalar from 1 to 9,
#' indicating the row corresponding to that tile.
get_tile_index <- function(index) {
  (index-1) %/% 3 + 1
}

#' @rdname get_tile_index
get_tile_indices <- function(index) {
  1:3 + (index-1)*3
}
