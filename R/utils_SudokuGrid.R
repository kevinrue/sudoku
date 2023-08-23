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
  (index-1) %/% 3 + 1
}

#' @rdname get_tile_index
get_tile_indices <- function(index) {
  1:3 + (index-1)*3
}


#' @importFrom tibble as_tibble
#' @importFrom dplyr filter pull
#' @importFrom rlang .data
get_tile_values <- function(x, row_idx, column_idx) {
  row_tile <- get_tile_index(row_idx)
  column_tile <- get_tile_index(column_idx)
  get_rows <- get_tile_indices(row_tile)
  get_columns <- get_tile_indices(column_tile)
  x %>% 
    as_tibble() %>% 
    # filter(row %in% get_rows & column %in% get_columns & !is.na(value)) %>% 
    filter(
      .data[[.grid_row_name]] %in% get_rows &
        .data[[.grid_column_name]] %in% get_columns &
        !is.na(.data[[.grid_value_name]])
    ) %>% 
    pull({{ .grid_value_name }})
}
