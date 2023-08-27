#' Replace Possible Values in SudokuGrid Cell
#' 
#' Replace the set of values possible in a SudokuGrid cell.
#'
#' @param object A [SudokuGrid-class] object.
#' @param i Row index.
#' @param j Column index.
#' @param values Integer vectors comprising values from 1 to 9.
#' @param given Whether the values were given in the starting grid.
#' Default to `TRUE` if a `value` has length 1.
#'
#' @return The updated [SudokuGrid-class] object.
#' 
#' @importFrom dplyr arrange bind_rows filter pick
#' @importFrom rlang := .data
#' @importFrom tibble tibble
#' @importFrom tidyselect all_of
#' 
#' @rdname INTERNAL_replaceCellValues
setMethod("replaceCellValues", "SudokuGrid", function(object, i, j, values,
  given=ifelse(identical(length(values), 1L), TRUE, FALSE)
){
  added_values <- tibble(
    grid_row = i, # TODO: <data-masking>
    grid_column = j, # TODO: <data-masking>
    grid_value = values, # TODO: <data-masking>
    given = given # TODO: <data-masking>
  )
  colnames(added_values) <- c(.grid_row_name, .grid_column_name, .grid_value_name, .grid_given_name)
  object %>% 
    as_tibble() %>% 
    filter(!(.data[[.grid_row_name]] == i & .data[[.grid_column_name]] == j)) %>% 
    bind_rows(added_values) %>% 
    arrange(pick(all_of(c(.grid_row_name, .grid_column_name))))
})

#' Plot the sudoku grid
#'
#' @param x A [SudokuGrid-class] object. 
#' @param ... Arguments passed to and from other methods.
#'
#' @return A `ggplot` object.
#' @export
#' @importFrom dplyr filter mutate_at
#' @importFrom ggplot2 aes geom_tile geom_text ggplot scale_color_manual theme_void
#' @importFrom rlang .data
#' @importFrom tibble as_tibble
#'
#' @examples
#' sudoku_grid <- simulate_grid()
#' plot(sudoku_grid)
plot.SudokuGrid <- function(x, ...) {
  x_gg <- x %>% 
    as_tibble() %>% 
    mutate_at(c(.grid_given_name), factor, c(TRUE, FALSE))
  tile_centers <- expand.grid(grid_row = 2 + 0:2*3, grid_column = 2 + 0:2*3)
  colnames(tile_centers) <- c(.grid_row_name, .grid_column_name)
  ggplot(x_gg, aes(.data[[.grid_column_name]], 10-.data[[.grid_row_name]])) +
    geom_tile(fill = "white", color = "black", width = 1, height = 1) +
    geom_tile(aes(10-.data[[.grid_row_name]], .data[[.grid_column_name]]), tile_centers, fill = NA, color = "black", width = 3, height = 3, linewidth = 2) +
    geom_text(aes(label = .data[[.grid_value_name]], color = .data[[.grid_given_name]]), x_gg %>% filter(!is.na(.data[[.grid_value_name]]))) +
    theme_void() +
    scale_color_manual(values = c("TRUE" = "black", "FALSE" = "cornflowerblue"))
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

#' @importFrom dplyr filter pull
#' @importFrom rlang .data
get_tile_values <- function(x, row_idx, column_idx) {
  row_tile <- get_tile_index(row_idx)
  column_tile <- get_tile_index(column_idx)
  get_rows <- get_tile_indices(row_tile)
  get_columns <- get_tile_indices(column_tile)
  x %>% 
    as_tibble() %>% 
    filter(.data[[.grid_row_name]] %in% get_rows &
        .data[[.grid_column_name]] %in% get_columns &
        !is.na(.data[[.grid_value_name]])) %>% 
    pull({{.grid_value_name}})
}

#' Eliminate Now-Impossible Choices
#' 
#' @param x A [SudokuGrid-class] object.
#' @param row_idx Index of the row of the cell being tested.
#' @param column_idx Index of the column of the cell being tested.
#' @param value Value being tested.
#'
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' 
#' @rdname INTERNAL_eliminate_impossible_choices
only_cell_in_tile_row_for_value <- function(x, row_idx, column_idx, value) {
  column_tile <- get_tile_index(column_idx)
  tile_columns <- get_tile_indices(column_tile)
  tile_other_columns <- setdiff(tile_columns, column_idx)
  x %>% 
    as_tibble() %>% 
    filter(.data[[.grid_value_name]] == value &
        .data[[.grid_row_name]] == row_idx &
        .data[[.grid_column_name]] %in% tile_other_columns) %>% 
    nrow() %>% 
    identical(0L)
}
