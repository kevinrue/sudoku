library(tidyverse)

initiate_empty_grid <- function() {
  tibble(
    value = NA,
    expand.grid(row=1:9, column=1:9)
  ) %>% 
    arrange(row, column)
}

add_value_xy <- function(grid, .row, .column, value) {
  idx <- which(grid$row == .row & grid$column == .column)
  grid[idx, "value"] <- value
  grid
}

plot_grid <- function(grid) {
  ggplot(sudoku_grid, aes(column, 10-row)) +
    geom_tile(fill = "white", color = "black", width = 1, height = 1) +
    geom_text(aes(label = value)) +
    theme_void()
}

sudoku_grid <- initiate_empty_grid()
sudoku_grid <- add_value_xy(sudoku_grid, 1, 1, 2)
sudoku_grid <- add_value_xy(sudoku_grid, 1, 4, 1)
sudoku_grid <- add_value_xy(sudoku_grid, 1, 5, 9)
sudoku_grid <- add_value_xy(sudoku_grid, 1, 9, 4)
sudoku_grid <- add_value_xy(sudoku_grid, 2, 2, 1)
sudoku_grid <- add_value_xy(sudoku_grid, 2, 5, 8)
sudoku_grid <- add_value_xy(sudoku_grid, 3, 3, 5)
sudoku_grid <- add_value_xy(sudoku_grid, 3, 7, 3)
sudoku_grid <- add_value_xy(sudoku_grid, 4, 1, 6)
sudoku_grid <- add_value_xy(sudoku_grid, 4, 6, 3)
sudoku_grid <- add_value_xy(sudoku_grid, 5, 1, 4)
sudoku_grid <- add_value_xy(sudoku_grid, 5, 2, 3)
sudoku_grid <- add_value_xy(sudoku_grid, 5, 8, 9)
sudoku_grid <- add_value_xy(sudoku_grid, 5, 9, 1)
sudoku_grid <- add_value_xy(sudoku_grid, 6, 4, 2)
sudoku_grid <- add_value_xy(sudoku_grid, 6, 9, 8)
sudoku_grid <- add_value_xy(sudoku_grid, 7, 3, 7)
sudoku_grid <- add_value_xy(sudoku_grid, 7, 7, 5)
sudoku_grid <- add_value_xy(sudoku_grid, 8, 5, 4)
sudoku_grid <- add_value_xy(sudoku_grid, 8, 8, 7)
sudoku_grid <- add_value_xy(sudoku_grid, 9, 1, 1)
sudoku_grid <- add_value_xy(sudoku_grid, 9, 5, 7)
sudoku_grid <- add_value_xy(sudoku_grid, 9, 6, 6)
sudoku_grid <- add_value_xy(sudoku_grid, 9, 9, 3)
plot_grid(sudoku_grid)

get_tile_values <- function(grid, .row, .column) {
  tile_row <- .row %/% m + 1
  tile_column <- .column %/% m + 1
  get_rows <- 1:3 + (tile_row-1)*3
  get_columns <- 1:3 + (tile_row-1)*3
}

get_cell_choices <- function(grid, .row, .column) {
  choices <- 1:9
  row_values_used <- grid %>% 
    filter(row == .row & !is.na(value)) %>% 
    pull(value)
  message("row values used:", row_values_used)
  choices <- setdiff(choices, row_values_used)
  column_values_used <- grid %>% 
    filter(column == .column & !is.na(value)) %>% 
    pull(value)
  message("column values used:", column_values_used)
  choices <- setdiff(choices, column_values_used)
  choices
}
get_cell_choices(sudoku_grid, 1, 3)
