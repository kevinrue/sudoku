library(tidyverse)

initiate_empty_grid <- function() {
  tibble(
    value = NA,
    expand.grid(row=1:9, column=1:9),
    given = NA
  ) %>% 
    arrange(row, column)
}

add_value_xy <- function(grid, .row, .column, value, given=FALSE) {
  idx <- which(grid$row == .row & grid$column == .column)
  grid[idx, "value"] <- value
  grid[idx, "given"] <- given
  grid
}

plot_grid <- function(grid) {
  tile_centers <- expand.grid(row = 2 + 0:2*3, column = 2 + 0:2*3)
  ggplot(grid, aes(column, 10-row)) +
    geom_tile(fill = "white", color = "black", width = 1, height = 1) +
    geom_tile(aes(10-row, column), tile_centers, fill = NA, color = "black", width = 3, height = 3, linewidth = 2) +
    geom_text(aes(label = value, color = given)) +
    theme_void()
}

sudoku_grid <- initiate_empty_grid()
sudoku_grid <- add_value_xy(sudoku_grid, 1, 1, 2, given=TRUE)
sudoku_grid <- add_value_xy(sudoku_grid, 1, 4, 1, given=TRUE)
sudoku_grid <- add_value_xy(sudoku_grid, 1, 5, 9, given=TRUE)
sudoku_grid <- add_value_xy(sudoku_grid, 1, 9, 4, given=TRUE)
sudoku_grid <- add_value_xy(sudoku_grid, 2, 2, 1, given=TRUE)
sudoku_grid <- add_value_xy(sudoku_grid, 2, 5, 8, given=TRUE)
sudoku_grid <- add_value_xy(sudoku_grid, 3, 3, 5, given=TRUE)
sudoku_grid <- add_value_xy(sudoku_grid, 3, 7, 3, given=TRUE)
sudoku_grid <- add_value_xy(sudoku_grid, 4, 1, 6, given=TRUE)
sudoku_grid <- add_value_xy(sudoku_grid, 4, 6, 3, given=TRUE)
sudoku_grid <- add_value_xy(sudoku_grid, 5, 1, 4, given=TRUE)
sudoku_grid <- add_value_xy(sudoku_grid, 5, 2, 3, given=TRUE)
sudoku_grid <- add_value_xy(sudoku_grid, 5, 8, 9, given=TRUE)
sudoku_grid <- add_value_xy(sudoku_grid, 5, 9, 1, given=TRUE)
sudoku_grid <- add_value_xy(sudoku_grid, 6, 4, 2, given=TRUE)
sudoku_grid <- add_value_xy(sudoku_grid, 6, 9, 8, given=TRUE)
sudoku_grid <- add_value_xy(sudoku_grid, 7, 3, 7, given=TRUE)
sudoku_grid <- add_value_xy(sudoku_grid, 7, 7, 5, given=TRUE)
sudoku_grid <- add_value_xy(sudoku_grid, 8, 5, 4, given=TRUE)
sudoku_grid <- add_value_xy(sudoku_grid, 8, 8, 7, given=TRUE)
sudoku_grid <- add_value_xy(sudoku_grid, 9, 1, 1, given=TRUE)
sudoku_grid <- add_value_xy(sudoku_grid, 9, 5, 7, given=TRUE)
sudoku_grid <- add_value_xy(sudoku_grid, 9, 6, 6, given=TRUE)
sudoku_grid <- add_value_xy(sudoku_grid, 9, 9, 3, given=TRUE)
plot_grid(sudoku_grid)

get_tile_index <- function(index) {
  (index-1) %/% 3 + 1
}

get_tile_indices <- function(index) {
  1:3 + (index-1)*3
}

get_tile_values <- function(grid, .row, .column) {
  row_tile <- get_tile_index(.row)
  column_tile <- get_tile_index(.column)
  get_rows <- get_tile_indices(row_tile)
  get_columns <- get_tile_indices(column_tile)
  grid %>% 
    filter(row %in% get_rows & column %in% get_columns & !is.na(value)) %>% 
    pull(value)
}

get_cell_choices <- function(grid, .row, .column) {
  choices <- 1:9
  
  row_values_used <- grid %>% 
    filter(row == .row & !is.na(value)) %>% 
    pull(value)
  choices <- setdiff(choices, row_values_used)
  
  column_values_used <- grid %>% 
    filter(column == .column & !is.na(value)) %>% 
    pull(value)
  choices <- setdiff(choices, column_values_used)
  
  tile_values_used <- get_tile_values(grid, .row, .column)
  choices <- setdiff(choices, tile_values_used)
  
  choices
}
get_cell_choices(sudoku_grid, 1, 3)
get_cell_choices(sudoku_grid, 2, 4)
get_cell_choices(sudoku_grid, 3, 8)
get_cell_choices(sudoku_grid, 6, 8)
get_cell_choices(sudoku_grid, 8, 7)

only_cell_for_value <- function(grid, .value, .row, .column) {
  row_tile <- get_tile_index(.row)
  column_tile <- get_tile_index(.column)
  get_rows <- setdiff(get_tile_indices(row_tile), .row)
  get_columns <- setdiff(get_tile_indices(column_tile), .column)
  only_row <- grid %>%
      filter(row %in% get_rows & !is.na(value)) %>% 
      group_by(row) %>% 
      summarise(test = any(value == .value | (column == .column & !is.na(value)), na.rm = TRUE)) %>% 
      pull()
  only_column <- grid %>%
      filter(column %in% get_columns) %>% 
      group_by(column) %>% 
      summarise(test = any(value == .value | (row == .row & !is.na(value)), na.rm = TRUE)) %>% 
      pull()
  print(c(only_row, only_column))
  all(only_row, only_column)
}
only_cell_for_value(sudoku_grid, 1, 3, 8)

prompt_next <- function() {
  x <- readline(prompt="Go next?")
  grepl("y", x)
}

iteration <- 0
next_iteration <- 1
while(any(is.na(sudoku_grid$value)) & iteration < next_iteration) {
  iteration <- next_iteration
  message("iteration: ", iteration)
  for (row in 1:9) {
    for (column in 1:9) {
      if (!is.na(sudoku_grid[sudoku_grid$row == row & sudoku_grid$column == column, "value"])) {
        next
      }
      choices <- get_cell_choices(sudoku_grid, row, column)
      message("== choices ==")
      message(choices)
      if (length(choices) == 1) {
        value <- choices
        message("== only choice ==")
        message("row: ", row, ", column: ", column, ", value: ", value)
        sudoku_grid <- add_value_xy(sudoku_grid, row, column, value)
        plot_grid(sudoku_grid)
        if (prompt_next()) {
          next_iteration <- iteration + 1
          next
        }
      }
      only_cell <- vapply(choices, only_cell_for_value, logical(1), grid = sudoku_grid, .row = row, .column = column)
      if (sum(only_cell) == 1) {
        value <- choices[only_cell]
        message("== only cell ==")
        message("row: ", row, ", column: ", column, ", value: ", value)
        sudoku_grid <- add_value_xy(sudoku_grid, row, column, value)
        plot_grid(sudoku_grid)
        if (prompt_next()) {
          next_iteration <- iteration + 1
          next
        }
      }
    }
  }
}
plot_grid(sudoku_grid)
