library(tidyverse)

sudoku_choices <- tibble(
  value = integer(0),
  row = integer(0),
  column = integer(0)
)

initiate_empty_grid <- function() {
  tibble(
    value = NA,
    expand.grid(row=1:9, column=1:9),
    given = NA
  ) %>% 
    arrange(row, column)
}

add_value_xy <- function(.grid, .row, .column, value, given=FALSE) {
  idx <- which(.grid$row == .row & .grid$column == .column)
  .grid[idx, "value"] <- value
  .grid[idx, "given"] <- given
  .grid
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
sudoku_grid

get_tile_index <- function(index) {
  (index-1) %/% 3 + 1
}

get_tile_indices <- function(index) {
  1:3 + (index-1)*3
}

get_tile_values <- function(.grid, .row, .column) {
  row_tile <- get_tile_index(.row)
  column_tile <- get_tile_index(.column)
  get_rows <- get_tile_indices(row_tile)
  get_columns <- get_tile_indices(column_tile)
  .grid %>% 
    filter(row %in% get_rows & column %in% get_columns & !is.na(value)) %>% 
    pull(value)
}

exclude_choice_required_elsewhere_in_tile <- function(.value, .choices, .row, .column) {
  # print("exclude_choice_required_elsewhere_in_tile")
  # look for row/columns where a number cannot appear in 2 out of 3 tiles (on that row)
  # deduct that the choice MUST appear on that row/column in the remaining tile
  # eliminate that choice from the other rows/columns in that remaining tile
  
  # considering that this function focuses on a given row/column,
  # it needs to look at the other two rows for that tile
  
  row_tile <- get_tile_index(.row)
  column_tile <- get_tile_index(.column)
  tile_rows <- get_tile_indices(row_tile)
  tile_columns <- get_tile_indices(column_tile)
  tile_other_rows <- setdiff(tile_rows, .row)
  tile_other_columns <- setdiff(tile_columns, .column)
  other_tiles_rows <- setdiff(1:9, tile_rows)
  other_tiles_columns <- setdiff(1:9, tile_columns)
  
  # only proceed if this is the only cell in this row of this tile possible for this value
  choice_unique_cell_in_tile_row <- .choices %>% 
    filter(value == .value & row == .row & column %in% tile_other_columns)
  # print("choice_unique_cell_in_tile_row")
  # print(choice_unique_cell_in_tile_row)
  if (identical(nrow(choice_unique_cell_in_tile_row), 0L)) {
    # which other rows in the tile is this value a choice for?
    choice_other_rows <- .choices %>% 
      filter(value == .value & row %in% tile_other_rows & column %in% tile_columns) %>% 
      pull(row) %>% 
      unique()
    # exclude the value from this cell if it is required in any other row in that tile
    # i.e., if it is a choice in the another row of that tile while not being a choice in that row in other tiles
    for (other_row in choice_other_rows) {
      choices_in_other_row_in_tile <- .choices %>% 
        filter(row %in% other_row & column %in% tile_columns) %>% 
        pull(value) %>% 
        unique()
      choices_in_other_row_in_other_tiles <- .choices %>% 
        filter(row %in% other_row & column %in% other_tiles_columns) %>% 
        pull(value) %>% 
        unique()
      if (.value %in% choices_in_other_row_in_tile && !.value %in% choices_in_other_row_in_other_tiles) {
        # print("exclude: required in other row in tile")
        return(TRUE)
      }
    }
  }
  
  # only proceed if this is the only cell in this column of this tile possible for this value
  choice_unique_cell_in_tile_column <- .choices %>% 
    filter(value == .value & column == .column & row %in% tile_other_rows)
  # print("choice_unique_cell_in_tile_column")
  # print(choice_unique_cell_in_tile_column)
  if (identical(nrow(choice_unique_cell_in_tile_column), 0L)) {
    # which other columns in the tile is this value a choice for?
    choice_other_columns <- .choices %>% 
      filter(value == .value & column %in% tile_other_columns & row %in% tile_rows) %>% 
      pull(column) %>% 
      unique()
    # exclude the value from this cell if it is required in any other column in that tile
    for (other_column in choice_other_columns) {
      choices_in_other_column_in_tile <- .choices %>% 
        filter(column %in% other_column & row %in% tile_rows) %>% 
        pull(value) %>% 
        unique()
      choices_in_other_column_in_other_tiles <- .choices %>% 
        filter(column %in% other_column & row %in% other_tiles_rows) %>% 
        pull(value) %>% 
        unique()
      if (.value %in% choices_in_other_column_in_tile && !.value %in% choices_in_other_column_in_other_tiles) {
        # print("exclude: required in other column in tile")
        return(TRUE) # exclude
      }
    }
  }
  return(FALSE)
}
exclude_choice_required_elsewhere_in_tile(8, sudoku_choices, 1, 2)

compute_cell_choices <- function(.grid, .choices, .row, .column) {
  grid_value <- .grid %>% 
    filter(row == .row & column == .column) %>% 
    pull(value)
  if (!is.na(grid_value)) {
    return(grid_value)
  }
  
  choices <- 1:9
  
  row_values_used <- .grid %>% 
    filter(row == .row & !is.na(value)) %>% 
    pull(value)
  choices <- setdiff(choices, row_values_used)
  
  column_values_used <- .grid %>% 
    filter(column == .column & !is.na(value)) %>% 
    pull(value)
  choices <- setdiff(choices, column_values_used)
  
  tile_values_used <- get_tile_values(.grid, .row, .column)
  choices <- setdiff(choices, tile_values_used)
  
  exclude <- vapply(choices, exclude_choice_required_elsewhere_in_tile, FUN.VALUE = logical(1), .choices = .choices, .row = .row, .column = .column)
  choices <- choices[!exclude]
  
  choices
}
compute_cell_choices(sudoku_grid, sudoku_choices, 1, 3)
compute_cell_choices(sudoku_grid, sudoku_choices, 2, 4)
compute_cell_choices(sudoku_grid, sudoku_choices, 3, 8)
compute_cell_choices(sudoku_grid, sudoku_choices, 6, 8)
compute_cell_choices(sudoku_grid, sudoku_choices, 8, 7)
compute_cell_choices(sudoku_grid, sudoku_choices, 1, 2)

update_choices_xy <- function(.grid, .row, .column, choices) {
  if (length(choices) == 0) {
    return(.grid)
  }
  .grid %>% 
    filter(!(row == .row & column == .column)) %>% 
    bind_rows(tibble(value = choices, row = .row, column = .column)) %>% 
    arrange(row, column)
}

update_choices_all <- function(sudoku_choices, sudoku_grid) {
  for (.row in 1:9) {
    for (.column in 1:9) {
      sudoku_grid_value <- sudoku_grid %>% 
        filter(row == .row & column == .column) %>% 
        pull(value)
      if (!is.na(sudoku_grid_value)) {
        sudoku_choices <- update_choices_xy(sudoku_choices, .row, .column, sudoku_grid_value)
      } else {
        choices <- compute_cell_choices(sudoku_grid, sudoku_choices, .row, .column)
        sudoku_choices <- update_choices_xy(sudoku_choices, .row, .column, choices)
      }
    }
  }
  sudoku_choices %>% 
    arrange(row, column)
}
sudoku_choices <- update_choices_all(sudoku_choices, sudoku_grid)

plot_grid <- function(.grid) {
  .grid <- .grid %>% 
    mutate(given = factor(given, c(TRUE, FALSE)))
  tile_centers <- expand.grid(row = 2 + 0:2*3, column = 2 + 0:2*3)
  ggplot(.grid, aes(column, 10-row)) +
    geom_tile(fill = "white", color = "black", width = 1, height = 1) +
    geom_tile(aes(10-row, column), tile_centers, fill = NA, color = "black", width = 3, height = 3, linewidth = 2) +
    geom_text(aes(label = value, color = given)) +
    theme_void() +
    scale_color_manual(values = c("TRUE" = "black", "FALSE" = "cornflowerblue"))
}
plot_grid(sudoku_grid)

plot_choices_value <- function(.choices, .value) {
  tile_centers <- expand.grid(row = 2 + 0:2*3, column = 2 + 0:2*3)
  .choices %>% 
    filter(value == .value) %>% 
    ggplot(aes(column, 10-row)) +
    geom_tile(fill = "white", color = "black", width = 1, height = 1) +
    geom_tile(aes(10-row, column), tile_centers, fill = NA, color = "black", width = 3, height = 3, linewidth = 2) +
    geom_text(aes(label = value)) +
    theme_void() +
    scale_color_manual(values = c("TRUE" = "black", "FALSE" = "cornflowerblue"))
}
plot_choices_value(sudoku_choices, 1)
plot_choices_value(sudoku_choices, 4)

strwrap_choices <- function(x) {
  x <- str_flatten(x)
  lapply(
    split(
      str_split(x, "")[[1]],
      head(rep(1:3, each = 3), str_length(x))
    ),
    str_flatten
  ) %>% str_flatten(collapse = "\n")
}
strwrap_choices("123456789")
strwrap_choices("12345")
strwrap_choices("12")

sudoku_choices %>% 
  group_by(row, column) %>% 
  summarise(value = strwrap_choices(value))

plot_choices <- function(.choices) {
  tile_centers <- expand.grid(row = 2 + 0:2*3, column = 2 + 0:2*3)
  .choices %>% 
    group_by(row, column) %>% 
    summarise(
      value = strwrap_choices(value),
      singleton = str_length(value) == 1
    ) %>%
    ggplot(aes(column, 10-row)) +
    geom_tile(fill = "white", color = "black", width = 1, height = 1) +
    geom_tile(aes(10-row, column), tile_centers, fill = NA, color = "black", width = 3, height = 3, linewidth = 2) +
    geom_text(aes(label = value, colour = singleton)) +
    theme_void() +
    scale_color_manual(values = c("TRUE" = "black", "FALSE" = "cornflowerblue"))
}
plot_choices(sudoku_choices)

get_cell_choices <- function(.grid, .row, .column) {
  .grid %>% 
    filter(row == .row & column == .column) %>% 
    pull(value)
}
get_cell_choices(sudoku_choices, 8, 7)
get_cell_choices(sudoku_choices, 1, 2)

only_cell_in_tile <- function(.choices, .value, .row, .column) {
  row_tile <- get_tile_index(.row)
  column_tile <- get_tile_index(.column)
  get_rows <- get_tile_indices(row_tile)
  get_columns <- get_tile_indices(column_tile)
  cells_in_tile <- .choices %>% 
    filter(value == .value) %>% 
    filter(row %in% get_rows & column %in% get_columns)
  identical(nrow(cells_in_tile), 1L) &&
    cells_in_tile$row == .row &&
    cells_in_tile$column == .column
}
only_cell_in_tile(sudoku_choices, 1, 3, 8)
only_cell_in_tile(sudoku_choices, 4, 4, 4)

only_cell_in_column <- function(.choices, .value, .row, .column) {
  cells_in_column <- .choices %>% 
    filter(column == .column & value == .value)
  identical(nrow(cells_in_column), 1L) &&
    cells_in_column$row == .row &&
    cells_in_column$column == .column
}
only_cell_in_column(sudoku_choices, 4, 4, 4)

only_cell_in_row <- function(.choices, .value, .row, .column) {
  cells_in_row <- .choices %>% 
    filter(row == .row & value == .value)
  identical(nrow(cells_in_row), 1L) &&
    cells_in_row$row == .row &&
    cells_in_row$column == .column
}
only_cell_in_row(sudoku_choices, 2, 5, 3)

prompt_next <- function() {
  x <- readline(prompt="Go next? ")
  grepl("y", x)
}

test_choices_xy <- function(.grid, .choices, .row, .column) {
  # what are the choices for the current cell?
  cell_choices <- get_cell_choices(.choices, .row, .column)
  message("== cell_choices ==")
  message("row: ", .row, ", column: ", .column, ", choice: ", cell_choices)
  if (identical(length(cell_choices), 1L)) {
    return(cell_choices)
  }
  # what are the choices for every other cell in the tile?
  tile_row_index <- get_tile_index(.row)
  tile_column_index <- get_tile_index(.column)
  tile_row_indices <- get_tile_indices(tile_row_index)
  tile_column_indices <- get_tile_indices(tile_column_index)
  competing_cells <- as_tibble(expand.grid(row = tile_row_indices, column = tile_column_indices)) %>% 
    bind_rows(tibble(row = .row, column = setdiff(1:9, .column))) %>% 
    bind_rows(tibble(row = setdiff(1:9, .row), column = .column)) %>% 
    filter(!(row == .row & column ==.column)) %>% 
    distinct(row, column) %>% 
    arrange(row, column)
  # print(c(.row, .column))
  # print(competing_cells, n = Inf)
  other_cell_choices <- competing_cells %>% 
    inner_join(.choices, c("row", "column")) %>% 
    pull(value) %>% 
    unique()
  # print(other_cell_choices)
  cell_choices[!cell_choices %in% other_cell_choices]
}
test_choices_xy(sudoku_grid, sudoku_choices, 3, 8)
test_choices_xy(sudoku_grid, sudoku_choices, 1, 2)

# Run ----

print(plot_grid(sudoku_grid))
firstpass <- TRUE
continue <- TRUE
prompt <- T
n_filled <- sum(!is.na(sudoku_grid$value))
while(any(is.na(sudoku_grid$value)) & continue) {
  if (firstpass) {
    sudoku_choices <- update_choices_all(sudoku_choices, sudoku_grid)
    firstpass <- FALSE
  }
  for (row in 1:9) {
    # interrupt if signal is toggled
    if (!continue) break
    for (column in 1:9) {
      # interrupt if signal is toggled
      if (!continue) break
      if (!is.na(sudoku_grid[sudoku_grid$row == row & sudoku_grid$column == column, "value"])) {
        next
      }
      # eliminate choices from 1:9
      only_choice <- test_choices_xy(sudoku_grid, sudoku_choices, row, column)
      message("== only_choice ==")
      message("row: ", row, ", column: ", column, ", only_choice: ", only_choice)
      if (length(only_choice) == 1) {
        if (prompt) {
          continue <- prompt_next()
        } else {
          continue <- TRUE
        }
        if (continue) {
          sudoku_grid <- add_value_xy(sudoku_grid, row, column, only_choice, given = FALSE)
          print(plot_grid(sudoku_grid))
          Sys.sleep(0.5)
          sudoku_choices <- update_choices_all(sudoku_choices, sudoku_grid)
          next
        } else {
          break
        }
      }
      # only cell in tile for this value?
      cell_choices <- get_cell_choices(sudoku_choices, row, column)
      only_cell <- vapply(cell_choices, only_cell_in_tile, logical(1), .choices = sudoku_choices, .row = row, .column = column)
      value <- cell_choices[only_cell]
      message("== only cell ==")
      message("row: ", row, ", column: ", column, ", value: ", value)
      if (sum(only_cell) == 1) {
        if (prompt) {
          continue <- prompt_next()
        } else {
          continue <- TRUE
        }
        if (continue) {
          sudoku_grid <- add_value_xy(sudoku_grid, row, column, value, given = FALSE)
          print(plot_grid(sudoku_grid))
          Sys.sleep(0.5)
          sudoku_choices <- update_choices_all(sudoku_choices, sudoku_grid)
          next
        } else {
          break
        }
      }
      # only cell in column for value?
      only_cell <- vapply(cell_choices, only_cell_in_column, logical(1), .choices = sudoku_choices, .row = row, .column = column)
      value <- cell_choices[only_cell]
      message("== only cell ==")
      message("row: ", row, ", column: ", column, ", value: ", value)
      if (sum(only_cell) == 1) {
        if (prompt) {
          continue <- prompt_next()
        } else {
          continue <- TRUE
        }
        if (continue) {
          sudoku_grid <- add_value_xy(sudoku_grid, row, column, value, given = FALSE)
          print(plot_grid(sudoku_grid))
          Sys.sleep(0.5)
          sudoku_choices <- update_choices_all(sudoku_choices, sudoku_grid)
          next
        } else {
          break
        }
      }
      # only cell in row for value?
      only_cell <- vapply(cell_choices, only_cell_in_row, logical(1), .choices = sudoku_choices, .row = row, .column = column)
      value <- cell_choices[only_cell]
      message("== only cell ==")
      message("row: ", row, ", column: ", column, ", value: ", value)
      if (sum(only_cell) == 1) {
        if (prompt) {
          continue <- prompt_next()
        } else {
          continue <- TRUE
        }
        if (continue) {
          sudoku_grid <- add_value_xy(sudoku_grid, row, column, value, given = FALSE)
          print(plot_grid(sudoku_grid))
          Sys.sleep(0.5)
          sudoku_choices <- update_choices_all(sudoku_choices, sudoku_grid)
          next
        } else {
          break
        }
      }
    }
  }
  new_n_filled <- sum(!is.na(sudoku_grid$value))
  if (continue && identical(new_n_filled, n_filled)) {
    message("No progress in last round. No point trying again.")
    break
  }
  n_filled <- new_n_filled
}
plot_grid(sudoku_grid)

