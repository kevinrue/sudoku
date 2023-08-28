#' @export
#' @rdname INTERNAL_replaceCellValues
replace_cell_values.sudoku <- function(object, i, j, values,
  given=ifelse(identical(length(values), 1L), TRUE, FALSE))
{
  added_values <- tibble(
    "{ .grid_row_name }" := i,
    "{ .grid_column_name }" := j,
    "{ .grid_value_name }" := values,
    "{ .grid_given_name }" := given
  )
  # colnames(added_values) <- c(.grid_row_name, .grid_column_name, .grid_value_name, .grid_given_name)
  object %>% 
    as_tibble() %>% 
    filter(!(.data[[.grid_row_name]] == i & .data[[.grid_column_name]] == j)) %>% 
    bind_rows(added_values) %>% 
    arrange(pick(all_of(c(.grid_row_name, .grid_column_name))))
}

#' Plot the sudoku grid
#'
#' @param x A `sudoku` object. 
#' @param ... Arguments passed to and from other methods.
#'
#' @return A `ggplot` object.
#' @export
#' @importFrom dplyr filter mutate_at
#' @importFrom ggplot2 aes geom_tile geom_text ggplot scale_color_manual theme_void
#' @importFrom rlang .data
#' @importFrom tibble as_tibble
#' 
#' @rdname plot.sudoku
#'
#' @examples
#' sudoku_grid <- simulate_grid()
#' plot(sudoku_grid)
plot.sudoku <- function(x, ...) {
  # data
  x_gg <- x %>% 
    as_tibble() %>% 
    mutate_at(c(.grid_given_name), factor, c(TRUE, FALSE))
  # tiles
  tile_centers <- expand.grid(grid_row = 2 + 0:2*3, grid_column = 2 + 0:2*3)
  colnames(tile_centers) <- c(.grid_row_name, .grid_column_name)
  # plot
  ggplot(x_gg, aes(.data[[.grid_column_name]], 10-.data[[.grid_row_name]])) +
    geom_tile(fill = "white", color = "black", width = 1, height = 1) +
    geom_tile(aes(10-.data[[.grid_row_name]], .data[[.grid_column_name]]), tile_centers, fill = NA, color = "black", width = 3, height = 3, linewidth = 2) +
    geom_text(aes(label = .data[[.grid_value_name]], color = .data[[.grid_given_name]]), x_gg %>% filter(!is.na(.data[[.grid_value_name]]))) +
    theme_void() +
    scale_color_manual(values = c("TRUE" = "black", "FALSE" = "cornflowerblue"))
}

#' @export
#' 
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' 
#' @rdname plot.sudoku
plot_value.sudoku <- function(x, value, ...) {
  # data
  x_gg <- x %>% 
    as_tibble() %>%
    filter(.data[[.grid_value_name]] == value) %>% 
    mutate_at(c(.grid_given_name), factor, c(TRUE, FALSE))
  empty_cells <- as_tibble(expand.grid(grid_row = 1:9, grid_column = 1:9))
  colnames(empty_cells) <- c(.grid_row_name, .grid_column_name)
  empty_cells[[.grid_value_name]] <- NA
  empty_cells[[.grid_given_name]] <- NA
  x_gg <- bind_rows(empty_cells, x_gg)
  x_gg <- as_sudoku(x_gg)
  plot.sudoku(x_gg)
}

#' @importFrom tibble as_tibble
#' @importFrom dplyr filter pull
#' @importFrom rlang .data
get_tile_values <- function(x, row_idx, column_idx) {
  # row
  row_tile <- get_tile_index(row_idx)
  get_rows <- get_tile_indices(row_tile)
  # column
  column_tile <- get_tile_index(column_idx)
  get_columns <- get_tile_indices(column_tile)
  # logic
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

#' Eliminate Impossible Choices
#' 
#' @param x A `sudoku` object.
#' @param row_idx Index of the row of the cell being tested.
#' @param column_idx Index of the column of the cell being tested.
#' @param value Value being tested.
#'
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' 
#' @rdname INTERNAL_update_choices
only_cell_in_tile_row_for_value <- function(x, row_idx, column_idx, value) {
  # column
  column_tile <- get_tile_index(column_idx)
  tile_columns <- get_tile_indices(column_tile)
  tile_other_columns <- setdiff(tile_columns, column_idx)
  # logic
  x %>% 
    as_tibble() %>% 
    filter(.data[[.grid_value_name]] == value &
        .data[[.grid_row_name]] == row_idx &
        .data[[.grid_column_name]] %in% tile_other_columns) %>% 
    nrow() %>% 
    identical(0L)
}

#' @importFrom dplyr filter
#' @importFrom rlang .data
#' 
#' @rdname INTERNAL_update_choices
only_cell_in_tile_column_for_value <- function(x, row_idx, column_idx, value) {
  # row
  row_tile <- get_tile_index(row_idx)
  tile_rows <- get_tile_indices(row_tile)
  tile_other_rows <- setdiff(tile_rows, row_idx)
  # logic
  x %>% 
    as_tibble() %>% 
    filter(.data[[.grid_value_name]] == value &
        .data[[.grid_row_name]] == column_idx &
        .data[[.grid_column_name]] %in% tile_other_rows) %>% 
    nrow() %>% 
    identical(0L)
}

#' @importFrom dplyr filter pull
#' @importFrom rlang .data
#' @importFrom tibble as_tibble
#' 
#' @rdname INTERNAL_update_choices
which_other_tile_rows_for_value <- function(x, row_idx, column_idx, value) {
  # row
  row_tile <- get_tile_index(row_idx)
  tile_rows <- get_tile_indices(row_tile)
  tile_other_rows <- setdiff(tile_rows, row_idx)
  # column
  column_tile <- get_tile_index(column_idx)
  tile_columns <- get_tile_indices(column_tile)
  # logic
  choice_other_rows <- x %>% 
    as_tibble() %>% 
    filter(.data[[.grid_value_name]] == value &
        .data[[.grid_row_name]] %in% tile_other_rows &
        .data[[.grid_column_name]] %in% tile_columns) %>% 
    pull({{.grid_row_name}}) %>% 
    unique()
}

#' @details
#' In retrospect,
#' `which_other_tile_rows_for_value` and `which_other_tile_columns_for_value()`
#' are redundant,
#' as
#' `value_required_in_other_tile_row()` and `value_required_in_other_tile_row()` 
#' return `FALSE` anyway if the value is not a choice is not valid for another row or column.
#' 
#' @importFrom dplyr filter pull
#' @importFrom rlang .data
#' @importFrom tibble as_tibble
#' 
#' @rdname INTERNAL_update_choices
which_other_tile_columns_for_value <- function(x, row_idx, column_idx, value) {
  # row
  row_tile <- get_tile_index(row_idx)
  tile_rows <- get_tile_indices(row_tile)
  # column
  column_tile <- get_tile_index(column_idx)
  tile_columns <- get_tile_indices(column_tile)
  tile_other_columns <- setdiff(tile_columns, column_idx)
  # logic
  choice_other_columns <- x %>% 
    as_tibble() %>% 
    filter(.data[[.grid_value_name]] == value &
        .data[[.grid_column_name]] %in% tile_other_columns &
        .data[[.grid_row_name]] %in% tile_rows) %>% 
    pull({{ .grid_column_name }}) %>% 
    unique()
}

#' @importFrom dplyr filter pull
#' @importFrom rlang .data
#' 
#' @rdname INTERNAL_update_choices
value_required_in_other_tile_row <- function(x, row_idx, column_idx, value) {
  # row
  row_tile <- get_tile_index(row_idx)
  tile_rows <- get_tile_indices(row_tile)
  tile_other_rows <- setdiff(tile_rows, row_idx)
  # column
  column_tile <- get_tile_index(column_idx)
  tile_columns <- get_tile_indices(column_tile)
  other_tiles_columns <- setdiff(1:9, tile_columns)
  # logic
  for (other_row_idx in tile_other_rows) {
    choices_in_other_row_in_tile <- x %>% 
      as_tibble() %>% 
      filter(.data[[.grid_row_name]] %in% other_row_idx &
          .data[[.grid_column_name]] %in% tile_columns) %>% 
      pull({{ .grid_value_name }}) %>% 
      unique()
    choices_in_other_row_in_other_tiles <- x %>% 
      as_tibble() %>% 
      filter(.data[[.grid_row_name]] %in% other_row_idx &
          .data[[.grid_column_name]] %in% other_tiles_columns) %>% 
      pull({{ .grid_value_name }}) %>% 
      unique()
    if (value %in% choices_in_other_row_in_tile &&
        !value %in% choices_in_other_row_in_other_tiles) {
      return(TRUE)
    }
  }
  return(FALSE)
}
#' @importFrom dplyr filter pull
#' @importFrom rlang .data
#' 
#' @rdname INTERNAL_update_choices
value_required_in_other_tile_column <- function(x, row_idx, column_idx, value) {
  # row
  row_tile <- get_tile_index(row_idx)
  tile_rows <- get_tile_indices(row_tile)
  other_tiles_rows <- setdiff(1:9, tile_rows)
  # column
  column_tile <- get_tile_index(column_idx)
  tile_columns <- get_tile_indices(column_tile)
  tile_other_columns <- setdiff(tile_columns, column_idx)
  # logic
  for (other_column_idx in tile_other_columns) {
    choices_in_other_column_in_tile <- x %>% 
      as_tibble() %>% 
      filter(.data[[.grid_column_name]] %in% other_column_idx &
          .data[[.grid_row_name]] %in% tile_rows) %>% 
      pull({{ .grid_value_name }}) %>% 
      unique()
    choices_in_other_column_in_other_tiles <- x %>% 
      as_tibble() %>% 
      filter(.data[[.grid_column_name]] %in% other_column_idx &
          .data[[.grid_row_name]] %in% other_tiles_rows) %>% 
      pull({{ .grid_value_name }}) %>% 
      unique()
    if (value %in% choices_in_other_column_in_tile &&
        !value %in% choices_in_other_column_in_other_tiles) {
      return(TRUE)
    }
  }
  return(FALSE)
}

#' @rdname INTERNAL_update_choices
value_required_elsewhere_in_tile <- function(x, row_idx, column_idx, value) {
  return(
    value_required_in_other_tile_column(x, row_idx, column_idx, value) ||
    value_required_in_other_tile_column(x, row_idx, column_idx, value)
  )
}

#' @param firstpass Logical scalar indicating whether the choices have been computed for all cells yet.
#'
#' @importFrom dplyr filter pull
#' @importFrom rlang .data
#' 
#' @rdname INTERNAL_update_choices
compute_cell_choices <- function(x, row_idx, column_idx, firstpass) {
  grid_value <- x %>% 
    as_tibble() %>% 
    filter(.data[[.grid_row_name]] == row_idx &
        .data[[.grid_column_name]] == column_idx) %>% 
    pull({{ .grid_value_name }})
  
  if (!is.na(grid_value)) {
    return(grid_value)
  }
  
  choices <- 1:9
  
  row_values_used <- x %>% 
    filter(.data[[.grid_row_name]] == row_idx &
        !is.na(.data[[.grid_value_name]])) %>% 
    pull({{ .grid_value_name }})
  choices <- setdiff(choices, row_values_used)
  
  column_values_used <- x %>% 
    as_tibble() %>% 
    filter(.data[[.grid_column_name]] == column_idx &
        !is.na(.data[[.grid_value_name]])) %>% 
    pull({{ .grid_value_name }})
  choices <- setdiff(choices, column_values_used)
  
  tile_values_used <- get_tile_values(x, row_idx, column_idx)
  choices <- setdiff(choices, tile_values_used)
  
  if (!firstpass) {
    exclude <- vapply(choices, value_required_elsewhere_in_tile, logical(1),
      x = x, row_idx = row_idx, column_idx = column_idx)
    choices <- choices[!exclude]
  }
  
  choices
}

#' @importFrom dplyr arrange bind_rows filter
#' @importFrom rlang .data
#' @importFrom tibble as_tibble
#' 
#' @rdname INTERNAL_update_choices
update_choices_xy <- function(x, row_idx, column_idx, values) {
  if (length(values) == 0) {
    return(x)
  }
  x %>% 
    filter(!(.data[[.grid_row_name]] == row_idx & .data[[.grid_column_name]] == column_idx)) %>% 
    bind_rows(tibble(
      "{ .grid_row_name }" := row_idx,
      "{ .grid_column_name }" := column_idx,
      "{ .grid_value_name }" := values,
      "{ .grid_given_name }" := FALSE
    )) %>% 
    arrange({{ .grid_row_name }}, {{ .grid_column_name }})
}

#' @importFrom rlang .data
update_choices_all <- function(x, firstpass) {
  for (row_idx in 1:9) {
    for (column_idx in 1:9) {
      sudoku_grid_value <- x %>% 
        filter(.data[[.grid_row_name]] == row_idx & .data[[.grid_column_name]] == column_idx) %>% 
        pull({{ .grid_value_name }})
      if (identical(length(sudoku_grid_value), 1L) && !is.na(sudoku_grid_value)) {
        next
      } else {
        choices <- compute_cell_choices(x, row_idx, column_idx, firstpass)
        x <- update_choices_xy(x, row_idx, column_idx, choices)
      }
    }
  }
  x %>% 
    arrange({{ .grid_row_name }}, {{ .grid_column_name }})
}
