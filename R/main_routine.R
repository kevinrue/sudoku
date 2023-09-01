run_solver <- function(x, max_iter = Inf, do.plot = FALSE, do.message = FALSE, do.prompt = FALSE) {
  if (do.plot) print(plot(x))
  x <- update_choices_all(x, TRUE)
  n_to_fill <- n_cells_to_fill(x)
  if (do.message) {
    message(sprintf("* Missing values left: %s", n_to_fill))
  }
  while(n_to_fill > 0 & max_iter > 0) {
    x <- update_choices_all(x, FALSE)
    for (row in 1:9) {
      if (identical(as.integer(max_iter), 0L)) break
      for (column in 1:9) {
        if (identical(as.integer(max_iter), 0L)) break
        if (!all(x %>% filter(.data[[.grid_row_name]] == row &
            .data[[.grid_column_name]] == column) %>% pull({{ .grid_status_name }}) == "candidate")) {
          next
        }
        # eliminate choices from 1:9
        remaining_choices <- eliminate_competing_choices_xy(x, row, column)
        if (length(remaining_choices) == 1) {
          continue <- ifelse(do.prompt, prompt_next(), TRUE)
          if (do.message) {
            message("== remaining choices ==")
            message("row: ", row, ", column: ", column, ", only_choice: ", remaining_choices)
          }
          x <- replace_cell_values(x, row, column, remaining_choices, status = "answer")
          max_iter <- max_iter - 1
          if (do.plot) print(plot(x))
          Sys.sleep(0.5)
          x <- update_choices_all(x, FALSE)
          next
        }
        # only cell in tile for this value?
        cell_choices <- get_cell_choices(x, row, column)
        only_cell <- vapply(cell_choices, only_cell_in_tile_for_value, logical(1),
          x = x, row_idx = row, column_idx = column)
        value <- cell_choices[only_cell]
        if (sum(only_cell) == 1) {
          continue <- ifelse(do.prompt, prompt_next(), TRUE)
          if (do.message) {
            message("== only cell in tile ==")
            message("row: ", row, ", column: ", column, ", value: ", value)
          }
          x <- replace_cell_values(x, row, column, value, status = "answer")
          max_iter <- max_iter - 1
          if (do.plot) print(plot(x))
          Sys.sleep(0.5)
          x <- update_choices_all(x, FALSE)
          next
        }
        # only cell in column for value?
        only_cell <- vapply(cell_choices, only_cell_in_column_for_value, logical(1),
          x = x, row_idx = row, column_idx = column)
        value <- cell_choices[only_cell]
        if (sum(only_cell) == 1) {
          continue <- ifelse(do.prompt, prompt_next(), TRUE)
          if (do.message) {
            message("== only cell in column ==")
            message("row: ", row, ", column: ", column, ", value: ", value)
          }
          x <- replace_cell_values(x, row, column, value, status = "answer")
          max_iter <- max_iter - 1
          if (do.plot) print(plot(x))
          Sys.sleep(0.5)
          x <- update_choices_all(x, FALSE)
          next
        }
        # only cell in row for value?
        only_cell <- vapply(cell_choices, only_cell_in_row_for_value, logical(1),
          x = x, row_idx = row, column_idx = column)
        value <- cell_choices[only_cell]
        if (sum(only_cell) == 1) {
          continue <- ifelse(do.prompt, prompt_next(), TRUE)
          if (do.message) {
            message("== only cell in row ==")
            message("row: ", row, ", column: ", column, ", value: ", value)
          }
          x <- replace_cell_values(x, row, column, value, status = "answer")
          max_iter <- max_iter - 1
          if (do.plot) print(plot(x))
          Sys.sleep(0.5)
          x <- update_choices_all(x, FALSE)
          next
        }
      } # for (column in 1:9)
    } # for (row in 1:9)
    new_n_to_fill <- n_cells_to_fill(x)
    if (max_iter > 0 && identical(new_n_to_fill, n_to_fill)) {
      message("No progress in last round. No point trying again.")
      max_iter <- 0 # redundant with break
      break
    }
    n_to_fill <- new_n_to_fill
    if (do.message) {
      message(sprintf("* Iterations left: %s", max_iter))
      message(sprintf("* Missing values left: %s", n_to_fill))
    }
  }
  if (all(x[[.grid_status_name]] %in% c("initial", "answer"))) {
    message("Grid completed!")
  }
  if (do.plot) print(plot(x))
  return(x)
}