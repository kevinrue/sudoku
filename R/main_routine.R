run_solver <- function(x, max_iter = Inf, do.plot = FALSE, do.message = FALSE) {
  if (do.plot) print(plot(x))
  firstpass <- TRUE
  n_filled <- sum(!is.na(x[[.grid_value_name]]))
  while(any(is.na(x[[.grid_value_name]])) & max_iter > 0) {
    if (firstpass) {
      x <- update_choices_all(x, firstpass)
      firstpass <- FALSE
    }
    for (row in 1:9) {
      if (identical(max_iter, 0L)) break
      for (column in 1:9) {
        if (identical(max_iter, 0L)) break
        if (any(x %>% filter(.data[[.grid_row_name]] == row &
            .data[[.grid_column_name]] == column) %>% pull({{ .grid_status_name }}) != "candidate")) {
          next
        }
        # eliminate choices from 1:9
        remaining_choices <- eliminate_competing_choices_xy(x, row, column)
        if (length(remaining_choices) == 1) {
          if (do.message) {
            message("== remaining choices ==")
            message("row: ", row, ", column: ", column, ", only_choice: ", remaining_choices)
          }
          x <- replace_cell_values(x, row, column, remaining_choices, status = "answer")
          if (do.plot) print(plot(x))
          Sys.sleep(0.5)
          x <- update_choices_all(x, firstpass)
          next
        }
        # only cell in tile for this value?
        cell_choices <- get_cell_choices(x, row, column)
        only_cell <- vapply(cell_choices, only_cell_in_tile_for_value, logical(1),
          x = x,
          row_idx = row,
          column_idx = column)
        value <- cell_choices[only_cell]
        if (sum(only_cell) == 1) {
          if (do.message) {
            message("== only cell in tile ==")
            message("row: ", row, ", column: ", column, ", value: ", value)
          }
          x <- replace_cell_values(x, row, column, value, status = "answer")
          if (do.plot) print(plot(x))
          Sys.sleep(0.5)
          x <- update_choices_all(x, firstpass)
          next
        }
        # only cell in column for value?
        # only_cell <- vapply(cell_choices, only_cell_in_column, logical(1), .choices = sudoku_choices, .row = row, .column = column)
        # value <- cell_choices[only_cell]
        # message("== only cell ==")
        # message("row: ", row, ", column: ", column, ", value: ", value)
        # if (sum(only_cell) == 1) {
        #   if (prompt) {
        #     continue <- prompt_next()
        #   } else {
        #     continue <- TRUE
        #   }
        #   if (continue) {
        #     x <- add_value_xy(x, row, column, value, status = "answer")
        #     print(plot(x))
        #     Sys.sleep(0.5)
        #     x <- update_choices_all(x, firstpass)
        #     next
        #   } else {
        #     break
        #   }
        # }
        # only cell in row for value?
        # only_cell <- vapply(cell_choices, only_cell_in_row, logical(1), .choices = sudoku_choices, .row = row, .column = column)
        # value <- cell_choices[only_cell]
        # message("== only cell ==")
        # message("row: ", row, ", column: ", column, ", value: ", value)
        # if (sum(only_cell) == 1) {
        #   if (prompt) {
        #     continue <- prompt_next()
        #   } else {
        #     continue <- TRUE
        #   }
        #   if (continue) {
        #     x <- add_value_xy(x, row, column, value, status = "answer")
        #     print(plot(x))
        #     Sys.sleep(0.5)
        #     x <- update_choices_all(x, firstpass)
        #     next
        #   } else {
        #     break
        #   }
        # }
      } # for (column in 1:9)
    } # for (row in 1:9)
    new_n_filled <- sum(!is.na(x[[.grid_value_name]]))
    if (max_iter > 0 && identical(new_n_filled, n_filled)) {
      message("No progress in last round. No point trying again.")
      max_iter <- 0 # redundant
      break
    }
    n_filled <- new_n_filled
    max_iter <- max_iter - 1
  }
  if (all(x[[.grid_status_name]] %in% c("initial", "answer"))) {
    message("Grid completed!")
  }
  if (do.plot) print(plot(x))
  return(x)
}