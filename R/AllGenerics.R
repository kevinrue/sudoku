#' Replace Possible Values in SudokuGrid Cell
#' 
#' Replace the set of values possible in a SudokuGrid cell.
#'
#' @param object A `sudoku` object.
#' @param i Row index.
#' @param j Column index.
#' @param values Integer vectors comprising values from 1 to 9.
#' @param given Whether the values were given in the starting grid.
#' Default to `TRUE` if a `value` has length 1.
#'
#' @return The updated `sudoku` object.
#' 
#' @export
#' @importFrom dplyr arrange bind_rows filter pick
#' @importFrom rlang := .data
#' @importFrom tibble tibble
#' @importFrom tidyselect all_of
#' 
#' @rdname INTERNAL_replaceCellValues
replace_cell_values <- function(object, i, j, values, given) UseMethod("replace_cell_values")

#' @param value Integer scalar from 1 to 9. Display only that value.
#' 
#' @export
#' 
#' @rdname plot.sudoku
plot_value <- function(x, value, ...) UseMethod("plot_value")

#' Coerce Objects to sudoku
#'
#' @param object Object to coerce to `sudoku`.
#' 
#' @export
#' 
#' @rdname as_sudoku
as_sudoku <- function(object) UseMethod("as_sudoku")

