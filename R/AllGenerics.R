#' Replace Possible Values in SudokuGrid Cell
#' 
#' Replace the set of values possible in a SudokuGrid cell.
#'
#' @param object A `sudoku` object.
#' @param i Row index.
#' @param j Column index.
#' @param values Integer vectors comprising values from 1 to 9.
#' @param status Factor indicating whether the value
#' was given initially (`initial`),
#' is one of several candidate values (`candidate`),
#' or was identified as the answer (`anwer`).
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
replace_cell_values <- function(object, i, j, values, status) UseMethod("replace_cell_values")

#' @param value Integer scalar from 1 to 9. Display only that value.
#' 
#' @export
#' 
#' @rdname plot.sudoku
plot_value <- function(x, value, ...) UseMethod("plot_value")

#' @export
#' 
#' @rdname plot.sudoku
plot_choices <- function(x, ...) UseMethod("plot_choices")

#' Coerce Objects to sudoku
#'
#' @param object Object to coerce to `sudoku`.
#' 
#' @export
#' 
#' @rdname as_sudoku
as_sudoku <- function(object) UseMethod("as_sudoku")

