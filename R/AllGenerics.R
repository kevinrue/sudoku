#' Generics for SudokuGrid
#'
#' @param object A [SudokuGrid-class] object.
#' @param i Row index.
#' @param j Column index.
#' @param values Integer vectors comprising values from 1 to 9.
#' @param ... Additional arguments passed to and from other methods.
#'
#' @return `replaceCellValues()` returns the updated [SudokuGrid-class] object.
#' @export
#'
#' @examples
#' showMethods("replaceCellValues")
setGeneric("replaceCellValues", function(object, i, j, values, ...) standardGeneric("replaceCellValues"))