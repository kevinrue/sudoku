#' Create a SudokuGrid Object
#'
#' @param matrix Numeric matrix.
#' Must be nine rows and nine columns,
#' filled with either integer from 1 to 9 or `NA`.
#'
#' @return A [SudokuGrid-class] object.
#' @export
#'
#' @examples
#' m <- matrix(data = c(
#'   2,NA,NA, 1, 9,NA,NA,NA, 4,
#'   NA, 1,NA,NA, 8,NA,NA,NA,NA,
#'   NA,NA, 5,NA,NA,NA, 3,NA,NA,
#'   6,NA,NA,NA,NA, 3,NA,NA,NA,
#'   4, 3,NA,NA,NA,NA,NA, 9, 1,
#'   NA,NA,NA, 2,NA,NA,NA,NA, 8,
#'   NA,NA, 7,NA,NA,NA, 5,NA,NA,
#'   NA,NA,NA,NA, 4,NA,NA, 7,NA,
#'   1,NA,NA,NA, 7, 6,NA,NA, 3
#'   ), nrow = 9, ncol = 9, byrow = TRUE)
from_matrix <- function(matrix) {
  stop("TODO")
}

.template <- "m <- matrix(data = c(
  NA,NA,NA,NA,NA,NA,NA,NA,NA,
  NA,NA,NA,NA,NA,NA,NA,NA,NA,
  NA,NA,NA,NA,NA,NA,NA,NA,NA,
  NA,NA,NA,NA,NA,NA,NA,NA,NA,
  NA,NA,NA,NA,NA,NA,NA,NA,NA,
  NA,NA,NA,NA,NA,NA,NA,NA,NA,
  NA,NA,NA,NA,NA,NA,NA,NA,NA,
  NA,NA,NA,NA,NA,NA,NA,NA,NA,
  NA,NA,NA,NA,NA,NA,NA,NA,NA,
), nrow = 9, ncol = 9, byrow = TRUE)"


#' @return `template_grid_code()` displays template code that can be used
#' to initialise a sudoku grid.
#' @export
#' @rdname from_matrix
template_grid_code <- function() {
  cat(.template)
}
