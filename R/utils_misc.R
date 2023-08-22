#' Simulate a sudoku grid
#'
#' @param return_type Character scalar. Type of output to return.
#'
#' @return For `return_type="SudokuGrid"` (default), a [SudokuGrid-class] object.
#' 
#' For `return_type="matrix"`, an integer matrix.
#' @export
#'
#' @examples
#' simulate_grid()
#' simulate_grid(return_type = "matrix")
simulate_grid <- function(return_type = c("SudokuGrid", "matrix")) {
  return_type <- match.arg(return_type)
  m <- matrix(data = c(
    2L,NA,NA,1L,9L,NA,NA,NA,4L,
    NA,1L,NA,NA,8L,NA,NA,NA,NA,
    NA,NA,5L,NA,NA,NA,3L,NA,NA,
    6L,NA,NA,NA,NA,3L,NA,NA,NA,
    4L,3L,NA,NA,NA,NA,NA,9L,1L,
    NA,NA,NA,2L,NA,NA,NA,NA,8L,
    NA,NA,7L,NA,NA,NA,5L,NA,NA,
    NA,NA,NA,NA,4L,NA,NA,7L,NA,
    1L,NA,NA,NA,7L,6L,NA,NA,3L
  ), nrow = 9, ncol = 9, byrow = TRUE)
  if (identical(return_type, "matrix")) {
    return(m)
  }
  sudoku_grid <- from_matrix(m)
  if (identical(return_type, "SudokuGrid")) {
    return(sudoku_grid)
  }
}
