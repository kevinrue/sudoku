as_sudoku.tbl_df <- function(object) {
  class(object) <- c("sudoku", class(object))
  object
}
