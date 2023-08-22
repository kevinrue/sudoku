#' A sudoku grid
#'
#' @slot contents tibble. 
#'
#' @return An object of class `SudokuGrid`.
#' @importClassesFrom tibble tbl_df
#' @importFrom methods setClass
#'
#' @examples
#' new("SudokuGrid")
setClass("SudokuGrid", 
  slots=c(
    contents="tbl_df"
  )
)
