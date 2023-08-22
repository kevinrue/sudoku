#' Methods for SudokuGrid
#'
#' @param object A [SudokuGrid-class] object.
#' @param i Row index.
#' @param j Column index.
#' @param values Integer vectors comprising values from 1 to 9.
#' @param given Whether the values were given in the starting grid.
#' Default to `TRUE` if a `value` has length 1.
#'
#' @return The updated [SudokuGrid-class] object.
#' @export
#' @importFrom dplyr arrange bind_rows filter pick
#' @importFrom tibble tibble
#' @importFrom tidyselect all_of
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
#' 
#' sudoku_grid <- from_matrix(m)
#' 
#' replaceCellValues(sudoku_grid, 1, 2, c(1, 2, 3))
setMethod("replaceCellValues", "SudokuGrid", function(object, i, j, values,
  given=ifelse(identical(length(values), 1L), TRUE, FALSE)
){
  added_values <- tibble(
    grid_row = i, # TODO: <data-masking>
    grid_column = j, # TODO: <data-masking>
    grid_value = values, # TODO: <data-masking>
    given = given # TODO: <data-masking>
  )
  object %>% 
    as_tibble() %>% 
    filter(!(pick(all_of(.grid_row_name)) == i & pick(all_of(.grid_column_name)) == j)) %>% 
    bind_rows(added_values) %>% 
    arrange(pick(all_of(c(.grid_row_name, .grid_column_name))))
})
