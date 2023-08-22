#' Create a SudokuGrid Object
#'
#' @param x Numeric matrix.
#' Must be nine rows and nine columns,
#' filled with either integer from 1 to 9 or `NA`.
#'
#' @return A [SudokuGrid-class] object.
#' @export
#' @importFrom dplyr mutate_at pick rename_with
#' @importFrom methods new
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect matches
#' @importFrom magrittr %>%
#'
#' @examples
#' template_grid_code()
#' 
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
#' sudoku_grid
from_matrix <- function(x) {
  x_tbl <- as_tibble(x, .name_repair = "minimal")
  colnames(x_tbl) <- as.character(1:9)
  x_tbl[[.grid_row_name]] <- as.character(1:9)
  x_tbl <- x_tbl %>% 
    pivot_longer(matches("[[:digit:]]"), names_to = .grid_column_name, values_to = .grid_value_name) %>% 
    mutate_at(c(.grid_row_name, .grid_column_name), as.numeric)
  x_tbl[[.grid_given_name]] <- ifelse(is.na(x_tbl[[.grid_value_name]]), NA, TRUE)
  new("SudokuGrid", x_tbl)
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
