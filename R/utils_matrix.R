#' Create a SudokuGrid Object
#'
#' @param object Numeric matrix.
#' Must be nine rows and nine columns,
#' filled with either integer from 1 to 9 or `NA`.
#'
#' @return A `sudoku` object.
#' @export
#' @importFrom dplyr mutate mutate_at pick rename_with
#' @importFrom methods new
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect matches
#' @importFrom magrittr %>%
#' @importFrom rlang := .data
#'
#' @examples
#' template_grid_code()
#' 
#' m <- simulate_grid(return_type = "matrix")
#' 
#' sudoku_grid <- as_sudoku(m)
#' sudoku_grid
as_sudoku.matrix <- function(object) {
  x_tbl <- as_tibble(object, .name_repair = "minimal")
  colnames(x_tbl) <- as.character(1:9)
  x_tbl[[.grid_row_name]] <- as.character(1:9)
  x_tbl <- x_tbl %>% 
    pivot_longer(matches("[[:digit:]]"), names_to = .grid_column_name, values_to = .grid_value_name) %>% 
    mutate_at(c(.grid_row_name, .grid_column_name), as.integer)
  x_tbl[[.grid_status_name]] <- factor(
    ifelse(is.na(x_tbl[[.grid_value_name]]), NA, "initial"),
    c("initial", "candidate", "answer"))
  x_tbl <- as_sudoku(x_tbl)
  x_tbl
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
#' @rdname as_sudoku.matrix
template_grid_code <- function() {
  cat(.template)
}
