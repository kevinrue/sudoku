#' Wrap Choices in Cell
#'
#' @param x Integer vector of choices comprised of unique values from 1 to 9.
#'
#' @return A character scalar of choices wrapped over multiple lines if needed,
#' with a maximum of three choices per line.
#' @importFrom stringr str_flatten str_length str_split
#' @importFrom utils head
strwrap_choices <- function(x) {
  x <- str_flatten(x)
  lapply(
    split(
      str_split(x, "")[[1]],
      head(rep(1:3, each = 3), str_length(x))
    ),
    str_flatten
  ) %>% str_flatten(collapse = "\n")
}
