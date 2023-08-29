#' Wrap Choices in Cell
#'
#' @param x Integer vector of choices comprised of unique values from 1 to 9.
#'
#' @return A character scalar of choices wrapped over multiple lines if needed,
#' with a maximum of three choices per line.
#' @importFrom stringr str_flatten str_length str_split
#' @importFrom utils head
strwrap_choices <- function(x) {
  if (identical(x, NA_integer_)) return(NA)
  x <- str_flatten(x)
  lapply(
    split(
      str_split(x, "")[[1]],
      head(rep(1:3, each = 3), str_length(x))
    ),
    str_flatten
  ) %>% str_flatten(collapse = "\n")
}

summarise_choices_status <- function(x) {
  if (all(is.na(x))) {
    return(x)
  } else if (identical(as.character(x), "initial")) {
    return(x)
  } else if (all(as.character(x) == "candidate")) {
    return(x[1])
  } else if (identical(as.character(x), "answer")) {
    return(x)
  } else {
    stop("Invalid set of values for status")
  }
}
