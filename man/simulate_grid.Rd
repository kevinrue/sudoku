% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_misc.R
\name{simulate_grid}
\alias{simulate_grid}
\title{Simulate a sudoku grid}
\usage{
simulate_grid(return_type = c("SudokuGrid", "matrix"))
}
\arguments{
\item{return_type}{Character scalar. Type of output to return.}
}
\value{
For \code{return_type="SudokuGrid"} (default), a \code{sudoku} object.

For \code{return_type="matrix"}, an integer matrix.
}
\description{
Simulate a sudoku grid
}
\examples{
simulate_grid()
simulate_grid(return_type = "matrix")
}
