test_that("simulate_grid(return_type = 'matrix') produces a matrix object", {
  out <- simulate_grid(return_type = "matrix")
  
  expect_type(out, "integer")
  expect_length(dim(out), 2L)
})

test_that("from_matrix()", {
  x <- simulate_grid(return_type = "matrix")
  out <- from_matrix(x)
  
  expect_s4_class(out, "SudokuGrid")
})

test_that("simulate_grid() produces a SudokuGrid object", {
  out <- simulate_grid()
  
  expect_s4_class(out, "SudokuGrid")
})

test_that("template_grid_code() outputs the template code", {
  expect_output(template_grid_code(), "m <- matrix(data = c(", fixed = TRUE)
})

test_that("replaceCellValues() replaces the values of a grid cell", {
  x <- simulate_grid()
  
  out <- replaceCellValues(x, 1, 1, 1:3)
  
  expect_identical(
    out %>% as_tibble() %>% filter(grid_row == 1 & grid_column == 1) %>% pull(grid_value),
    1:3
  )
})