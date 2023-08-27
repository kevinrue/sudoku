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

test_that("get_tile_values() returns the expected values", {
  x <- simulate_grid()
  
  out <- sudoku:::get_tile_values(x, 1, 1)
  
  expect_identical(out, c(2L, 1L, 5L))
})

test_that("only_cell_in_tile_row_for_value() return TRUE/FALSE when expected", {
  x <- simulate_grid()
  
  out <- sudoku:::only_cell_in_tile_row_for_value(x, 1, 1, 2)
  
  expect_true(out)
})

test_that("only_cell_in_tile_column_for_value() return TRUE/FALSE when expected", {
  x <- simulate_grid()
  
  out <- sudoku:::only_cell_in_tile_column_for_value(x, 1, 1, 2)
  
  expect_true(out)
})

test_that("which_other_tile_rows_for_value returns the expected indices", {
  x <- simulate_grid()
  
  out <- sudoku:::which_other_tile_rows_for_value(x, 1, 1, 5)
  
  expect_identical(out, 3L)
})


test_that("which_other_tile_columns_for_value returns the expected indices", {
  x <- simulate_grid()
  
  out <- sudoku:::which_other_tile_columns_for_value(x, 1, 1, 5)
  
  expect_identical(out, 3L)
})

test_that("value_required_in_other_tile_row return TRUE/FALSE when expected", {
  x <- simulate_grid()
  
  out <- sudoku:::value_required_in_other_tile_row(x, 1, 1, 5)
  
  expect_true(out)
})

test_that("value_required_in_other_tile_row return TRUE/FALSE when expected", {
  x <- simulate_grid()
  
  out <- sudoku:::value_required_in_other_tile_column(x, 1, 1, 5)
  
  expect_true(out)
})

test_that("value_required_elsewhere_in_tile return TRUE/FALSE when expected", {
  x <- simulate_grid()
  
  out <- sudoku:::value_required_elsewhere_in_tile(x, 1, 1, 5)
  
  expect_true(out)
})
