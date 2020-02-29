test_that("The output should be a matrix", {
  string <- c(1,1,1,1,1,1)
  A <- matrix_create(string,2,2)

  expect_equal(class(A), "matrix")
})
