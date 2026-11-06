test_that("preprocess_connectivity returns a valid matrix", {

  mat <- matrix(
    c(
      1, 0.8, 0.2,
      0.8, 1, 0.5,
      0.2, 0.5, 1
    ),
    nrow = 3,
    byrow = TRUE
  )

  result <- preprocess_connectivity(mat)

  expect_true(is.matrix(result))
  expect_equal(diag(result), c(0, 0, 0))
  expect_equal(result, t(result))
})


test_that("preprocess_connectivity detects invalid matrices", {

  non_square <- matrix(1:6, nrow = 2)

  expect_error(
    preprocess_connectivity(non_square),
    "square"
  )

  with_na <- matrix(
    c(
      1, NA,
      NA, 1
    ),
    nrow = 2
  )

  expect_error(
    preprocess_connectivity(with_na),
    "missing"
  )

  asymmetric <- matrix(
    c(
      1, 0.8,
      0.2, 1
    ),
    nrow = 2,
    byrow = TRUE
  )

  expect_error(
    preprocess_connectivity(asymmetric),
    "symmetric"
  )
})
