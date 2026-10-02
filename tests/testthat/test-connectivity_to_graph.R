test_that("connectivity_to_graph creates an igraph object", {

  mat <- matrix(
    c(
      1, 0.7, 0.2,
      0.7, 1, 0.5,
      0.2, 0.5, 1
    ),
    nrow = 3,
    byrow = TRUE
  )

  g <- connectivity_to_graph(
    mat,
    threshold = 0.3
  )

  expect_s3_class(g, "igraph")
  expect_equal(igraph::vcount(g), 3)
  expect_equal(igraph::ecount(g), 2)
})
