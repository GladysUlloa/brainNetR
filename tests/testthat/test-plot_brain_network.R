test_that("plot_brain_network accepts an igraph object", {

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

  expect_invisible(
    plot_brain_network(g)
  )
})
