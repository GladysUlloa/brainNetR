test_that("brain_network_metrics returns expected metrics", {

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

  metrics <- brain_network_metrics(g)

  expect_s3_class(metrics, "data.frame")
  expect_equal(metrics$n_nodes, 3)
  expect_equal(metrics$n_edges, 2)
  expect_true(all(
    c(
      "mean_degree",
      "clustering_coefficient",
      "global_efficiency",
      "modularity"
    ) %in% names(metrics)
  ))
})
