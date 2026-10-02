#' Compute basic brain network metrics
#'
#' Computes a set of basic graph metrics from an igraph object
#' representing a brain connectivity network.
#'
#' @param graph An igraph graph object.
#'
#' @return A data frame containing basic network metrics.
#'
#' @export
#'
#' @examples
#' mat <- matrix(
#'   c(
#'     1, 0.7, 0.2,
#'     0.7, 1, 0.5,
#'     0.2, 0.5, 1
#'   ),
#'   nrow = 3,
#'   byrow = TRUE
#' )
#'
#' g <- connectivity_to_graph(mat, threshold = 0.3)
#' brain_network_metrics(g)
brain_network_metrics <- function(graph) {

  if (!inherits(graph, "igraph")) {
    stop("`graph` must be an igraph object.")
  }

  n_nodes <- igraph::vcount(graph)

  n_edges <- igraph::ecount(graph)

  mean_degree <- mean(
    igraph::degree(graph)
  )

  clustering_coefficient <- igraph::transitivity(
    graph,
    type = "global"
  )

  global_efficiency <- igraph::global_efficiency(
    graph,
    weights = NA
  )

  community <- igraph::cluster_louvain(
    graph,
    weights = NA
  )

  modularity <- igraph::modularity(
    community
  )

  data.frame(
    n_nodes = n_nodes,
    n_edges = n_edges,
    mean_degree = mean_degree,
    clustering_coefficient = clustering_coefficient,
    global_efficiency = global_efficiency,
    modularity = modularity
  )
}
