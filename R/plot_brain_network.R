#' Plot a brain connectivity network
#'
#' Produces a simple visualization of a brain connectivity network
#' represented as an igraph object.
#'
#' @param graph An igraph graph object.
#' @param vertex_size Numeric value controlling node size.
#' @param show_labels Logical. Whether to display node labels.
#'
#' @return Invisibly returns the input graph.
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
#' plot_brain_network(g)
plot_brain_network <- function(
    graph,
    vertex_size = 20,
    show_labels = TRUE
) {

  if (!inherits(graph, "igraph")) {
    stop("`graph` must be an igraph object.")
  }

  labels <- if (show_labels) {
    igraph::V(graph)$name
  } else {
    NA
  }

  plot(
    graph,
    vertex.size = vertex_size,
    vertex.label = labels,
    edge.width = 2
  )

  invisible(graph)
}
