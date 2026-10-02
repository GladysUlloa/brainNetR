#' Plot a brain connectivity network
#'
#' Produces a visualization of a brain connectivity network represented
#' as an igraph object. Edge width reflects connectivity strength.
#'
#' @param graph An igraph graph object.
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
plot_brain_network <- function(graph) {
  if (!inherits(graph, "igraph")) {
    stop("`graph` must be an igraph object.")
  }

  # Layout más profesional
  layout_coords <- igraph::layout_with_fr(graph)

  # Tamaño de aristas según pesos
  if (!is.null(igraph::E(graph)$weight)) {
    w <- igraph::E(graph)$weight
    edge_width <- 1 + 5 * w / max(w)
  } else {
    edge_width <- rep(2, igraph::ecount(graph))
  }

  # Tamaño de nodos según grado
  deg <- igraph::degree(graph)
  vertex_size <- 22 + 4 * deg

  # Plot
  plot(
    graph,
    layout = layout_coords,
    vertex.size = vertex_size,
    vertex.color = "gold",
    vertex.frame.color = "gray30",
    vertex.label.color = "black",
    vertex.label.cex = 1,
    vertex.label.family = "sans",
    edge.width = edge_width,
    edge.color = "gray60",
    edge.curved = 0.1,
    main = "Brain Connectivity Network",
    margin = 0.2
  )

  invisible(graph)
}
