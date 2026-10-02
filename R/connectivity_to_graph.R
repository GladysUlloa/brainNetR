#' Convert a brain connectivity matrix to a graph
#'
#' Converts a square brain connectivity matrix into an igraph object
#' using a connectivity threshold.
#'
#' @param matrix A square numeric connectivity matrix.
#' @param threshold Numeric value used to retain connections.
#'
#' @return An igraph graph object.
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
#' connectivity_to_graph(mat, threshold = 0.3)
connectivity_to_graph <- function(matrix, threshold = 0.3) {

  if (!is.matrix(matrix)) {
    stop("`matrix` must be a matrix.")
  }

  if (!is.numeric(matrix)) {
    stop("`matrix` must contain numeric values.")
  }

  if (nrow(matrix) != ncol(matrix)) {
    stop("Connectivity matrix must be square.")
  }

  adjacency <- matrix

  adjacency[abs(adjacency) < threshold] <- 0

  diag(adjacency) <- 0

  igraph::graph_from_adjacency_matrix(
    adjacency,
    mode = "undirected",
    weighted = TRUE,
    diag = FALSE
  )
}
