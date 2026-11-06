#' Preprocess a brain connectivity matrix
#'
#' Validates and prepares a brain connectivity matrix before network
#' construction and analysis.
#'
#' The function checks whether the input is a numeric square matrix,
#' verifies missing or infinite values, optionally checks symmetry,
#' and sets the diagonal to zero.
#'
#' @param matrix A numeric square connectivity matrix.
#' @param check_symmetry Logical. If TRUE, checks whether the matrix
#'   is symmetric. Default is TRUE.
#' @param zero_diagonal Logical. If TRUE, sets the diagonal values
#'   to zero. Default is TRUE.
#' @param tolerance Numeric tolerance used when checking symmetry.
#'   Default is 1e-8.
#'
#' @return A validated and preprocessed connectivity matrix.
#'
#' @examples
#' mat <- matrix(
#'   c(
#'     1, 0.8, 0.2,
#'     0.8, 1, 0.5,
#'     0.2, 0.5, 1
#'   ),
#'   nrow = 3,
#'   byrow = TRUE
#' )
#'
#' preprocess_connectivity(mat)
#'
#' @export
preprocess_connectivity <- function(
    matrix,
    check_symmetry = TRUE,
    zero_diagonal = TRUE,
    tolerance = 1e-8
) {

  if (!is.matrix(matrix)) {
    stop("Input must be a matrix.")
  }

  if (!is.numeric(matrix)) {
    stop("Connectivity matrix must contain numeric values.")
  }

  if (nrow(matrix) != ncol(matrix)) {
    stop("Connectivity matrix must be square.")
  }

  if (anyNA(matrix)) {
    stop("Connectivity matrix contains missing values (NA).")
  }

  if (any(!is.finite(matrix))) {
    stop("Connectivity matrix contains infinite or non-finite values.")
  }

  if (check_symmetry) {
    symmetric <- isTRUE(
      all.equal(
        matrix,
        t(matrix),
        tolerance = tolerance,
        check.attributes = FALSE
      )
    )

    if (!symmetric) {
      stop("Connectivity matrix must be symmetric.")
    }
  }

  if (zero_diagonal) {
    diag(matrix) <- 0
  }

  matrix
}

