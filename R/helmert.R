#' Internal Helmert 2D Transformation Solver
#'
#' @description Calculates the four parameters of a 2D similarity (Helmert)
#'   transformation using a standard, numerically stable least-squares solution
#'   based on centroids. This is an internal function called by `train_pai_model`.
#'
#' @param source_x Numeric vector of approximate ('from') x coordinates.
#' @param source_y Numeric vector of approximate ('from') y coordinates.
#' @param target_x Numeric vector of actual ('to') x coordinates.
#' @param target_y Numeric vector of actual ('to') y coordinates.
#'
#' @return An object of class `helmert` containing the calculated coefficients
#'   and the original data centroids, ready for use by `predict.pai_model`.
#'
#' @keywords internal
helmert <- function(source_x, source_y, target_x, target_y) {
  # Calculate centroids of both source and target points
  u_mean <- mean(source_x)
  v_mean <- mean(source_y)
  x_mean <- mean(target_x)
  y_mean <- mean(target_y)

  # Calculate centered coordinates for both systems
  u_i <- source_x - u_mean
  v_i <- source_y - v_mean
  x_i <- target_x - x_mean
  y_i <- target_y - y_mean

  # Solve for parameters 'a' and 'b' using the centered coordinates
  # This is the standard least-squares solution for the rotation and scale part.
  # a = s * cos(theta)
  # b = s * sin(theta)
  denominator <- sum(u_i^2 + v_i^2)
  if (abs(denominator) < 1e-9) {
    stop("Cannot solve Helmert transformation: source points are co-located.", call. = FALSE)
  }
  a <- (sum(u_i * x_i) + sum(v_i * y_i)) / denominator
  b <- (sum(u_i * y_i) - sum(v_i * x_i)) / denominator

  # Create the final model object containing the essential parameters
  model <- structure(
    list(
      coefficients = c(a = a, b = b),
      centroids = c(u_mean = u_mean, v_mean = v_mean, x_mean = x_mean, y_mean = y_mean)
    ),
    class = "helmert"
  )

  return(model)
}
