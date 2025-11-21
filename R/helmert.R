#' Helmert 2D Transformation Solver
#'
#' @description Calculates the four parameters of a 2D similarity (Helmert)
#'   transformation using a standard, numerically stable least-squares solution
#'   based on centroids.
#'
#' @param source_x Numeric vector of approximate ('from') x coordinates.
#' @param source_y Numeric vector of approximate ('from') y coordinates.
#' @param target_x Numeric vector of actual ('to') x coordinates.
#' @param target_y Numeric vector of actual ('to') y coordinates.
#'
#' @return An object of class `helmert` containing the calculated coefficients
#'   and the original data centroids.
#'
#' @export
#' @examples
#' # Sample data
#' source_coords <- data.frame(source_x = c(10, 20, 30, 15),
#'                            source_y = c(15, 25, 10, 5))
#'
#'target_coords <- data.frame(target_x = c(110.5, 119.8, 131.2, 114.0),
#'                          target_y = c(114.5, 125.2, 109.8, 104.9))
#'
#' # Calculate Helmert transformation
#' helmert_model <- helmert(
#'   source_x = source_coords$source_x,
#'   source_y = source_coords$source_y,
#'   target_x = target_coords$target_x,
#'   target_y = target_coords$target_y
#' )
#' print(helmert_model)
helmert <- function(source_x, source_y, target_x, target_y) {

  # ---  Input Validation ---
  input_validation(source_x, source_y, target_x, target_y)

  # --- Core Helmert Calculation ---

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
    stop("Cannot solve Helmert transformation: source points are co-located.",
         call. = FALSE)
  }

  a <- (sum(u_i * x_i) + sum(v_i * y_i)) / denominator
  b <- (sum(u_i * y_i) - sum(v_i * x_i)) / denominator

  # --- Create Model Object ---

  # Create the final model object containing the essential parameters
  model <- structure(
    list(
      coefficients = c(a = a, b = b),
      centroids = c(u_mean = u_mean, v_mean = v_mean,
                    x_mean = x_mean, y_mean = y_mean)
    ),
    class = "helmert"
  )

  return(model)
}


#' Print a Helmert Model Object
#'
#' @description S3 print method for objects of class `helmert`.
#' @param x An object of class `helmert`.
#' @param ... Additional arguments (not used).
#' @export
print.helmert <- function(x, ...) {
  cat("--- Helmert Transformation Model (OLS) ---\n\n")
  cat("Helmert Transformation Parameters:\n")
  print(round(x$coefficients, 6))
  print(round(x$centroids, 6))
  invisible(x)
}

#' Predict Helmert 2D Transformation
#'
#' @description Applies a trained Helmert 2D transformation to new source
#'   coordinates to predict their position in the target coordinate system.
#'
#' @param object An object of class `helmert`, as created by the `helmert()`
#'   function.
#' @param newdata A data frame containing the new source coordinates. It must
#'   have columns with the same names as the original source data, typically
#'   `source_x` and `source_y`.
#' @param ... Additional arguments (not used).
#'
#' @return A data frame with the predicted `target_x` and `target_y`
#'   coordinates.
#' @export
#'
#' @examples
#' # Create some sample data
#' source_coords <- data.frame(
#'   source_x = c(10, 20, 30, 15),
#'   source_y = c(15, 25, 10, 5)
#' )
#'
#' target_coords <- data.frame(
#'   target_x = c(110.5, 119.8, 131.2, 114.0),
#'   target_y = c(114.5, 125.2, 109.8, 104.9)
#' )
#'
#' # 1. Train the Helmert model
#' helmert_model <- helmert(
#'   source_x = source_coords$source_x,
#'   source_y = source_coords$source_y,
#'   target_x = target_coords$target_x,
#'   target_y = target_coords$target_y
#' )
#'
#' # 2. Define new points to transform
#' new_points <- data.frame(
#'   source_x = c(25, 5),
#'   source_y = c(18, 8)
#' )
#'
#' # 3. Predict the target coordinates for the new points
#' predicted_points <- predict(helmert_model, new_points)
#'
#' print(predicted_points)
predict.helmert <- function(object, newdata, ...) {

  # --- Input Validation ---
  if (!inherits(object, "helmert")) {
    stop("The 'object' provided must be of class 'helmert'.", call. = FALSE)
  }
  required_cols <- c("source_x", "source_y")
  if (!is.data.frame(newdata) || !all(required_cols %in% names(newdata))) {
    stop(
      "'newdata' must be a data frame with columns 'source_x' and 'source_y'.",
      call. = FALSE)
  }
  if (any(!is.finite(newdata$source_x)) || any(!is.finite(newdata$source_y))) {
    stop(
      "All 'source_x' and 'source_y' values in 'newdata' must be finite.",
         call. = FALSE)
  }

  # --- Extract Parameters ---

  # Extract coefficients (a, b) and centroids from the model object
  a <- object$coefficients["a"]
  b <- object$coefficients["b"]
  u_mean <- object$centroids["u_mean"]
  v_mean <- object$centroids["v_mean"]
  x_mean <- object$centroids["x_mean"]
  y_mean <- object$centroids["y_mean"]

  # Extract new source coordinates from the newdata data frame
  u_new <- newdata$source_x
  v_new <- newdata$source_y

  # --- Apply Transformation ---
  # The transformation is applied relative to the centroids for numerical
  # stability.
  pred_x <- x_mean + a * (u_new - u_mean) - b * (v_new - v_mean)
  pred_y <- y_mean + b * (u_new - u_mean) + a * (v_new - v_mean)

  # --- Return Results ---
  return(data.frame(target_x = pred_x, target_y = pred_y))
}
