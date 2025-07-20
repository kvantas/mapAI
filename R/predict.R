#' @title Predict Method for pai_model Objects
#' @description Predicts spatial corrections (dx, dy) from a trained `pai_model`
#'   object. This is an S3 method for the generic `predict()` function.
#'
#' @details This function provides the core prediction logic for all models
#'   created by `train_pai_model()`. As an S3 method, it should not be called
#'   directly (e.g., `predict.pai_model(...)`), but rather through the generic
#'   `predict()` function (e.g., `predict(my_model, ...)`).
#'
#'   Key features of this method include:
#' \itemize{
#'   \item \strong{Automatic Model Handling:} It transparently handles the
#'    different output structures of `helmert`,`tps`, `gam`, `lm`, `rf`, `svmRadial` and `svmLinear`
#'    models, always returning a consistent `data.frame`.
#'   \item \strong{Robust NA Handling:} It correctly handles `NA` values in the
#'     `newdata` predictors. Rows with `NA` inputs will produce `NA` outputs,
#'     ensuring the output has the same number of rows as the input and
#'     preventing errors from underlying prediction functions.
#' }
#'
#' @param object A trained model object of class `pai_model` returned by
#'    `train_pai_model()`.
#' @param newdata A `data.frame` with `source_x` and `source_y` columns for
#'    which to generate predictions.
#' @param ... Additional arguments passed on to the underlying predict methods
#'   (e.g., `predict.lm`, `predict.gam`, `predict.svm`).
#'
#' @return A `data.frame` with predicted `dx` and `dy` columns, having the same
#'   number of rows as `newdata`.
#'
#' @keywords internal
#' @export
#' @examples
#' \dontrun{
#' # This example shows how the generic `predict()` function can be used
#' # on any model trained by `train_pai_model()`.
#'
#' # --- 1. Load Data and Train Models ---
#' data(gcps) # Load the package's built-in homologous points
#'
#' # Train two different types of models
#' pai_model_gam <- train_pai_model(gcps, pai_method = "gam")
#' pai_model_rf <- train_pai_model(gcps, pai_method = "rf")
#'
#' # --- 2. Create New Data for Prediction ---
#' # We'll create a small data frame of new points.
#' # Note the third row contains an NA to demonstrate robust NA handling.
#' new_points_to_predict <- data.frame(
#'   source_x = c(241643.0, 241650.0, NA),
#'   source_y = c(4477383, 4477370, 4477390)
#' )
#'
#' # --- 3. Use the Generic `predict()` Function ---
#' # The same `predict()` call works for both model objects.
#'
#' # Predict using the GAM model
#' predictions_from_gam <- predict(pai_model_gam, newdata = new_points_to_predict)
#'
#' # Predict using the Random Forest model
#' predictions_from_rf <- predict(pai_model_rf, newdata = new_points_to_predict)
#'
#' # --- 4. Inspect the Results ---
#' print("Predictions from GAM model:")
#' print(predictions_from_gam)
#' #>           dx        dy
#' #> 1  0.5898319 -0.163833
#' #> 2  0.5908929 -0.161099
#' #> 3         NA        NA
#'
#' print("Predictions from Random Forest model:")
#' print(predictions_from_rf)
#' }
predict.pai_model <- function(object, newdata, ...) {

  # --- 1. Input Validation ---
  if (missing(newdata) || is.null(newdata)) {
    stop("The 'newdata' argument is required.",
         call. = FALSE)
  }
  if (!all(c("source_x", "source_y") %in% names(newdata))) {
    stop("'newdata' must contain 'source_x' and 'source_y' columns.",
         call. = FALSE)
  }

  # --- 2. Handle NA values ---
  # Identify rows with NAs in the predictors. This is the only robust way
  # to handle libraries (like ranger) that have internal NA imputation.
  predictor_cols <- c("source_x", "source_y")
  complete_rows_idx <- stats::complete.cases(newdata[, predictor_cols])

  # Initialize full-length output vectors with NAs
  pred_dx <- rep(NA_real_, nrow(newdata))
  pred_dy <- rep(NA_real_, nrow(newdata))

  # Only predict if there are some complete rows to work with
  if (any(complete_rows_idx)) {
    clean_data <- newdata[complete_rows_idx, , drop = FALSE]

    # --- 3. Call the appropriate predict method on CLEAN data ---
    if (object$method == "helmert") {
      # Extract coefficients and centroids
      coefs <- object$model$coefficients
      cents <- object$model$centroids
      a <- coefs["a"]
      b <- coefs["b"]

      # Calculate centered coordinates for the new data
      u_i <- newdata$source_x - cents["u_mean"]
      v_i <- newdata$source_y - cents["v_mean"]

      # Predict the TARGET coordinates using the standard transformation formula
      pred_target_x <- (a * u_i - b * v_i) + cents["x_mean"]
      pred_target_y <- (b * u_i + a * v_i) + cents["y_mean"]

      # compute.the correction vectors (dx, dy)
      pred_dx[complete_rows_idx]  <- pred_target_x - newdata$source_x
      pred_dy[complete_rows_idx]  <- pred_target_y - newdata$source_y

    } else if (object$method == "gam") {
      preds_clean <- stats::predict(object$model, newdata = clean_data, ...)
      pred_dx[complete_rows_idx] <- preds_clean[, 1]
      pred_dy[complete_rows_idx] <- preds_clean[, 2]

    } else if (object$method %in% c("lm", "svmRadial", "svmLinear")) {
      pred_dx[complete_rows_idx] <- stats::predict(object$model$model_dx, newdata = clean_data, ...)
      pred_dy[complete_rows_idx] <- stats::predict(object$model$model_dy, newdata = clean_data, ...)

    } else if (object$method == "rf") {
      pred_dx_obj <- stats::predict(object$model$model_dx, data = clean_data, ...)
      pred_dy_obj <- stats::predict(object$model$model_dy, data = clean_data, ...)

      pred_dx[complete_rows_idx] <- pred_dx_obj$predictions
      pred_dy[complete_rows_idx] <- pred_dy_obj$predictions
    } else if (object$method == "tps") {
      clean_coords <- as.matrix(clean_data[, c("source_x", "source_y")])

      pred_dx[complete_rows_idx] <- predict(object$model$model_dx, x = clean_coords, ...)
      pred_dy[complete_rows_idx] <- predict(object$model$model_dy, x = clean_coords, ...)

    }
  }

  # --- 4. Return the reconstructed data frame ---
  return(data.frame(dx = pred_dx, dy = pred_dy, row.names = row.names(newdata)))
}
