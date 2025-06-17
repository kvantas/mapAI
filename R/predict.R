#' @title Predict method for pai_model objects
#' @description Predicts spatial corrections (dx, dy) from a trained `pai_model`.
#' @param object A trained model object of class `pai_model`.
#' @param newdata A data frame with `source_x` and `source_y` columns for which
#'   to generate predictions.
#' @param ... Additional arguments (not used).
#' @return A data frame with predicted `dx` and `dy` columns.
#' @keywords internal
#' @export
predict.pai_model <- function(object, newdata, ...) {
  # --- 1. Input Validation ---
  if (missing(newdata) || is.null(newdata)) {
    stop("The 'newdata' argument is required.", call. = FALSE)
  }
  if (!all(c("source_x", "source_y") %in% names(newdata))) {
    stop("'newdata' must contain 'source_x' and 'source_y' columns.", call. = FALSE)
  }

  # --- 2. THE DEFINITIVE FIX: Pre-emptive NA Handling ---
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
    if (object$method == "gam") {
      preds_clean <- stats::predict(object$model, newdata = clean_data, ...)
      pred_dx[complete_rows_idx] <- preds_clean[, 1]
      pred_dy[complete_rows_idx] <- preds_clean[, 2]

    } else if (object$method == "lm") {
      pred_dx[complete_rows_idx] <- stats::predict(object$model$model_dx, newdata = clean_data, ...)
      pred_dy[complete_rows_idx] <- stats::predict(object$model$model_dy, newdata = clean_data, ...)

    } else if (object$method == "rf") {
      pred_dx_obj <- stats::predict(object$model$model_dx, data = clean_data, ...)
      pred_dy_obj <- stats::predict(object$model$model_dy, data = clean_data, ...)

      pred_dx[complete_rows_idx] <- pred_dx_obj$predictions
      pred_dy[complete_rows_idx] <- pred_dy_obj$predictions
    }
  }

  # --- 4. Return the reconstructed data frame ---
  return(data.frame(dx = pred_dx, dy = pred_dy, row.names = row.names(newdata)))
}
