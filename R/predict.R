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
  if (object$method == "gam") {
    preds <- stats::predict(object$model, newdata = newdata)
    return(data.frame(dx = preds[, 1], dy = preds[, 2]))
  } else {
    pred_dx_obj <- stats::predict(object$model$model_dx, data = newdata)
    pred_dy_obj <- stats::predict(object$model$model_dy, data = newdata)

    pred_dx <- if (inherits(pred_dx_obj, "ranger.prediction")) pred_dx_obj$predictions else pred_dx_obj
    pred_dy <- if (inherits(pred_dy_obj, "ranger.prediction")) pred_dy_obj$predictions else pred_dy_obj

    return(data.frame(dx = pred_dx, dy = pred_dy))
  }
}
