#' @title Train a PAI Model
#' @description Trains a supervised learning model using a flexible plug-in
#'   architecture, allowing for easy extension with both built-in and custom
#'   algorithms.
#' @details This function is the central training utility for the package. It
#' uses a `method` argument that can be either a character string to call a
#' built-in model (`"helmert"` `"lm"`,  `"gam_biv"`, `"rf"`, `"tps"`,), or a
#' list to define a completely custom model on the fly.
#'
#' \strong{Using Custom Models}: To provide a custom model, the `method`
#' argument should be a list with four required elements: `label` (a string),
#' `modelType` ("univariate" or "bivariate"), `fit` (a function), and `predict`
#' (a function). This allows advanced users to integrate virtually any
#' regression algorithm into the `mapAI` workflow.
#' @param gcp_data An `gcp` object of homologous points.
#' @param method A character string specifying a built-in algorithm, OR a list
#'   defining a custom model.
#' @param seed An integer for reproducibility.
#' @param ... Additional arguments passed to the model's `fit` function.
#' @return A trained model object of class `pai_model`.
#' @export
#' @examples
#' # Example using built-in models
#'
#' demo_data <- create_demo_data()
#'
#' # fit a linear model (two independent univariate models)
#' lm_model <- train_pai_model(gcp_data = demo_data$gcp, method = "lm")
#' print(lm_model)
#'
#' # fit a bivariate GAM model
#' gam_model <- train_pai_model(gcp_data = demo_data$gcp, method = "gam_biv")
#' print(gam_model)
train_pai_model <- function(gcp_data, method, seed = 123, ...) {

  set.seed(seed)

  # Input validation
  if (missing(gcp_data) || missing(method)) {
    stop("Both 'gcp_data' and 'method' arguments are required.", call. = FALSE)
  }
  # the get_model_info function handles validation of the method argument
  model_info <- get_model_info(method)

  if (!inherits(gcp_data, "gcp")) {
    stop("gcp_data must be an object of class 'gcp'.", call. = FALSE)
  }

  # Check for required packages
  if (!is.null(model_info$library)) {
    for (lib in model_info$library) {
      if (!requireNamespace(lib, quietly = TRUE)) {
        stop(paste("Package '", lib, "' is required for this model."),
             call. = FALSE)
      }
    }
  }

  message(paste("Training", model_info$label, "model..."))

  source_coords <- gcp_data[c("source_x", "source_y")]

  # Check if model is univariate or bivariate and fit accordingly
  if (model_info$modelType == "univariate") {
    model_dx <- model_info$fit(x = source_coords,
                               y = gcp_data$dx,
                               ...)
    model_dy <- model_info$fit(x = source_coords,
                               y = gcp_data$dy,
                               ...)
    model_fit <- list(model_dx = model_dx, model_dy = model_dy)
  } else if (model_info$modelType == "bivariate") {
    model_fit <- model_info$fit(gcp_data,
                                ...)
  }

  # Prepare output
  output <- list("model" = model_fit,
                 "method" = method,
                 "model_info" = model_info)

  class(output) <- c("pai_model", "list")
  return(output)
}


#' @title Predict Method for pai_model Objects
#' @description Predicts spatial displacement vectors (dx, dy) and the target
#' coordinates (target_x, target_y) from a trained  `pai_model` object.
#'
#' @param object A trained model object of class `pai_model`.
#' @param newdata A data frame with `source_x` and `source_y` columns.
#' @param ... Additional arguments passed to the model's `predict` function.
#'
#' @return A `gcp` object the predicted displacement columns: `dx` and `dy` and
#'  the predicted target coordinates `target_x` and `target_y`
#'
#' @importFrom stats predict
#' @export
#' @export predict.pai_model
#' @examples
#' # example code
#' demo_data <- create_demo_data()
#'
#' # split to train and test set
#' set.seed(1)
#' indx <- sample(1:nrow(demo_data$gcp), 100)
#' train_set <- demo_data$gcp[indx, ]
#' test_set <- demo_data$gcp[indx, ]
#'
#' # fit a linear model
#' lm_model <- train_pai_model(train_set, method = "lm")
#'
#' pred_gcp <- predict(lm_model, test_set)
#'
predict.pai_model <- function(object, newdata, ...) {

  # validate newdata
  new_data_validation(newdata)

  # get model details
  model_info <- object$model_info
  model_fit <- object$model

  # check model type to compute dx and dy
  if (model_info$modelType == "univariate") {

    dx <- model_info$predict(model_fit$model_dx, newdata = newdata, ...)
    dy <- model_info$predict(model_fit$model_dy, newdata = newdata, ...)
    df <- data.frame(dx = dx, dy = dy)

  } else if (model_info$modelType == "bivariate") {

    predictions <- model_info$predict(model_fit, newdata = newdata, ...)
    if (is.null(dim(predictions)) || dim(predictions)[2] != 2) {
      stop("The predict function for a bivariate model must return a 2-column object.", call. = FALSE)
    }

    df <- (data.frame(dx = predictions[, 1], dy = predictions[, 2]))
  }

  # build output
  df$source_x <- newdata$source_x
  df$source_y <- newdata$source_y
  df$target_x <-  df$dx + newdata$source_x
  df$target_y <-  df$dy + newdata$source_y

  # set the class to gcp
  class(df) <- c("gcp", "data.frame")

  return(df)

}


#' Print Method for pai_model Objects
#'
#' @param x An object of class `pai_model`.
#' @param ... Additional arguments (not used).
#' @details The print method provides a concise summary of the trained model,
#' @export
#' @export print.pai_model
print.pai_model <- function(x, ...) {
  cat("PAI Model -", x$model_info$label, "\n")
  if (x$model_info$modelType == "univariate") {
    cat("Model Type: Univariate\n")
    cat("Components:\n")
    # Using cat() for more controlled output of model summaries
    cat("Model for dx:\n")
    print(summary(x$model$model_dx)) # Example: print a summary instead of the whole object
    cat("\nModel for dy:\n")
    print(summary(x$model$model_dy))
  } else if (x$model_info$modelType == "bivariate") {
    cat("Model Type: Bivariate\n")
    print(summary(x$model))
  }
  invisible(x)
}

#' Plot Method for pai_model Objects
#'
#' @param x An object of class `pai_model`.
#' @param ... Additional arguments (not used).
#' @details The plot method visualizes the model components based on whether the
#'  model is univariate or bivariate.
#' @export
#' @export plot.pai_model
plot.pai_model <- function(x, ...) {
  cat("PAI Model -", x$model_info$label, "\n")
  if (x$model_info$modelType == "univariate") {
    cat("Model Type: Univariate\n")
    cat("Components:\n")
    plot(x$model$model_dx, main = "Model for dx")
    plot(x$model$model_dy, main = "Model for dy")
  } else if (x$model_info$modelType == "bivariate") {
    cat("Model Type: Bivariate\n")
    plot(x$model, main = "Bivariate Model")
  }
  invisible(x)
}


#' Residuals Method for pai_model Objects
#' @param x An object of class `pai_model`.
#' @details The residuals method computes the residuals of the model and returns
#'  a list with them and two plots for dx and dy.
# @export
# @export residuals.pai_model
residuals.pai_model <- function(x) {

  #TODO after predict method is implemented

}

