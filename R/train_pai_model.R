#' @title Train a PAI Model
#' @description Trains a supervised learning model using a flexible plug-in
#'   architecture, allowing for easy extension with both built-in and custom
#'   algorithms.
#' @details This function is the central training utility for the package. It
#' uses a `method` argument that can be either a character string to call a
#' built-in model (`"helmert"`, `"lm"`,  `"gam_biv"`, `"rf"`, `"tps"`), or a
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
#' # fit a bivariate GAM model
#' gam_model <- train_pai_model(gcp_data = demo_data$gcp, method = "gam_biv")
#'
#' # plot the residuals of the GAM model
#' residuals(gam_model)
#'
#' # plot the learned correction surfaces for dx and dy for the model
#' surface(gam_model)
#'
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
    # --- Robust fitting for model_dx ---
    model_dx <- tryCatch({
      model_info$fit(x = source_coords, y = gcp_data$dx, ...)
    }, error = function(e) {
      # This function executes if an error occurs in the 'try' block
      stop(sprintf(
        "Failed to train the model for the dx component.\n  Underlying error: %s",
        e$message
      ), call. = FALSE)
    })

    # --- Robust fitting for model_dy ---
    model_dy <- tryCatch({
      model_info$fit(x = source_coords, y = gcp_data$dy, ...)
    }, error = function(e) {
      stop(sprintf(
        "Failed to train the model for the dy component.\n  Underlying error: %s",
        e$message
      ), call. = FALSE)
    })

    model_fit <- list(model_dx = model_dx, model_dy = model_dy)

  } else if (model_info$modelType == "bivariate") {
    # --- Robust fitting for bivariate model ---
    model_fit <- tryCatch({
      model_info$fit(gcp_data, ...)
    }, error = function(e) {
      stop(sprintf(
        "Failed to train the bivariate model '%s'.\n  Underlying error: %s",
        model_info$label, e$message
      ), call. = FALSE)
    })
  }


  # Prepare output
  output <- list("model" = model_fit,
                 "method" = method,
                 "model_info" = model_info,
                 "gcp" = gcp_data)

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
#' # predict using the test_set
#' pred_gcp <- predict(lm_model, test_set)
#' pred_gcp
predict.pai_model <- function(object, newdata, ...) {

  # validate newdata
  new_data_validation(newdata)

  # get model details
  model_info <- object$model_info
  model_fit <- object$model

  # use only source coordinates for prediction
  source_coords <- newdata[c("source_x", "source_y")]


  # check model type to compute dx and dy
  if (model_info$modelType == "univariate") {
    # --- Robust prediction for dx ---
    dx <- tryCatch({
      model_info$predict(model_fit$model_dx, newdata = source_coords, ...)
    }, error = function(e) {
      stop(sprintf(
        "Prediction for the dx component failed.\n  Underlying error: %s",
        e$message
      ), call. = FALSE)
    })



    # --- Robust prediction for dy ---
    dy <- tryCatch({
      model_info$predict(model_fit$model_dy, newdata = source_coords, ...)
    }, error = function(e) {
      stop(sprintf(
        "Prediction for the dy component failed.\n  Underlying error: %s",
        e$message
      ), call. = FALSE)
    })

    df <- data.frame(dx = dx, dy = dy)

  } else if (model_info$modelType == "bivariate") {
    # --- Robust prediction for bivariate model ---
    predictions <- tryCatch({
      model_info$predict(model_fit, newdata = source_coords, ...)
    }, error = function(e) {
      stop(sprintf(
        "Prediction for the bivariate model '%s' failed.\n  Underlying error: %s",
        model_info$label, e$message
      ), call. = FALSE)
    })

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


#' @title Print Method for pai_model Objects
#'
#' @param x An object of class `pai_model`.
#' @param ... Additional arguments (not used).
#' @details The print method provides a concise summary of the trained model,
#' @export
#' @examples
#' # See ?train_pai_model for a complete, runnable example.
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

#' @title Plot Method for pai_model Objects
#'
#' @param x An object of class `pai_model`.
#' @param ... Additional arguments (not used).
#' @details The plot method visualizes the model components based on whether the
#'  model is univariate or bivariate.
#' @export
#' @examples
#' # See ?train_pai_model for a complete, runnable example.
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


#' @title Residuals Method for pai_model Objects
#' @description Visualizes the residual errors of a trained `pai_model` as arrows
#'  pointing from predicted to actual target locations.
#'
#' @param object An object of class `pai_model`.
#' @param title A character string for the plot's main title.
#' @param subtitle A character string for the plot's subtitle.
#' @param arrow_color A character string specifying the color of the residual
#'    arrows.
#' @param point_color A character string specifying the color of the points
#'    marking the predicted locations.
#' @param exaggeration_factor A numeric value to scale the length of the
#'   residual vectors. A value of 2, for instance, will double their
#'   plotted length, making subtle residuals more visible. Defaults to 1
#'   (no exaggeration).
#' @param ... Additional arguments (not used).
#' @return A plot with the residual as arrows using a `ggplot` object, which
#' can be further customized.
#' @import ggplot2
#' @importFrom rlang .data
#' @importFrom grid arrow unit
#' @export
#' @examples
#' # See ?train_pai_model for a complete, runnable example.
residuals.pai_model <- function(
    object,
    title = "Model Residual Error Vectors",
    subtitle = "Arrows point from predicted to true target locations",
    arrow_color = "darkblue",
    point_color = "blue",
    exaggeration_factor = 1,
    ...) {

  # Predict the displacements
  pred <- predict(object, object$gcp)

  # add actual target coordinates from pai_model
  pred$actual_target_x <- object$gcp$target_x
  pred$actual_target_y <- object$gcp$target_y

  # create the plots of residuals
  plt <- ggplot2::ggplot(pred) +
    # Draw arrows from the predicted location to the true location
    ggplot2::geom_segment(
      ggplot2::aes(
        x = .data$target_x,
        y = .data$target_y,
        xend = .data$target_x + exaggeration_factor * (.data$actual_target_x - .data$target_x),
        yend = .data$target_y + exaggeration_factor * (.data$actual_target_y - .data$target_y)
      ),
      arrow = grid::arrow(length = grid::unit(0.1, "cm")),
      color = arrow_color, # Use the parameter
      alpha = 0.7
    ) +
    # Add a point at the start of each arrow (the predicted location)
    ggplot2::geom_point(
      ggplot2::aes(x = .data$target_x, y = .data$target_y),
      color = point_color, # Use the parameter
      size = 0.5,
      shape = 1
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Target X Coordinate",
      y = "Target Y Coordinate"
    ) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal()

  return(plt)

}

#' @title Plot Surface Method for the package's Objects
#'
#' @description This is a generic function that generates a surface
#'  from the package's objects
#'
#' @param object An object from the `mapAI` package.
#' @param n_grid The resolution of the interpolation grid used to create the
#'  smooth surface. Higher values create a more detailed plot but take longer to
#'   compute.
#' @param ... Additional arguments.
#' @return A surface representation (e.g. a list with two ggplots).
#' @import ggplot2
#' @importFrom viridis scale_fill_viridis
#' @importFrom rlang .data
#' @export
#' @examples
#' # See ?train_pai_model for a complete, runnable example.

surface <- function(object,
                    n_grid,
                    ...) {
  UseMethod("surface")
}

#' @title Plot the Learned Correction Surface
#'
#' @description Visualizes the spatial correction field (dx, dy) learned by a
#'   PAI model.
#'
#' @details This function serves as a key diagnostic tool for understanding the
#' behavior of a trained `pai_model`. It creates two raster plots: one for the
#' `dx` (East-West) corrections and one for the `dy` (North-South) corrections.
#'
#' The color intensity on the plots reveals the magnitude of the correction at
#' any given location. Contour lines show the gradient of the change, and black
#' crosses mark the location of the original Ground Control Points (GCPs),
#' showing where the model had direct information to learn from.
#'
#' By examining these surfaces, users can:
#' \itemize{
#'   \item Understand the spatial nature of the distortion their model has
#'   learned.
#'   \item Identify areas of high vs. low correction.
#'   \item Spot potential issues like extreme corrections or unusual artifacts,
#'     especially at the edges of the data where the model is extrapolating.
#' }
#'
#' @param object A trained `pai_model` object returned by `train_pai_model()`.
#' @param n_grid The resolution of the interpolation grid used to create the
#'  smooth surface. Higher values create a more detailed plot but take longer to
#'   compute. Defaults to 100.
#' @param plot_gcp A logical value indicating whether to plot the GCP
#' locations on the correction surfaces. Defaults to `TRUE`.
#' @param dx_range A numeric vector of length 2 specifying the limits for the
#'  `dx` color scale (e.g., `c(-10, 10)`). Defaults to `NULL`, which uses the
#'  data's range.
#' @param dy_range A numeric vector of length 2 specifying the limits for the
#'  `dy` color scale. Defaults to `NULL`.
#' @param ... Additional arguments (not used).
#'
#' @return A list containing two `ggplot` objects: `dx_plot` and `dy_plot`.
#' You can plot them individually.
#'
#' @import ggplot2
#' @importFrom viridis scale_fill_viridis
#' @importFrom rlang .data
#' @export
#' @examples
#'
#' # create demo data and fit a bivariate GAM model
#' demo_data <- create_demo_data()
#' gam_model <- train_pai_model(demo_data$gcp, method = "gam_biv")
#' surface_plots <- surface(gam_model, n_grid = 100)
#'
#' # Plot the dx surface
#' surface_plots$dx_plot
#'
#' # Plot the dy surface
#' surface_plots$dy_plot
#'
surface.pai_model <- function(object,
                              n_grid = 100,
                              plot_gcp = TRUE,
                              dx_range = NULL,
                              dy_range = NULL,
                              ...) {

  # TODO validate parameters

  x_range <- range(object$gcp$source_x)
  y_range <- range(object$gcp$source_y)

  grid_to_predict <- expand.grid(
    source_x = seq(x_range[1], x_range[2], length.out = n_grid),
    source_y = seq(y_range[1], y_range[2], length.out = n_grid)
  )

  # Use the new S3 predict method
  plot_data <- predict(object, newdata = grid_to_predict)

  # --- Create dx plot ---
  p_dx <- ggplot(plot_data,
                 aes(x = .data$source_x, y = .data$source_y)) +
    geom_raster(aes(fill = .data$dx)) +
    geom_contour(aes(z = .data$dx), color = "white", alpha = 0.4, bins = 12) +
    labs(title = "Correction Surface (dx)", x = "X", y = "Y") +
    coord_equal() + theme_minimal()

  # Apply custom dx range if provided
  if (!is.null(dx_range) && is.numeric(dx_range) && length(dx_range) == 2) {
    p_dx <- p_dx +
      scale_fill_viridis(
        option = "viridis",
        name = "dx",
        limits = dx_range,
        na.value = "transparent")
  } else {
    p_dx <- p_dx +
      scale_fill_viridis(
        option = "viridis",
        name = "dx")
  }

  # --- Create dy plot ---
  p_dy <- ggplot(plot_data,
                 aes(x = .data$source_x, y = .data$source_y)) +
    geom_raster(aes(fill = .data$dy)) +
    geom_contour(aes(z = .data$dy), color = "white", alpha = 0.4, bins = 12) +
    labs(title = "Correction Surface (dy)", x = "X", y = "Y") +
    coord_equal() + theme_minimal()

  # Apply custom dy range if provided
  if (!is.null(dy_range) && is.numeric(dy_range) && length(dy_range) == 2) {
    p_dy <- p_dy +
      scale_fill_viridis(
        option = "viridis",
        name = "dy",
        limits = dy_range,
        na.value = "transparent")
  } else {
    p_dy <- p_dy +
      scale_fill_viridis(
        option = "viridis",
        name = "dy",
        na.value = "transparent")
  }

  # Add GCPs if requested
  if (plot_gcp) {
    gcp_layer <- geom_point(data = object$gcp, inherit.aes = FALSE,
                            mapping = aes(x = .data$source_x,
                                          y = .data$source_y),
                            shape = 3, color = "black", size = 0.8,
                            alpha = 0.7)
    p_dx <- p_dx + gcp_layer
    p_dy <- p_dy + gcp_layer
  }

  # Return a named list of plots
  return(list(dx_plot = p_dx, dy_plot = p_dy))
}
