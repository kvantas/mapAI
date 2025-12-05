#' @title Perform a Differential Distortion Analysis
#' @description Computes a comprehensive set of detailed distortion metrics for
#'   a PAI model at specified locations, based on Tissot's indicatrix theory.
#' @details This function is the core analytical engine of the package. It
#'   implements a differential analysis by calculating the first partial
#'   derivatives of the spatial transformation learned by a `pai_model` using
#'   numerical differentiation. From these derivatives, it calculates key
#'   distortion metrics that describe how shape, area, and angles are warped.
#'
#' **Interpreting Results by Model Type:**
#'   The nature of the output is highly dependent on the model used.
#' \itemize{
#'   \item \strong{`gam` & `tps`}: Produce a smooth, differentiable surface,
#'     leading to spatially variable and meaningful distortion metrics.
#'   \item \strong{`helmert` & `lm`}: Represent global transformations,
#'    resulting in distortion metrics that are constant for every point.
#'   \item \strong{`rf`}: Creates a step-like surface where local derivatives
#'     may be zero, potentially showing no local distortion.
#' }
#'
#' @param pai_model A model object of class `pai_model`.
#' @param newdata A data frame with `source_x` and `source_y` columns.
#' If `NULL` (default),
#'   the GCPs used to train the model will be used.
#' @param reference_scale A single numeric value to normalize area scale.
#'
#' @return An `distortion` object (a data frame) with the original points and
#'  new columns for all calculated distortion metrics (e.g., `a`, `b`,
#'  `log2_area_scale`, `max_shear`, `max_angular_distortion`, `airy_kavrayskiy`,
#'  `theta_a`).
#'
#' @export
#' @examples
#'   # Create data and train a model
#'   demo_data <- create_demo_data()
#'   pai_model <- train_pai_model(demo_data$gcp, method = "tps")
#'
#'   # Analyze distortion on the training points
#'   distortion_results <- analyze_distortion(pai_model)
#'   print(distortion_results)
#'   summary(distortion_results)
#'
#'   # plot a selected metric
#'   plot(distortion_results, metric = "area_scale", diverging = TRUE)
#'
#' # Plot Tissot's indicatrices using automatic scale factor
#'  indicatrices(distortion_results)
analyze_distortion <- function(pai_model,
                               newdata = NULL,
                               reference_scale = 1) {

  # --- Input validation ---
  an_dist_validation(pai_model, reference_scale)

  if (!is.null(newdata)) {
    new_data_validation(newdata)
  } else {
    newdata <- pai_model$gcp
  }

  message(paste("Calculating distortion metrics for",
                pai_model$model_info$label, "model..."))

  # ---  Numerical Derivatives Calculation ---

  # Determine a small step size h
  coord_range <- max(c(diff(range(newdata$source_x)),
                       diff(range(newdata$source_y))),
                     na.rm = TRUE)
  h <- coord_range * 1e-6

  # --- Step 1: Compute partial derivatives with respect to x ---

  # Create minimal data frames for prediction
  coords_x_plus_h <- data.frame(source_x = newdata$source_x + h,
                                source_y = newdata$source_y)
  coords_x_minus_h <- data.frame(source_x = newdata$source_x - h,
                                 source_y = newdata$source_y)

  # Predict transformed coordinates (this is the corrected logic)
  T_x_plus <- predict(pai_model, newdata = coords_x_plus_h)
  T_x_minus <- predict(pai_model, newdata = coords_x_minus_h)

  dfx_dx <- (T_x_plus$target_x - T_x_minus$target_x) / (2 * h)
  dfy_dx <- (T_x_plus$target_y - T_x_minus$target_y) / (2 * h)

  # Explicitly remove large intermediate objects to free memory
  rm(coords_x_plus_h, coords_x_minus_h, T_x_plus, T_x_minus)

  # --- Step 2: Compute partial derivatives with respect to y ---
  coords_y_plus_h <- data.frame(source_x = newdata$source_x,
                                source_y = newdata$source_y + h)
  coords_y_minus_h <- data.frame(source_x = newdata$source_x,
                                 source_y = newdata$source_y - h)

  # Predict transformed coordinates
  T_y_plus <- predict(pai_model, newdata = coords_y_plus_h)
  T_y_minus <- predict(pai_model, newdata = coords_y_minus_h)

  dfx_dy <- (T_y_plus$target_x - T_y_minus$target_x) / (2 * h)
  dfy_dy <- (T_y_plus$target_y - T_y_minus$target_y) / (2 * h)

  # remove large intermediate objects to free memory
  rm(coords_y_plus_h, coords_y_minus_h, T_y_plus, T_y_minus)

  # --- Finalizing metrics from derivatives (vectorized and efficient) ---
  E <- dfx_dx^2 + dfy_dx^2
  G <- dfx_dy^2 + dfy_dy^2
  F_metric <- dfx_dx * dfx_dy + dfy_dx * dfy_dy

  sqrt_term <- sqrt(pmax(0, (E - G)^2 + 4 * F_metric^2))

  a <- sqrt(0.5 * (E + G + sqrt_term))
  b <- sqrt(0.5 * (E + G - sqrt_term))

  area_scale <- a * b

  # Add results to a new data frame to avoid modifying the input object directly
  results <- newdata
  results$a <- a
  results$b <- b
  results$area_scale <- area_scale
  results$log2_area_scale <- log2(area_scale / (reference_scale^2))
  results$max_shear <- asin((a - b) / (a + b)) * 180 / pi
  results$max_angular_distortion <- 2 * asin((a - b) / (a + b))
  results$airy_kavrayskiy <- 0.5 * (log(a)^2 + log(b)^2)

  theta_xp <- atan2(dfy_dx, dfx_dx)
  alpha_p <- atan2(2 * F_metric, E - G) / 2
  results$theta_a <- (theta_xp - alpha_p) * 180 / pi

  class(results) <- c("distortion", "data.frame")

  message("Distortion analysis complete.")
  return(results)
}


#' @title Print Method for distortion Objects
#' @description Custom print method for `distortion` objects to provide a
#' concise summary of the distortion analysis results.
#' @param x An object of class `distortion`.
#' @param ... Additional arguments (not used).
#' @export
#' @examples
#' # See ?analyze_distortion for a runnable example that creates a
#' # distortion object.
print.distortion <- function(x, ...) {
  cat("Distortion Analysis Results\n")
  cat("---------------------------\n")
  cat("Number of Points Analyzed:", nrow(x), "\n")
  cat("Metrics Included:\n")
  cat(" - a: Major axis length of Tissot's indicatrix\n")
  cat(" - b: Minor axis length of Tissot's indicatrix\n")
  cat(" - area_scale: Area distortion factor\n")
  cat(" - log2_area_scale: Log2 area distortion relative to reference scale\n")
  cat(" - max_shear: Maximum shear distortion (degrees)\n")
  cat(" - max_angular_distortion: Maximum angular distortion (radians)\n")
  cat(" - airy_kavrayskiy: Airy-Kavrayskiy distortion measure\n")
  cat(" - theta_a: Orientation of maximum distortion (degrees)\n")

  n <- nrow(x)
  if (n > 10) {
    cat("Displaying first 10 points:\n")
    x <- x[1:10, ]
  }
  print.data.frame(x)
  invisible(x)
}


#' @title Summary Method for distortion Objects
#' @description Provides a statistical summary of the distortion metrics
#' contained in a `distortion` object.
#' @param object An object of class `distortion`.
#' @param ... Additional arguments (not used).
#' @return A data frame summarizing key statistics for each distortion metric.
#' @importFrom stats sd median
#' @export
#' @examples
#' # See ?analyze_distortion for a runnable example.
summary.distortion <- function(object, ...) {
  if (!inherits(object, "distortion")) {
    stop("`object` must be of class 'distortion'.", call. = FALSE)
  }

  metrics <- c("a", "b", "area_scale", "log2_area_scale",
               "max_shear", "max_angular_distortion", "airy_kavrayskiy",
               "theta_a")

  summary_list <- lapply(metrics, function(metric) {
    if (metric %in% names(object)) {
      data <- object[[metric]]
      c(
        Mean = mean(data, na.rm = TRUE),
        Median = median(data, na.rm = TRUE),
        SD = sd(data, na.rm = TRUE),
        Min = min(data, na.rm = TRUE),
        Max = max(data, na.rm = TRUE)
      )
    } else {
      NULL
    }
  })

  summary_df <- do.call(rbind, summary_list)
  rownames(summary_df) <- metrics
  return(as.data.frame(summary_df))
}

#' @title Visualize a Distortion Metric as a Continuous Surface
#' @description Creates a smooth, interpolated surface visualization of a
#'   distortion metric from the output of `analyze_distortion()`.
#'
#' @details This function visualizes the distortion field as it exists on the
#'   **target map's coordinate space**. It uses linear interpolation via the
#'   `interp` package to create a continuous raster surface, even from
#'    scattered, irregular output points (like the original GCPs). This provides
#'    a true surface plot in all cases.
#'
#' @param x A `distortion` object returned by `analyze_distortion()`.
#' @param metric A character string specifying the metric to plot.
#' @param palette A viridis color palette name (e.g., "viridis", "magma").
#' @param diverging If `TRUE`, uses a red-white-blue diverging color scale.
#' @param value_range A numeric vector of length 2 specifying color scale
#'  limits.
#' @param add_points If `TRUE`, the original analysis points are overlaid.
#' @param n_grid The resolution of the interpolation grid (e.g., 200x200).
#' @param ... Additional arguments (not used).

#' @return A `ggplot` object.
#'
#' @import ggplot2
#' @importFrom rlang .data sym
#' @importFrom interp interp
#' @importFrom viridis scale_fill_viridis
#' @export
#' @examples
#' # See ?analyze_distortion for a runnable example.
plot.distortion <- function(x,
                            metric = "area_scale",
                            palette = "viridis",
                            diverging = FALSE,
                            value_range = NULL,
                            add_points = TRUE,
                            n_grid = 200,
                            ...) {

  # --- 1. Input Validation ---
  plot_input_validation(x, metric, palette, diverging, value_range, add_points)


  # --- 2. Interpolate Data for a Smooth Surface ---
  message("Interpolating data to create a smooth surface...")
  interp_result <- tryCatch({
    interp::interp(
      x = x$target_x,
      y = x$target_y,
      z = x[[metric]],
      nx = n_grid,
      ny = n_grid,
      duplicate = "strip"
    )
  }, error = function(e) {
    stop(
      "Interpolation failed. Ensure you have at least 5 non-collinear points.",
      call. = FALSE)
  })

  # Convert interpolation result to a plotable data frame
  interp_df <- data.frame(
    target_x = rep(interp_result$x, times = length(interp_result$y)),
    target_y = rep(interp_result$y, each = length(interp_result$x)),
    metric_val = as.vector(interp_result$z)
  )
  # Remove NA values which can occur at the convex hull of the points
  interp_df <- stats::na.omit(interp_df)

  # --- 3. Create the Base Plot ---
  p <- ggplot2::ggplot(interp_df, aes(x = .data$target_x, y = .data$target_y)) +
    geom_raster(aes(fill = .data$metric_val)) +
    coord_equal(expand = FALSE) +
    labs(
      title = paste("Distortion Analysis:", metric),
      subtitle = "Interpolated surface from analysis points",
      x = "Target X Coordinate",
      y = "Target Y Coordinate",
      fill = metric # Legend title
    ) +
    theme_minimal()

  # --- 4. Refactored and Corrected Color Scale Application ---
  scale_layer <- NULL
  if (diverging) {
    midpoint <- if (metric == "area_scale") 1 else 0
    scale_layer <- scale_fill_gradient2(
      low = "#3B4CC0", mid = "#F1F1F1", high = "#B40426",
      midpoint = midpoint,
      limits = value_range,
      na.value = "transparent"
    )
  } else {
    scale_layer <- scale_fill_viridis_c(
      option = palette,
      limits = value_range,
      na.value = "transparent"
    )
  }
  p <- p + scale_layer

  # --- 5. Optionally Add Original Points ---
  if (add_points) {
    p <- p +
      geom_point(
        data = x,
        aes(x = .data$target_x, y = .data$target_y),
        shape = 3, color = "black", size = 1.5, alpha = 0.7,
        inherit.aes = FALSE # Important!
      )
  }

  return(p)
}




#' @title Plot Tissot's Indicatrices of Distortion
#' @description Visualizes distortion by drawing Tissot's indicatrices
#'   (ellipses) at their original source locations.
#' @details This function creates a powerful visual representation of
#' distortion. It draws an ellipse at each analyzed point, centered on its
#' **target coordinate**. The size, shape, and orientation of the ellipse
#' graphically represent the distortion at that location.
#'
#' @param object A `distortion` object from `analyze_distortion()`.
#' @param scale_factor A numeric value to control the visual size of the
#'   plotted ellipses. If `NULL` (the default), a reasonable scale factor is
#'   automatically calculated based on the spatial extent of the data.
#' @param fill_color A character string specifying the fill color of the
#'  ellipses.
#' @param border_color A character string specifying the border color.
#' @param alpha A numeric value (0-1) for the transparency of the ellipses.
#'
#' @return A `ggplot` object.
#'
#' @import ggplot2
#' @importFrom ggforce geom_ellipse
#' @export
#' @examples
#' # See ?analyze_distortion for a complete, runnable example.
indicatrices <- function(object,
                         scale_factor = NULL,
                         fill_color = "lightblue",
                         border_color = "black",
                         alpha = 0.7) {

  # --- 1. Input Validation ---
  # Note: validation for scale_factor now allows NULL
  indicatrices_validation(object, scale_factor, fill_color, border_color)

  # --- 2. Automatic Scale Factor Calculation ---
  if (is.null(scale_factor)) {
    # Calculate the maximum spatial extent (width or height) of the points
    x_range <- diff(range(object$target_x, na.rm = TRUE))
    y_range <- diff(range(object$target_y, na.rm = TRUE))
    max_extent <- max(x_range, y_range)

    # Calculate a scale factor that makes the average ellipse ~1/40th of the
    # extent
    avg_axis <- mean(object$a, na.rm = TRUE)
    if (avg_axis > 0 && max_extent > 0) {
      scale_factor <- (max_extent / 40) / avg_axis
      message(paste("`scale_factor` is NULL. Automatically chosen value:",
                    round(scale_factor, 2)))
    } else {
      warning(
        "Could not automatically determine scale_factor. Defaulting to 1.",
        call. = FALSE)
      scale_factor <- 1 # Fallback for edge cases
    }
  }

  # --- 3. Create the Plot using ggforce ---
  p <- ggplot(
    data = object,
    aes(
      x0 = .data$target_x,
      y0 = .data$target_y,
      a = .data$a * scale_factor,
      b = .data$b * scale_factor,
      angle = .data$theta_a * pi / 180
    )
  ) +
    ggforce::geom_ellipse(
      fill = fill_color,
      color = border_color,
      alpha = alpha
    ) +
    coord_equal(expand = TRUE) +
    labs(
      title = "Tissot's Indicatrices of Distortion",
      subtitle = "Ellipses are centered on their source coordinates",
      x = "Target X Coordinate",
      y = "Target Y Coordinate"
    ) +
    theme_minimal()

  return(p)
}
