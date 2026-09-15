#' @title Perform Differential Distortion Analysis on a Spatial Transformation
#' @description Evaluates the local differential geometry and distortion metrics
#'   of a spatial transformation learned by a `pai_model`, based on Tissot's
#'   indicatrix theory, Cauchy-Green deformation analysis, and signed Jacobian
#'   determinants.
#'
#' @details
#' This function forms the analytical diagnostic engine of the `mapAI` package.
#' By numerically evaluating first-order partial derivatives of the coordinate
#' transformation \eqn{\mathbf{f}(x, y) = (f_x(x, y), f_y(x, y)) = (x + d_x(x, y), y + d_y(x, y))},
#' it computes the local Jacobian transformation matrix:
#' \deqn{\mathbf{J} = \begin{pmatrix} \frac{\partial f_x}{\partial x} & \frac{\partial f_x}{\partial y} \\ \frac{\partial f_y}{\partial x} & \frac{\partial f_y}{\partial y} \end{pmatrix}}
#'
#' **Metric Tensor Elements:**
#' \deqn{E = \left(\frac{\partial f_x}{\partial x}\right)^2 + \left(\frac{\partial f_y}{\partial x}\right)^2}
#' \deqn{G = \left(\frac{\partial f_x}{\partial y}\right)^2 + \left(\frac{\partial f_y}{\partial y}\right)^2}
#' \deqn{F = \frac{\partial f_x}{\partial x}\frac{\partial f_x}{\partial y} + \frac{\partial f_y}{\partial x}\frac{\partial f_y}{\partial y}}
#'
#' **Tissot Indicatrix Semi-Axes (Singular Values):**
#' The maximum and minimum local linear scale distortions \eqn{a} and \eqn{b}
#' correspond to the singular values of \eqn{\mathbf{J}}:
#' \deqn{a = \sqrt{\frac{1}{2}\left(E + G + \sqrt{(E - G)^2 + 4F^2}\right)}}
#' \deqn{b = \sqrt{\frac{1}{2}\left(E + G - \sqrt{(E - G)^2 + 4F^2}\right)}}
#'
#' **Signed Jacobian Determinant & Topological Inversion Detection:**
#' While the classical area scale factor is \eqn{a \cdot b = |\det(\mathbf{J})|},
#' `analyze_distortion` explicitly evaluates the signed Jacobian determinant:
#' \deqn{\det(\mathbf{J}) = \frac{\partial f_x}{\partial x}\frac{\partial f_y}{\partial y} - \frac{\partial f_x}{\partial y}\frac{\partial f_y}{\partial x}}
#' If \eqn{\det(\mathbf{J}) \le 0}, the transformation locally suffers a topological
#' inversion (fold-over, self-intersection, or dimensional collapse). An automatic
#' warning is issued when \code{det_J <= 0} is detected, and the flag \code{is_inverted}
#' is recorded.
#'
#' **Angular and Total Distortion Criteria:**
#' \itemize{
#'   \item Maximum Angular Distortion \eqn{2\Omega = 2 \arcsin\left(\frac{a - b}{a + b}\right)},
#'     reported in \strong{degrees}.
#'   \item Airy-Kavrayskiy Measure, with both semi-axes normalised by
#'     \code{reference_scale} \eqn{s_{\text{ref}}}:
#'     \deqn{E_{AK} = \sqrt{\frac{1}{2}\left(\left(\ln \frac{a}{s_{\text{ref}}}\right)^2 + \left(\ln \frac{b}{s_{\text{ref}}}\right)^2\right)}}
#'   \item Principal Axis Orientation \eqn{\theta_a}, in \strong{degrees}. Writing
#'     \eqn{\alpha = \frac{1}{2}\arctan_2(2F,\, E - G)} for the principal direction
#'     in the source plane (the eigenvector angle of the metric tensor), the
#'     reported orientation is that direction carried through the transformation:
#'     \deqn{\theta_a = \arctan_2\left(\frac{\partial f_y}{\partial x}\cos\alpha + \frac{\partial f_y}{\partial y}\sin\alpha,\; \frac{\partial f_x}{\partial x}\cos\alpha + \frac{\partial f_x}{\partial y}\sin\alpha\right)}
#'     This is the major-axis orientation of the indicatrix in the target plane,
#'     equal to the angle of the first left singular vector of \eqn{\mathbf{J}}.
#' }
#'
#' All three angular quantities are returned in degrees.
#'
#' @references
#' \itemize{
#'   \item Tissot, A. (1881). \emph{Mémoire sur la représentation des surfaces et les projections des cartes géographiques}. Gauthier-Villars.
#'   \item Snyder, J. P. (1987). \emph{Map Projections: A Working Manual}. U.S. Geological Survey Professional Paper 1395.
#'   \item Vantas, K., & Mirkopoulou, E. (2025). \emph{mapAI: An R Package for Positional Accuracy Improvement of Vector Maps}.
#' }
#'
#' @param pai_model A model object of class `pai_model`.
#' @param newdata A data frame with `source_x` and `source_y` columns, or a
#'   `terra` `SpatRaster` object. If `NULL` (default), the GCPs used to train
#'   the model will be used. If a `SpatRaster` is provided, distortion metrics
#'   are evaluated at each cell center and returned as a multi-layer `SpatRaster`.
#' @param reference_scale A single positive number giving the reference
#'   \strong{linear} scale \eqn{s_{\text{ref}}} against which distortion is
#'   measured (for example the scale factor of a global Helmert fit, or `1` for
#'   none). `log2_area_scale` is normalised by \eqn{s_{\text{ref}}^2} and
#'   `airy_kavrayskiy` by \eqn{s_{\text{ref}}}. Supply a linear scale, not an
#'   area scale.
#'
#' @return A `distortion` object (a data frame) or a `terra::SpatRaster` object
#'   with all calculated distortion metrics: `a`, `b` (principal semi-axes),
#'   `area_scale` (\eqn{= a \cdot b = |\det \mathbf{J}|}), `signed_area_scale`,
#'   `det_J`, `is_inverted`, `log2_area_scale`, `max_angular_distortion`
#'   (degrees), `airy_kavrayskiy`, and `theta_a` (degrees). For a `SpatRaster`
#'   input the layers carry these names and the `NA` mask of the input is
#'   preserved.
#'
#' @importFrom terra crds rast values<- ncell hasValues mask
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

  is_raster <- inherits(newdata, "SpatRaster")

  if (is_raster) {
    orig_raster <- newdata
    # na.rm = FALSE is essential: terra::crds() defaults to dropping NA cells,
    # which would return fewer coordinates than the raster has cells and cause
    # the metrics matrix to be silently recycled into the wrong cells.
    coords_mat <- terra::crds(newdata, na.rm = FALSE)
    newdata <- data.frame(source_x = coords_mat[, 1],
                          source_y = coords_mat[, 2])
  } else if (!is.null(newdata)) {
    new_data_validation(newdata)
  } else {
    newdata <- pai_model$gcp
  }

  message(paste("Calculating distortion metrics for",
                pai_model$model_info$label, "model..."))

  # ---  Numerical Derivatives Calculation ---

  # Determine a small step size h for the central differences. The span of
  # `newdata` is degenerate for a single evaluation point or a collinear set, so
  # fall back to the span of the GCPs the model was trained on, and finally to an
  # absolute floor. Without this, h = 0 and every metric silently becomes NaN.
  span_of <- function(x, y) {
    if (is.null(x) || is.null(y)) return(0)
    s <- max(c(diff(range(x, na.rm = TRUE)), diff(range(y, na.rm = TRUE))),
             na.rm = TRUE)
    if (!is.finite(s)) 0 else s
  }

  coord_range <- span_of(newdata$source_x, newdata$source_y)
  if (coord_range <= 0) {
    coord_range <- span_of(pai_model$gcp$source_x, pai_model$gcp$source_y)
  }
  if (coord_range <= 0) {
    coord_range <- max(1, mean(abs(c(newdata$source_x, newdata$source_y)),
                               na.rm = TRUE))
  }
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
  b <- sqrt(0.5 * pmax(0, E + G - sqrt_term))

  # Signed Jacobian determinant: det(J) = (dfx/dx)(dfy/dy) - (dfx/dy)(dfy/dx)
  det_J <- dfx_dx * dfy_dy - dfx_dy * dfy_dx
  area_scale <- a * b
  signed_area_scale <- det_J
  is_inverted <- det_J <= 0

  n_inverted <- sum(is_inverted, na.rm = TRUE)
  if (n_inverted > 0) {
    warning(
      sprintf("Topological fold-over detected: %d location(s) have non-positive Jacobian determinant (det_J <= 0).",
              n_inverted),
      call. = FALSE
    )
  }

  sum_ab <- a + b
  diff_ab <- a - b
  ratio_ab <- ifelse(sum_ab > 0, diff_ab / sum_ab, 0)
  ratio_ab <- pmin(1, pmax(-1, ratio_ab))

  # Maximum angular distortion 2*Omega, reported in degrees like every other
  # angular column of the result.
  max_angular_distortion <- 2 * asin(ratio_ab) * 180 / pi

  # Airy-Kavrayskiy criterion. Both semi-axes are normalised by reference_scale
  # before the logarithm, and the outer square root is part of the standard
  # definition; without them the value is dominated by the global map scale.
  a_ref <- pmax(1e-12, a / reference_scale)
  b_ref <- pmax(1e-12, b / reference_scale)
  airy_kavrayskiy <- sqrt(0.5 * (log(a_ref)^2 + log(b_ref)^2))

  # Orientation of the major semi-axis of Tissot's indicatrix, in degrees.
  #
  # alpha_p is the principal direction in the SOURCE plane: the eigenvector angle
  # of the metric tensor [[E, F], [F, G]], satisfying tan(2*alpha) = 2F/(E - G).
  # The reported orientation is that direction carried through the transformation,
  # i.e. the angle of J %*% (cos alpha_p, sin alpha_p), which is the major-axis
  # orientation in the TARGET plane and matches the first left singular vector of
  # J. Do not subtract alpha_p from atan2(dfy_dx, dfx_dx): that mixes a source
  # angle with a target angle and is only correct when F = 0.
  alpha_p <- atan2(2 * F_metric, E - G) / 2
  theta_a <- atan2(dfy_dx * cos(alpha_p) + dfy_dy * sin(alpha_p),
                   dfx_dx * cos(alpha_p) + dfx_dy * sin(alpha_p)) * 180 / pi

  if (is_raster) {
    metrics_mat <- cbind(
      a = a,
      b = b,
      area_scale = area_scale,
      signed_area_scale = signed_area_scale,
      det_J = det_J,
      is_inverted = as.numeric(is_inverted),
      log2_area_scale = log2(pmax(1e-12, area_scale) / (reference_scale^2)),
      max_angular_distortion = max_angular_distortion,
      airy_kavrayskiy = airy_kavrayskiy,
      theta_a = theta_a
    )
    if (nrow(metrics_mat) != terra::ncell(orig_raster)) {
      stop("Internal error: computed ", nrow(metrics_mat), " metric rows for a ",
           terra::ncell(orig_raster), "-cell raster.", call. = FALSE)
    }
    out_rast <- terra::rast(orig_raster, nlyrs = ncol(metrics_mat))
    names(out_rast) <- colnames(metrics_mat)
    terra::values(out_rast) <- metrics_mat
    # Carry the input's NA mask through, so cells that held no data do not come
    # back populated with distortion values.
    if (terra::hasValues(orig_raster)) {
      out_rast <- terra::mask(out_rast, orig_raster[[1]])
    }
    message("Distortion analysis complete.")
    return(out_rast)
  } else {
    # Add results to a new data frame to avoid modifying the input object directly
    results <- newdata
    results$a <- a
    results$b <- b
    results$area_scale <- area_scale
    results$signed_area_scale <- signed_area_scale
    results$det_J <- det_J
    results$is_inverted <- is_inverted
    results$log2_area_scale <- log2(pmax(1e-12, area_scale) / (reference_scale^2))
    results$max_angular_distortion <- max_angular_distortion
    results$airy_kavrayskiy <- airy_kavrayskiy
    results$theta_a <- theta_a

    class(results) <- c("distortion", "data.frame")

    message("Distortion analysis complete.")
    return(results)
  }
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
  cat(" - area_scale: Area distortion factor (a * b)\n")
  cat(" - signed_area_scale: Signed area distortion (Jacobian determinant)\n")
  cat(" - det_J: Jacobian determinant\n")
  cat(" - is_inverted: Topological fold-over indicator (det_J <= 0)\n")
  cat(" - log2_area_scale: Log2 area distortion relative to reference scale\n")
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

  metrics <- c("a", "b", "area_scale", "signed_area_scale", "det_J", "is_inverted",
               "log2_area_scale", "max_angular_distortion",
               "airy_kavrayskiy", "theta_a")

  summary_list <- lapply(metrics, function(metric) {
    if (metric %in% names(object)) {
      data <- as.numeric(object[[metric]])
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
  rownames(summary_df) <- metrics[metrics %in% names(object)]
  return(as.data.frame(summary_df))
}

#' @title Visualize a Distortion Metric as a Continuous Surface
#' @description Creates a smooth, interpolated surface visualization of a
#'   distortion metric from the output of `analyze_distortion()`.
#'
#' @details This function visualizes the distortion field as it exists on the
#'   **source map's coordinate space**. It uses linear interpolation via the
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
      x = x$source_x,
      y = x$source_y,
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
    source_x = rep(interp_result$x, times = length(interp_result$y)),
    source_y = rep(interp_result$y, each = length(interp_result$x)),
    metric_val = as.vector(interp_result$z)
  )
  # Remove NA values which can occur at the convex hull of the points
  interp_df <- stats::na.omit(interp_df)

  # --- 3. Create the Base Plot ---
  p <- ggplot2::ggplot(interp_df, aes(x = .data$source_x, y = .data$source_y)) +
    geom_raster(aes(fill = .data$metric_val)) +
    coord_equal(expand = FALSE) +
    labs(
      title = paste("Distortion Analysis:", metric),
      subtitle = "Interpolated surface from analysis points",
      x = "Source X Coordinate",
      y = "Source Y Coordinate",
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
        aes(x = .data$source_x, y = .data$source_y),
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
#' **source coordinate**. The size, shape, and orientation of the ellipse
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
    x_range <- diff(range(object$source_x, na.rm = TRUE))
    y_range <- diff(range(object$source_y, na.rm = TRUE))
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
      x0 = .data$source_x,
      y0 = .data$source_y,
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
      x = "Source X Coordinate",
      y = "Source Y Coordinate"
    ) +
    theme_minimal()

  return(p)
}
