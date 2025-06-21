#' @title Plot a Distortion Metric as a Surface
#' @description Creates a rasterized surface plot to visualize a continuous
#'   distortion metric across the analysis area.
#' @details This function takes the output from `analyze_distortion()` and creates
#'   a smooth surface plot of a chosen metric (e.g., `area_scale`, `max_shear`).
#'   It is highly recommended to run the analysis on a regular grid of points
#'   (created with `sf::st_make_grid()`) to produce an accurate surface.
#'
#'   The function allows for both sequential and diverging color palettes, which
#'   should be chosen based on the nature of the metric being plotted.
#'
#' @param distortion_sf An `sf` object returned by `analyze_distortion()`.
#' @param metric A character string specifying the name of the metric column to plot.
#'   Must be one of "a", "b", "area_scale", "max_shear", or "theta_a".
#' @param gcp_data An optional `sf` object of the original GCPs to overlay on the
#'   plot for context.
#' @param palette A character string specifying the name of a viridis color palette
#'   (e.g., "viridis", "magma", "cividis"). Defaults to "viridis".
#' @param diverging A logical value. If `TRUE`, a diverging color scale centered
#'   at a meaningful midpoint is used (e.g., for `area_scale` centered at 1).
#'   If `FALSE` (default), a standard sequential scale is used.
#'
#' @return A `ggplot` object.
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom rlang .data sym
#' @export
#' @examples
#' # --- 1. Train a TPS model ---
#' data(gcps)
#' tps_model <- train_pai_model(gcps, method = "tps")
#'
#' # --- 2. Create a grid and analyze distortion on it ---
#' analysis_grid <- sf::st_make_grid(gcps, n = c(25, 25)) %>%
#'   sf::st_sf() %>%
#'   st_set_crs(st_crs(gcps))
#'
#' distortion_on_grid <- analyze_distortion(tps_model, analysis_grid)
#'
#' # --- 3. Plot the results ---
#' # Plot maximum shear (a sequential metric)
#' plot_distortion_surface(distortion_on_grid, metric = "max_shear", gcp_data = gcps)
#'
#' # Plot areal distortion (a diverging metric, centered at 1)
#' plot_distortion_surface(distortion_on_grid, metric = "area_scale", gcp_data = gcps,
#'                         palette = "cividis", diverging = TRUE)
#'
plot_distortion_surface <- function(distortion_sf, metric, gcp_data = NULL,
                                    palette = "viridis", diverging = FALSE) {

  # --- Input Validation ---
  valid_metrics <- c("a", "b", "area_scale", "max_shear", "theta_a")
  if (!metric %in% valid_metrics) {
    stop(paste("`metric` must be one of:", paste(valid_metrics, collapse = ", ")), call. = FALSE)
  }
  if (!metric %in% names(distortion_sf)) {
    stop(paste("Metric", metric, "not found in the input data."), call. = FALSE)
  }

  # Use `sym` to safely pass the column name to ggplot's aes()
  metric_sym <- rlang::sym(metric)

  # --- Create the plot ---
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = distortion_sf, ggplot2::aes(fill = !!metric_sym), color = NA) +
    ggplot2::coord_sf(datum = sf::st_crs(distortion_sf), expand = FALSE) +
    ggplot2::labs(
      title = paste("Distortion Surface:", metric),
      fill = metric
    ) +
    ggplot2::theme_minimal()

  # --- Add GCPs if provided ---
  if (!is.null(gcp_data)) {
    p <- p + ggplot2::geom_sf(data = gcp_data, shape = 3, color = "red", size = 0.8, alpha = 0.7)
  }

  # --- Apply Color Scale ---
  if (diverging) {
    # For diverging scales (like area_scale), center the palette at 1
    midpoint <- if (metric == "area_scale") 1 else 0
    p <- p + ggplot2::scale_fill_gradient2(
      low = "#3B4CC0", mid = "#F1F1F1", high = "#B40426",
      midpoint = midpoint
    )
  } else {
    p <- p + ggplot2::scale_fill_viridis_c(option = palette)
  }

  return(p)
}

#' @title Plot Tissot's Indicatrices
#' @description Visualizes distortion at specific points by drawing Tissot's
#'   indicatrices (ellipses).
#' @details This function creates a visual representation of distortion at each point
#'   in the input data. Each ellipse graphically shows the magnitude and direction
#'   of distortion:
#'   \itemize{
#'     \item The **shape** of the ellipse shows the angular distortion (shear). A perfect circle means no shear.
#'     \item The **size** of the ellipse shows the areal distortion. An ellipse larger than a reference circle means expansion.
#'     \item The **orientation** of the ellipse shows the direction of maximum scale.
#'   }
#' @param distortion_sf An `sf` object returned by `analyze_distortion()`. It must
#'   contain the columns a, b, and theta_a.
#' @param scale_factor A numeric value to control the size of the plotted ellipses
#'   for better visibility. You may need to adjust this based on your map's scale.
#' @param fill_metric An optional character string specifying a metric from
#'   `distortion_sf` to map to the fill color of the ellipses (e.g., "area_scale").
#'
#' @return A `ggplot` object.
#'
#' @import sf
#' @import ggplot2
#' @import dplyr
#' @importFrom rlang .data
#' @export
#' @examples
#' # --- 1. Train a TPS model and analyze distortion at GCPs ---
#' data(gcps)
#' tps_model <- train_pai_model(gcps, method = "tps")
#' distortion_at_gcps <- analyze_distortion(tps_model, gcps)
#'
#' # --- 2. Plot the indicatrices ---
#' # Adjust scale_factor for best visibility
#' plot_indicatrices(distortion_at_gcps, scale_factor = 15)
#'
#' # --- 3. Color the ellipses by the areal distortion ---
#' plot_indicatrices(distortion_at_gcps, scale_factor = 15, fill_metric = "area_scale")
#'
plot_indicatrices <- function(distortion_sf, scale_factor = 1, fill_metric = NULL) {

  # --- Input Validation ---
  required_cols <- c("a", "b", "theta_a")
  if (!all(required_cols %in% names(distortion_sf))) {
    stop("Input data must be the output of `analyze_distortion()`.", call. = FALSE)
  }
  if (!is.null(fill_metric) && !fill_metric %in% names(distortion_sf)) {
    stop(paste("`fill_metric` '", fill_metric, "' not found in the input data."), call. = FALSE)
  }

  message("Generating indicatrix polygons...")
  indicatrices_list <- vector("list", nrow(distortion_sf))

  # --- Create Ellipse for each point ---
  for (i in 1:nrow(distortion_sf)) {
    point_data <- distortion_sf[i, ]
    center_coords <- sf::st_coordinates(point_data)

    a <- point_data$a
    b <- point_data$b
    theta_rad <- point_data$theta_a * pi / 180 # Convert to radians

    # 1. Create a base circle
    t <- seq(0, 2 * pi, length.out = 51)
    base_circle <- cbind(cos(t), sin(t))

    # 2. Scale into an ellipse based on a and b
    scaled_ellipse <- base_circle %*% diag(c(a, b))

    # 3. Rotate the ellipse
    rotation_matrix <- matrix(c(cos(theta_rad), sin(theta_rad), -sin(theta_rad), cos(theta_rad)), 2, 2)
    rotated_ellipse <- scaled_ellipse %*% rotation_matrix

    # 4. Scale for visibility and translate to the correct location
    final_ellipse <- rotated_ellipse * scale_factor + rep(center_coords, each = 51)

    indicatrices_list[[i]] <- sf::st_polygon(list(final_ellipse))
  }

  # --- Create the final sf object ---
  indicatrices_sf <- sf::st_sfc(indicatrices_list, crs = sf::st_crs(distortion_sf))
  # Keep original data attributes
  indicatrices_sf <- sf::st_sf(distortion_sf, geometry = indicatrices_sf)


  # --- Create the plot ---
  p <- ggplot2::ggplot() +
    ggplot2::labs(
      title = "Tissot's Indicatrices of Distortion",
      subtitle = "Ellipses show magnitude and direction of distortion"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::coord_sf(datum = sf::st_crs(distortion_sf))

  # Add layers based on whether a fill metric is provided
  if (!is.null(fill_metric)) {
    fill_metric_sym <- rlang::sym(fill_metric)
    p <- p + ggplot2::geom_sf(data = indicatrices_sf, ggplot2::aes(fill = !!fill_metric_sym), alpha = 0.7) +
      ggplot2::scale_fill_viridis_c()
  } else {
    p <- p + ggplot2::geom_sf(data = indicatrices_sf, fill = "lightblue", color = "black", alpha = 0.7)
  }

  # Add original points for context
  p <- p + ggplot2::geom_sf(data = distortion_sf, shape = 21, size = 1, fill = "red")

  return(p)
}

#' @title Plot a Distortion Metric as a Surface
#' @description Creates a rasterized surface plot to visualize a continuous
#'   distortion metric across the analysis area.
#' @details This function takes the output from `analyze_distortion()` and creates
#'   a smooth surface plot of a chosen metric (e.g., `area_scale`, `max_shear`).
#'   It is highly recommended to run the analysis on a regular grid of points
#'   (created with `sf::st_make_grid()`) to produce an accurate surface.
#'
#'   The function allows for both sequential and diverging color palettes, which
#'   should be chosen based on the nature of the metric being plotted.
#'
#' @param distortion_sf An `sf` object returned by `analyze_distortion()`.
#' @param metric A character string specifying the name of the metric column to plot.
#'   Must be one of "a", "b", "area_scale", "max_shear", or "theta_a".
#' @param gcp_data An optional `sf` object of the original GCPs to overlay on the
#'   plot for context.
#' @param palette A character string specifying the name of a viridis color palette
#'   (e.g., "viridis", "magma", "cividis"). Defaults to "viridis".
#' @param diverging A logical value. If `TRUE`, a diverging color scale centered
#'   at a meaningful midpoint is used (e.g., for `area_scale` centered at 1).
#'   If `FALSE` (default), a standard sequential scale is used.
#'
#' @return A `ggplot` object.
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom rlang .data sym
#' @export
#' @examples
#' # --- 1. Train a TPS model ---
#' data(gcps)
#' tps_model <- train_pai_model(gcps, method = "tps")
#'
#' # --- 2. Create a grid and analyze distortion on it ---
#' analysis_grid <- sf::st_make_grid(gcps, n = c(25, 25)) %>%
#'   sf::st_sf() %>%
#'   st_set_crs(st_crs(gcps))
#'
#' distortion_on_grid <- analyze_distortion(tps_model, analysis_grid)
#'
#' # --- 3. Plot the results ---
#' # Plot maximum shear (a sequential metric)
#' plot_distortion_surface(distortion_on_grid, metric = "max_shear", gcp_data = gcps)
#'
#' # Plot areal distortion (a diverging metric, centered at 1)
#' plot_distortion_surface(distortion_on_grid, metric = "area_scale", gcp_data = gcps,
#'                         palette = "cividis", diverging = TRUE)
#'
plot_distortion_surface <- function(distortion_sf, metric, gcp_data = NULL,
                                    palette = "viridis", diverging = FALSE) {

  # --- Input Validation ---
  valid_metrics <- c("a", "b", "area_scale", "max_shear", "theta_a")
  if (!metric %in% valid_metrics) {
    stop(paste("`metric` must be one of:", paste(valid_metrics, collapse = ", ")), call. = FALSE)
  }
  if (!metric %in% names(distortion_sf)) {
    stop(paste("Metric", metric, "not found in the input data."), call. = FALSE)
  }

  # Use `sym` to safely pass the column name to ggplot's aes()
  metric_sym <- rlang::sym(metric)

  # --- Create the plot ---
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = distortion_sf, ggplot2::aes(fill = !!metric_sym), color = NA) +
    ggplot2::coord_sf(datum = sf::st_crs(distortion_sf), expand = FALSE) +
    ggplot2::labs(
      title = paste("Distortion Surface:", metric),
      fill = metric
    ) +
    ggplot2::theme_minimal()

  # --- Add GCPs if provided ---
  if (!is.null(gcp_data)) {
    p <- p + ggplot2::geom_sf(data = gcp_data, shape = 3, color = "red", size = 0.8, alpha = 0.7)
  }

  # --- Apply Color Scale ---
  if (diverging) {
    # For diverging scales (like area_scale), center the palette at 1
    midpoint <- if (metric == "area_scale") 1 else 0
    p <- p + ggplot2::scale_fill_gradient2(
      low = "#3B4CC0", mid = "#F1F1F1", high = "#B40426",
      midpoint = midpoint
    )
  } else {
    p <- p + ggplot2::scale_fill_viridis_c(option = palette)
  }

  return(p)
}
