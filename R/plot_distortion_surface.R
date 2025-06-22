#' @title Visualize a Distortion Metric
#' @description Creates a visualization of a continuous distortion metric from the
#'   output of `analyze_distortion()`.
#'
#' @details
#' This function visualizes the distortion field as it exists on the **source map's
#' coordinate space**. This allows for a direct visual correlation between features on
#' the original map and the calculated distortion metrics.
#' \itemize{
#'   \item \strong{For points on a regular grid} (recommended): It creates a rasterized
#'     surface plot using `geom_raster()`, which is ideal for visualizing continuous
#'     distortion surfaces. To create a regular grid, use the pattern:
#'     `sf::st_make_grid(...) %>% sf::st_centroid() %>% sf::st_sf()`.
#'   \item \strong{For scattered, irregular points} (like the original GCPs): It creates a
#'     point plot where each point is colored by its metric value. This avoids
#'     memory errors and still provides a useful diagnostic plot, along with a
#'     message recommending the use of a grid for a true surface plot.
#' }
#'
#' **Interpreting Plots by Model Type**
#'
#' The visual output will depend on the model used to generate the
#' `distortion_sf` data:
#' \itemize{
#'   \item **`gam` model data**: Will produce a smooth, continuous surface with
#'     spatially varying colors, which is highly informative. This is the ideal
#'     model for this function.
#'   \item **`helmert` or `lm` model data**: Will produce a plot with a single,
#'     uniform color, as the underlying distortion metrics are constant across the map.
#'   \item **`rf` model data**: Will produce a plot indicating an Identity
#'     transformation (`area_scale` = 1, `max_shear` = 0). While RF is an excellent
#'     correction tool, this analysis is not informative for it.
#' }
#'
#' You may see a benign warning from `ggplot2` about "uneven horizontal intervals."
#' This is caused by minor floating-point inaccuracies in the grid coordinates and
#' can be safely ignored; the plot is still accurate.
#'
#' @param distortion_sf An `sf` object of **points** returned by `analyze_distortion()`.
#' @param metric A character string specifying the name of the metric column to plot.
#'   Must be one of "a", "b", "area_scale", "log2_area_scale", "max_shear",
#'   "max_angular_distortion", "airy_kavrayskiy", or "theta_a".
#' @param gcp_data An optional `sf` object of the original GCPs to overlay on the
#'   plot for context.
#' @param palette A character string specifying the name of a viridis color palette
#'   (e.g., "viridis", "magma", "cividis"), used when `diverging=FALSE`.
#' @param diverging A logical value. If `TRUE`, a diverging color scale is used,
#'   which is ideal for metrics like `area_scale` or `log2_area_scale`.
#'
#' @return A `ggplot` object, which can be further customized.
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom rlang .data sym
#' @importFrom sf st_drop_geometry st_coordinates st_crs
#' @importFrom magrittr %>%
#' @export
#' @examples
#' # --- 1. Train a GAM model for the best visual results ---
#' library(magrittr)
#' data(gcps)
#' gam_model <- train_pai_model(gcps, method = "gam")
#'
#' # --- 2. Create a regular grid of POINTS for analysis ---
#' analysis_points <- sf::st_make_grid(gcps, n = c(25, 25)) %>%
#'   sf::st_centroid() %>%
#'   sf::st_sf()
#'
#' distortion_on_grid <- analyze_distortion(gam_model, analysis_points)
#'
#' # --- 3. Plot a metric using a standard sequential scale ---
#' plot_distortion_surface(
#'   distortion_on_grid,
#'   metric = "max_shear",
#'   gcp_data = gcps,
#'   palette = "magma"
#' )
#'
#' # --- 4. Plot a metric using a diverging scale ---
#' # 'log2_area_scale' is ideal for this, as it's centered at 0.
#' plot_distortion_surface(
#'   distortion_on_grid,
#'   metric = "log2_area_scale",
#'   gcp_data = gcps,
#'   diverging = TRUE
#' )
#'
plot_distortion_surface <- function(distortion_sf, metric, gcp_data = NULL,
                                    palette = "viridis", diverging = FALSE) {

  # --- Input Validation ---
  valid_metrics <- c("a", "b", "area_scale", "log2_area_scale", "max_shear", "max_angular_distortion", "theta_a", "airy_kavrayskiy")
  if (!metric %in% valid_metrics) {
    stop(paste("`metric` must be one of:", paste(valid_metrics, collapse = ", ")), call. = FALSE)
  }
  if (!metric %in% names(distortion_sf)) {
    stop(paste("Metric", metric, "not found in the input data."), call. = FALSE)
  }

  # --- Prepare data & Detect Grid ---
  plot_df <- sf::st_drop_geometry(distortion_sf) %>%
    dplyr::select(metric_val = !!rlang::sym(metric)) %>%
    dplyr::bind_cols(as.data.frame(sf::st_coordinates(distortion_sf)))

  n_x <- length(unique(round(plot_df$X, 6)))
  n_y <- length(unique(round(plot_df$Y, 6)))
  is_grid <- abs((n_x * n_y) - nrow(plot_df)) <= 2

  # --- Create the base plot ---
  p <- ggplot2::ggplot() +
    ggplot2::coord_equal(expand = FALSE) +
    ggplot2::labs(title = paste("Distortion Analysis:", metric), x = "X Coordinate", y = "Y Coordinate") +
    ggplot2::theme_minimal()

  # --- Choose the right geom based on data type ---
  if (is_grid) {
    message("Regular grid detected. Creating a surface plot with geom_raster().")
    p <- p + ggplot2::geom_raster(data = plot_df, aes(x = .data$X, y = .data$Y, fill = .data$metric_val))
    aesthetic_to_scale <- "fill"
  } else {
    message("Scattered points detected. Creating a point plot with geom_point().")
    message("For a continuous surface plot, run analyze_distortion() on a regular grid of points.")
    p <- p + ggplot2::geom_point(data = plot_df, aes(x = .data$X, y = .data$Y, color = .data$metric_val), size = 3)
    aesthetic_to_scale <- "color"
  }

  p <- p + labs(fill = metric, color = metric)

  # --- Add GCPs if provided ---
  if (!is.null(gcp_data)) {
    p <- p + ggplot2::geom_sf(data = gcp_data, inherit.aes = FALSE, shape = 3, color = "black", size = 1.5)
  }

  # --- Apply Color Scale ---
  if (diverging) {
    midpoint <- if (metric == "area_scale") 1 else 0
    p <- p + ggplot2::scale_color_gradient2(
      aesthetics = aesthetic_to_scale,
      low = "#3B4CC0", mid = "#F1F1F1", high = "#B40426",
      midpoint = midpoint
    )
  } else {
    p <- p + ggplot2::scale_color_viridis_c(
      aesthetics = aesthetic_to_scale,
      option = palette
    )
  }

  return(p)
}
