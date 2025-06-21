#' @title Plot a Distortion Metric
#' @description Creates a visualization of a continuous distortion metric from the
#'   output of `analyze_distortion()`.
#' @details
#' This function intelligently creates the best visualization for your data...
#' (rest of details section is unchanged)
#'
#' @param distortion_sf An `sf` object of **points** returned by `analyze_distortion()`.
#' @param metric A character string specifying the name of the metric column to plot.
#' @param gcp_data An optional `sf` object of the original GCPs to overlay on the plot.
#' @param palette A character string for the viridis color palette.
#' @param diverging A logical value for using a diverging color scale.
#'
#' @return A `ggplot` object.
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom rlang .data sym
#' @importFrom sf st_drop_geometry st_coordinates st_crs
#' @importFrom magrittr %>%
#' @export
#' @examples
#' # --- Setup: Train a model and create analysis points ---
#'
#' data(gcps)
#' library(magrittr)
#'
#' gam_model <- train_pai_model(gcps, method = "gam")
#' analysis_points <- sf::st_make_grid(gcps, n = c(25, 25)) %>%
#' sf::st_centroid() %>%
#'   sf::st_sf()
#'
#' distortion_on_grid <- analyze_distortion(gam_model, analysis_points)
#'
#' # --- Plot with a diverging scale ---
#' plot_distortion_surface(distortion_on_grid, metric = "area_scale", diverging = TRUE)
#'
plot_distortion_surface <- function(distortion_sf, metric, gcp_data = NULL,
                                    palette = "viridis", diverging = FALSE) {

  # --- Input Validation ---
  valid_metrics <- c("a", "b", "area_scale", "log2_area_scale", "max_shear", "max_angular_distortion", "theta_a")
  if (!metric %in% valid_metrics) {
    stop(paste("`metric` must be one of:", paste(valid_metrics, collapse = ", ")), call. = FALSE)
  }
  # ... The rest of the function code remains exactly the same ...
  if (!metric %in% names(distortion_sf)) {
    stop(paste("Metric", metric, "not found in the input data."), call. = FALSE)
  }
  plot_df <- sf::st_drop_geometry(distortion_sf) %>%
    dplyr::select(metric_val = !!rlang::sym(metric)) %>%
    dplyr::bind_cols(as.data.frame(sf::st_coordinates(distortion_sf)))
  n_x <- length(unique(round(plot_df$X, 6)))
  n_y <- length(unique(round(plot_df$Y, 6)))
  is_grid <- abs((n_x * n_y) - nrow(plot_df)) <= 2
  p <- ggplot2::ggplot() +
    ggplot2::coord_equal(expand = FALSE) +
    ggplot2::labs(title = paste("Distortion Analysis:", metric), x = "X", y = "Y") +
    ggplot2::theme_minimal()
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
  if (!is.null(gcp_data)) {
    p <- p + ggplot2::geom_sf(data = gcp_data, inherit.aes = FALSE, shape = 3, color = "black", size = 1.5)
  }
  if (diverging) {
    midpoint <- if (metric %in% c("area_scale", "log2_area_scale")) { if (metric == "area_scale") 1 else 0 } else 0
    p <- p + ggplot2::scale_color_gradient2(aesthetics = aesthetic_to_scale, low = "#3B4CC0", mid = "#F1F1F1", high = "#B40426", midpoint = midpoint)
  } else {
    p <- p + ggplot2::scale_color_viridis_c(aesthetics = aesthetic_to_scale, option = palette)
  }
  return(p)
}
