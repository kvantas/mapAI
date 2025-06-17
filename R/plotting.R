#' @title Plot the Learned Correction Surface
#' @description Visualizes the spatial correction field (dx, dy) as interpolated
#'   raster surfaces. This is a diagnostic plot to understand the transformation.
#' @param pai_model A trained `pai_model` object.
#' @param gcp_data The `sf` object of homologous points used for training.
#' @param n_grid The resolution of the interpolation grid (default is 100).
#' @return A `patchwork` object containing two `ggplot` plots for dx and dy.
#' @import ggplot2
#' @import dplyr
#' @import patchwork
#' @import viridis
#' @importFrom rlang .data
#' @export
plot_correction_surface <- function(pai_model, gcp_data, n_grid = 100) {
  if (!inherits(pai_model, "pai_model")) stop("pai_model must be a valid pai_model object.")
  if (!inherits(gcp_data, "sf")) stop("gcp_data must be a valid sf object.")

  bbox <- sf::st_bbox(gcp_data)
  grid_to_predict <- expand.grid(
    source_x = seq(bbox["xmin"], bbox["xmax"], length.out = n_grid),
    source_y = seq(bbox["ymin"], bbox["ymax"], length.out = n_grid)
  )
  # Use the new S3 predict method
  predictions <- predict(pai_model, newdata = grid_to_predict)
  plot_data <- cbind(grid_to_predict, predictions)

  # Plot for dx
  p_dx <- ggplot(plot_data, aes(x = .data$source_x, y = .data$source_y, fill = .data$dx)) +
    geom_raster() +
    geom_contour(aes(z = .data$dx), color = "white", alpha = 0.4, bins = 12) +
    scale_fill_viridis(option = "cividis", name = "dx") +
    geom_point(data = gcp_data, inherit.aes = FALSE,
               mapping = aes(x = .data$source_x, y = .data$source_y),
               shape = 3, color = "red", size = 0.8, alpha = 0.7) +
    labs(title = "Correction Surface (dx)", x = "Source X", y = "Source Y") +
    coord_equal() + theme_minimal()

  # Plot for dy
  p_dy <- ggplot(plot_data, aes(x = .data$source_x, y = .data$source_y, fill = .data$dy)) +
    geom_raster() +
    geom_contour(aes(z = .data$dy), color = "white", alpha = 0.4, bins = 12) +
    scale_fill_viridis(option = "cividis", name = "dy") +
    geom_point(data = gcp_data, inherit.aes = FALSE,
               mapping = aes(x = .data$source_x, y = .data$source_y),
               shape = 3, color = "red", size = 0.8, alpha = 0.7) +
    labs(title = "Correction Surface (dy)", x = "Source X", y = "Source Y") +
    coord_equal() + theme_minimal()

  return(p_dx + p_dy)
}
