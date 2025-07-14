#' @title Plot the Learned Correction Surface
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
#' @param pai_model A trained `pai_model` object returned by `train_pai_model()`.
#' @param gcp_data The `sf` object of homologous points that was used for
#'  training the model.
#' @param n_grid The resolution of the interpolation grid used to create the
#'  smooth surface. Higher values create a more detailed plot but take longer to
#'   compute. Defaults to 100.
#' @param plot_gcps A logical value indicating whether to plot the GCP
#' locations on the correction surfaces. Defaults to `TRUE`.
#'
#' @return A `patchwork` object containing two `ggplot` plots, one for `dx` and
#'   one for `dy`.
#'
#' @import ggplot2
#' @import dplyr
#' @import patchwork
#' @import viridis
#' @importFrom rlang .data
#' @export
#' @examples
#' \dontrun{
#' # This example demonstrates how to train a model and then visualize
#' # what it has learned.
#'
#' # --- 1. Load required data from the package ---
#' data(gcps)
#'
#' # --- 2. Train a model to visualize ---
#' # A GAM (Generalized Additive Model) is a great choice for this
#' # because it naturally produces smooth surfaces that are easy to interpret.
#' pai_model_gam <- train_pai_model(gcps, pai_method = "gam")
#'
#' # --- 3. Generate and display the plot ---
#' # This creates the two-panel plot showing the dx and dy correction fields.
#' correction_plot <- plot_correction_surface(
#'   pai_model = pai_model_gam,
#'   gcp_data = gcps,
#'   n_grid = 75 # Using a slightly coarser grid for a quick example
#' )
#'
#' # To display the plot in an interactive session:
#' print(correction_plot)
#'
#' # You can also hide the GCPs
#' correction_plot_no_gcps <- plot_correction_surface(
#'   pai_model = pai_model_gam,
#'   gcp_data = gcps,
#'   plot_gcps = FALSE
#' )
#' print(correction_plot_no_gcps)
#' }
plot_correction_surface <- function(pai_model,
                                    gcp_data,
                                    n_grid = 100,
                                    plot_gcps = TRUE) {
  if (!inherits(pai_model, "pai_model")) {
    stop("pai_model must be a valid pai_model object.")
  }

  if (!inherits(gcp_data, "sf")) {
    stop("gcp_data must be a valid sf object.")
    }

  bbox <- sf::st_bbox(gcp_data)
  grid_to_predict <- expand.grid(
    source_x = seq(bbox["xmin"], bbox["xmax"], length.out = n_grid),
    source_y = seq(bbox["ymin"], bbox["ymax"], length.out = n_grid)
  )
  # Use the new S3 predict method
  predictions <- predict(pai_model, newdata = grid_to_predict)
  plot_data <- cbind(grid_to_predict, predictions)

  # Plot for dx
  suppressWarnings(
  p_dx <- ggplot(plot_data,
                 aes(x = .data$source_x, y = .data$source_y, fill = .data$dx)) +
    geom_raster() +
    geom_contour(aes(z = .data$dx), color = "white", alpha = 0.4, bins = 12) +
    scale_fill_viridis(option = "viridis", name = "dx") +
    labs(title = "Correction Surface (dx)", x = "X", y = "Y") +
    coord_equal() + theme_minimal()
  )
  # Plot for dy
  suppressWarnings(
  p_dy <- ggplot(plot_data,
                 aes(x = .data$source_x, y = .data$source_y, fill = .data$dy)) +
    geom_raster() +
    geom_contour(aes(z = .data$dy), color = "white", alpha = 0.4, bins = 12) +
    scale_fill_viridis(option = "viridis", name = "dy") +
    labs(title = "Correction Surface (dy)", x = "X", y = "Y") +
    coord_equal() + theme_minimal()
)
  if (plot_gcps) {
    p_dx <- p_dx + geom_point(data = gcp_data, inherit.aes = FALSE,
                              mapping = aes(x = .data$source_x,
                                            y = .data$source_y),
                              shape = 3, color = "black", size = 0.8,
                              alpha = 0.7)
    p_dy <- p_dy + geom_point(data = gcp_data, inherit.aes = FALSE,
                              mapping = aes(x = .data$source_x,
                                            y = .data$source_y),
                              shape = 3, color = "black", size = 0.8,
                              alpha = 0.7)
  }
  return(p_dx + p_dy)
}
