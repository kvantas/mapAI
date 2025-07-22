#' @title Plot Model Residuals at Homologous Point Locations
#' @description Creates a vector plot showing the residual errors of a trained
#'   PAI model at the location of the Ground Control Points (GCPs).
#'
#' @details This is a crucial diagnostic function for assessing model
#' performance. It answers the question: "What errors did the model fail to
#' correct?"
#'
#' The function first predicts the correction for each GCP. It then calculates
#' the model's predicted target coordinate for each point. The resulting arrows
#' are drawn starting from this **predicted target location** and pointing to
#' the
#' **true target location**.
#'
#' A perfect model would have zero-length residual vectors. The presence of long
#' arrows or clear spatial patterns in the residuals may indicate that the
#' chosen model was not complex enough to capture the full distortion pattern.
#'
#' @param pai_model An object of class `pai_model` from `train_pai_model()`.
#' @param gcp_data An `sf` object of homologous points, from `read_gcps()`.
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
#'
#' @return A `ggplot` object, which can be further customized.
#'
#' @import sf
#' @import ggplot2
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom grid arrow unit
#' @export
#' @examples
#' # --- 1. Load data and train a simple model ---
#' data(swiss_cps)
#' # A helmert model is used as it will leave significant residuals.
#' helmert_model <- train_pai_model(swiss_cps, pai_method = "helmert")
#'
#' # --- 2. Plot the residuals with default colors ---
#' plot_residuals(helmert_model, swiss_cps)
#'
#' # --- 3. Exaggerate the residuals to make them more visible ---
#' plot_residuals(helmert_model, swiss_cps, exaggeration_factor = 10)
#'
#' # --- 4. Compare with a more advanced model ---
#'   gam_model <- train_pai_model(swiss_cps, pai_method = "gam")
#'
#' # The residuals for the GAM model should be much smaller and more random.
#' plot_residuals(gam_model, swiss_cps,
#'                arrow_color = "#006d2c", # A dark green
#'                point_color = "#66c2a5"  # A lighter green
#' )
#'
plot_residuals <- function(
    pai_model, gcp_data,
    title = "Model Residual Error Vectors",
    subtitle = "Arrows point from predicted to true target locations",
    arrow_color = "darkblue",
    point_color = "blue",
    exaggeration_factor = 1) {

  # --- Input Validation ---
  if (!inherits(pai_model, "pai_model")) {
    stop("`pai_model` must be an object of class 'pai_model'.", call. = FALSE)
  }

  if (!inherits(gcp_data, "sf")) {
    stop("`gcp_data` must be a valid `sf` object.", call. = FALSE)
  }

  message("Calculating model residuals...")

  # Predict the displacements
  predicted_displacements <- predict(pai_model,
                                     newdata = sf::st_drop_geometry(gcp_data))

  # Create a clean data frame for plotting
  plot_df <- sf::st_drop_geometry(gcp_data) %>%
    dplyr::select("source_x", "source_y", "target_x", "target_y") %>%
    dplyr::mutate(
      dx_pred = predicted_displacements$dx,
      dy_pred = predicted_displacements$dy,
      predicted_target_x = .data$source_x + .data$dx_pred,
      predicted_target_y = .data$source_y + .data$dy_pred
    )

  # Create the plot
  residual_plot <- ggplot2::ggplot(plot_df) +
    # Draw arrows from the predicted location to the true location
    ggplot2::geom_segment(
      ggplot2::aes(
        x = .data$predicted_target_x,
        y = .data$predicted_target_y,
        xend = .data$predicted_target_x + exaggeration_factor * (.data$target_x - .data$predicted_target_x),
        yend = .data$predicted_target_y + exaggeration_factor * (.data$target_y - .data$predicted_target_y)
      ),
      arrow = grid::arrow(length = grid::unit(0.1, "cm")),
      color = arrow_color, # Use the parameter
      alpha = 0.7
    ) +
    # Add a point at the start of each arrow (the predicted location)
    ggplot2::geom_point(
      ggplot2::aes(x = .data$predicted_target_x, y = .data$predicted_target_y),
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

  return(residual_plot)
}
