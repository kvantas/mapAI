#' @title Plot Displacement Vectors
#' @description Creates a visualization of the displacement vectors from source
#'   (distorted) to target (true) coordinates for a set of homologous points.
#' @details This function is a key exploratory tool for understanding the nature
#'   and magnitude of positional error in a dataset before correction. It plots
#'   arrows that originate at the distorted `source` coordinates and point to
#'   the correct `target` coordinates. This provides an immediate visual sense
#'   of the spatial patterns in the distortion (e.g., rotation, scaling, or
#'   non-linear warping).
#'
#' @param gcp_data An `sf` object of homologous points, typically the output of
#'   `read_gcps()`. Must contain `source_x`, `source_y`, `target_x`, and
#'   `target_y` columns.
#' @param title A character string for the plot's main title.
#' @param subtitle A character string for the plot's subtitle.
#' @param arrow_color A character string specifying the color of the
#'   displacement arrows.
#' @param point_color A character string specifying the color of the points
#'   marking the source locations.
#'
#' @return A `ggplot` object, which can be further customized using standard
#'   `ggplot2` syntax.
#'
#' @import ggplot2
#' @importFrom rlang .data
#' @importFrom grid arrow unit
#' @export
#' @examples
#' # --- 1. First, create a demo dataset to work with ---
#' demo_files <- create_demo_data(type = "complex")
#' gcp_data <- read_gcps(gcp_path = demo_files$gcp_path, crs = 3857)
#'
#' # --- 2. Create the default displacement plot ---
#' plot_displacement(gcp_data)
#'
#' # --- 3. Customize the plot with different titles and colors ---
#' plot_displacement(
#'   gcp_data,
#'   title = "My Custom Displacement Plot",
#'   subtitle = "Visualizing error vectors",
#'   arrow_color = "blue",
#'   point_color = "orange"
#' )
#'
plot_displacement <- function(
    gcp_data,
    title = "Distortion Displacement Vectors",
    subtitle = "Arrows point from distorted to true locations",
    arrow_color = "darkred",
    point_color = "red") {

  # --- 1. Input Validation ---
  if (!inherits(gcp_data, "sf")) {
    stop("`gcp_data` must be an sf object, typically created by `read_gcps()`.",
         call. = FALSE)
  }
  required_cols <- c("source_x", "source_y", "target_x", "target_y")
  if (!all(required_cols %in% names(gcp_data))) {
    stop("`gcp_data` is missing required columns for plotting.",
         call. = FALSE)
  }

  # --- 2. Create the Plot ---
  # Using the .data pronoun for robust programming with dplyr/ggplot2
  displacement_plot <- ggplot2::ggplot(gcp_data) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = .data$source_x,
        y = .data$source_y,
        xend = .data$target_x,
        yend = .data$target_y
      ),
      arrow = grid::arrow(length = grid::unit(0.1, "cm")),
      color = arrow_color,
      alpha = 0.6
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = .data$source_x, y = .data$source_y),
      color = point_color,
      size = 0.5,
      shape = 1 # Use a hollow circle for better visibility
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Source X Coordinate",
      y = "Source Y Coordinate"
    ) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal()

  # --- 3. Return the ggplot object ---
  return(displacement_plot)
}
