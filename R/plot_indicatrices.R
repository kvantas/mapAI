#' @title Plot Tissot's Indicatrices of Distortion
#' @description Visualizes distortion by drawing Tissot's indicatrices (ellipses).
#' @details This function creates a pure visualization of the distortion ellipses.
#'   It takes the results from `analyze_distortion()` and draws an ellipse for each
#'   analyzed point. Each ellipse graphically shows the magnitude and direction
#'   of distortion at that location. The ellipses are drawn centered at their
#'   predicted target locations to show the state of distortion on the corrected map space.
#'
#' @param distortion_sf An `sf` object of **points** returned by `analyze_distortion()`. It must
#'   contain the columns `a`, `b`, and `theta_a`.
#' @param pai_model The `pai_model` object that was used to generate the `distortion_sf` data.
#' @param scale_factor A numeric value to control the size of the plotted ellipses
#'   for better visibility. You will need to adjust this based on your map's scale.
#' @param fill_color A character string specifying the fill color of the ellipses.
#' @param border_color A character string specifying the border color of the ellipses.
#'
#' @return A `ggplot` object containing only the distortion ellipses.
#'
#' @import sf
#' @import ggplot2
#' @import dplyr
#' @export
#' @examples
#' # --- 1. Train a model and analyze distortion ---
#' data(gcps)
#' gam_model <- train_pai_model(gcps, method = "gam")
#' distortion_at_gcps <- analyze_distortion(gam_model, gcps)
#'
#' # --- 2. Plot the indicatrices ---
#' # The scale_factor needs to be large enough to make the ellipses visible.
#' plot_indicatrices(
#'   distortion_sf = distortion_at_gcps,
#'   pai_model = gam_model,
#'   scale_factor = 20
#' )
#'
plot_indicatrices <- function(distortion_sf, pai_model, scale_factor = 1,
                              fill_color = "lightblue", border_color = "black") {

  # --- Input Validation ---
  required_cols <- c("a", "b", "theta_a")
  if (!inherits(distortion_sf, "sf")) stop("`distortion_sf` must be an sf object.", call. = FALSE)
  if (!all(required_cols %in% names(distortion_sf))) {
    stop("Input data must be the output of `analyze_distortion()`.", call. = FALSE)
  }
  if (!inherits(pai_model, "pai_model")) {
    stop("`pai_model` must be an object of class 'pai_model'.", call. = FALSE)
  }

  message("Generating indicatrix polygons...")
  # Predict target locations for ellipse centers
  source_coords_df <- as.data.frame(sf::st_coordinates(distortion_sf))
  names(source_coords_df) <- c("source_x", "source_y")
  displacements <- predict(pai_model, newdata = source_coords_df)
  target_coords <- source_coords_df + displacements
  names(target_coords) <- c("target_x", "target_y")

  indicatrices_list <- vector("list", nrow(distortion_sf))

  # --- Create Ellipse for each point ---
  for (i in 1:nrow(distortion_sf)) {
    point_data <- distortion_sf[i, ]
    center_coords <- as.numeric(target_coords[i, ])
    a <- point_data$a
    b <- point_data$b
    theta_rad <- point_data$theta_a * pi / 180

    # 1. Generate points for the open path of the ellipse
    t <- seq(0, 2 * pi, length.out = 51)
    ellipse_path <- cbind(a * cos(t), b * sin(t))

    # 2. CRITICAL FIX: Manually close the polygon by appending the first point
    # This guarantees the polygon is closed, regardless of floating-point issues.
    closed_ellipse_path <- rbind(ellipse_path, ellipse_path[1,])

    # 3. Create the 2D rotation matrix
    rotation_matrix <- matrix(c(cos(theta_rad), sin(theta_rad), -sin(theta_rad), cos(theta_rad)), 2, 2)

    # 4. Apply rotation, visibility scaling, and translation
    final_ellipse <- (closed_ellipse_path %*% rotation_matrix) * scale_factor
    final_ellipse[,1] <- final_ellipse[,1] + center_coords[1]
    final_ellipse[,2] <- final_ellipse[,2] + center_coords[2]

    indicatrices_list[[i]] <- sf::st_polygon(list(final_ellipse))
  }

  # --- Create the final sf object for plotting ---
  indicatrices_sf <- sf::st_sfc(indicatrices_list, crs = sf::st_crs(distortion_sf))

  # --- Create the plot ---
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = indicatrices_sf,
      fill = fill_color,
      color = border_color,
      linewidth = 0.2,
      alpha = 0.7
    ) +
    ggplot2::labs(
      title = "Tissot's Indicatrices of Distortion",
      x = "Target X",
      y = "Target Y"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::coord_sf(datum = sf::st_crs(distortion_sf))

  return(p)
}
