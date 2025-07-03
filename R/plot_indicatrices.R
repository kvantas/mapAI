#' @title Plot Tissot's Indicatrices of Distortion
#' @description Visualizes distortion by drawing Tissot's indicatrices
#'   (ellipses) at their original source locations.
#' @details This function creates a powerful visual representation of
#' distortion, following the methodology of Boùùaert et al. (2016). It takes the
#' results from `analyze_distortion()` and draws an ellipse at each analyzed
#' point, centered on its **source coordinate**. This allows for a direct visual
#' assessment of distortion on the historical map's geographic space.
#'
#' Each ellipse graphically shows the magnitude and direction of distortion at
#' that location:
#' \itemize{
#'   \item The **shape** of the ellipse shows the angular distortion (shear).
#'   \item The **size** of the ellipse shows the areal distortion.
#'   \item The **orientation** of the ellipse shows the direction of maximum
#'   scale.
#' }
#'
#' @param distortion_sf An `sf` object of **points** returned by
#'   `analyze_distortion()`. It must contain the columns `a`, `b`, and
#'   `theta_a`.
#' @param scale_factor A numeric value to control the size of the plotted
#'   ellipses for better visibility. You will need to adjust this based on your
#'   map's scale.
#' @param fill_color A character string specifying the fill color of the
#'   ellipses.
#' @param border_color A character string specifying the border color of the
#'   ellipses.
#'
#' @return A `ggplot` object containing the distortion ellipses plotted in the
#'   source coordinate space.
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
#' # Note that the pai_model is no longer needed. The function plots the
#' # distortion centered on the source locations from the distortion_sf object.
#' # The scale_factor needs to be large enough to make the ellipses visible.
#' plot_indicatrices(
#'   distortion_sf = distortion_at_gcps,
#'   scale_factor = 20
#' )
#'
plot_indicatrices <- function(distortion_sf, scale_factor = 1,
                              fill_color = "lightblue", border_color = "black") {

  # --- Input Validation ---
  required_cols <- c("a", "b", "theta_a")

  if (!inherits(distortion_sf, "sf")) {
    stop("`distortion_sf` must be an sf object.", call. = FALSE)
    }

  if (!all(required_cols %in% names(distortion_sf))) {
    stop("Input data must be the output of `analyze_distortion()`.", call. = FALSE)
  }

  message("Generating indicatrix polygons at source locations...")

  # Get the source coordinates which will be the ellipse centers
  source_coords <- sf::st_coordinates(distortion_sf)

  indicatrices_list <- vector("list", nrow(distortion_sf))

  # --- Create Ellipse for each point ---
  for (i in seq_along(nrow(distortion_sf))) {
    point_data <- distortion_sf[i, ]
    # The center of the ellipse is its original SOURCE location
    center_coords <- source_coords[i, ]

    a <- point_data$a
    b <- point_data$b
    theta_rad <- point_data$theta_a * pi / 180

    t <- seq(0, 2 * pi, length.out = 51)
    base_ellipse <- cbind(a * cos(t), b * sin(t))
    closed_ellipse <- rbind(base_ellipse, base_ellipse[1,])

    rotation_matrix <- matrix(c(cos(theta_rad),
                                sin(theta_rad),
                                -sin(theta_rad),
                                cos(theta_rad)),
                              2, 2)

    # Apply rotation, visibility scaling, and translation to the source location
    final_ellipse <- (closed_ellipse %*% rotation_matrix) * scale_factor
    final_ellipse[,1] <- final_ellipse[,1] + center_coords[1]
    final_ellipse[,2] <- final_ellipse[,2] + center_coords[2]

    indicatrices_list[[i]] <- sf::st_polygon(list(final_ellipse))
  }

  # --- Create the final sf object for plotting ---
  indicatrices_sf <- sf::st_sfc(indicatrices_list,
                                crs = sf::st_crs(distortion_sf))

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
      subtitle = "Ellipses are centered on source map coordinates",
      x = "Source X",
      y = "Source Y"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::coord_sf(datum = sf::st_crs(distortion_sf))

  return(p)
}
