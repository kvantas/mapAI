#' @title Read and Prepare Homologous Points (GCPs) to an `gcp` Object
#' @description Imports data regarding homologous points and prepares them for
#'   modeling by calculating displacement vectors and creating an `gcp` object.
#' @details This function is the first step in the modeling workflow. It
#'   calculates the `dx` and `dy` errors that the models will learn to predict.
#'
#' @param source_x Numeric vector of approximate ('from') x coordinates.
#' @param source_y Numeric vector of approximate ('from') y coordinates.
#' @param target_x Numeric vector of actual ('to') x coordinates.
#' @param target_y Numeric vector of actual ('to') y coordinates.
#' @param crs Optional Coordinate Reference System (CRS) for the control points,
#'   acceptable by \code{sf::st_crs()}. Defaults to \code{NULL}.
#'
#' @return An `gcp` object of the homologous points and calculated `dx` and `dy`
#'   displacement columns.
#'
#' @importFrom utils read.csv
#' @importFrom rlang .data
#' @importFrom sf st_crs st_as_sf
#' @export
#' @examples
#' # Sample data
#' source_coords <- data.frame(source_x = c(10, 20, 20, 30, 15),
#'                             source_y = c(15, 25, 15, 10, 5))
#' target_coords <- data.frame(target_x = c(12.5, 19.8, 19.6, 32.2, 14.0),
#'                             target_y = c(14.5, 27.2, 15.9, 9.8, 4.9))
#' # Read and prepare GCPs
#' gcp_data <- read_gcp(
#'   source_x = source_coords$source_x,
#'   source_y = source_coords$source_y,
#'   target_x = target_coords$target_x,
#'   target_y = target_coords$target_y
#' )
#' print(gcp_data)
#' summary(gcp_data)
#' plot(gcp_data)
read_gcp <- function(source_x, source_y, target_x, target_y, crs = NULL) {

  # ---  Input Validation ---
  input_validation(source_x, source_y, target_x, target_y)

  # --- Create Data Frame ---
  gcp_df <- data.frame(
    source_x = source_x,
    source_y = source_y,
    target_x = target_x,
    target_y = target_y
  )
  # add displacement columns
  gcp_df$dx <- gcp_df$target_x - gcp_df$source_x
  gcp_df$dy <- gcp_df$target_y - gcp_df$source_y

  if (!is.null(crs)) {
    attr(gcp_df, "crs") <- sf::st_crs(crs)
  }

  # set the class to gcp
  class(gcp_df) <- c("gcp", "data.frame")

  return(gcp_df)
}

#' Print Method for gcp Objects
#' @param x An object of class `gcp`.
#' @param ... Additional arguments (not used).
#' @export
print.gcp <- function(x, ...) {
  cat("GCP Object with", nrow(x), "points\n")
  if (!is.null(attr(x, "crs")) && !is.na(attr(x, "crs"))) {
    crs_info <- attr(x, "crs")$input
    if (is.null(crs_info)) crs_info <- attr(x, "crs")$wkt
    cat("CRS:", crs_info, "\n")
  }
  n <- nrow(x)
  if (n > 10) {
    cat("Displaying first 10 points:\n")
    x <- x[1:10, ]
  }
  print.data.frame(x)
  invisible(x)
}

#' Summary Method for gcp Objects
#' @param object An object of class `gcp`.
#' @param ... Additional arguments (not used).
#' @details The summary method provides a concise overview of the GCP object,
#' @export
summary.gcp <- function(object, ...) {
  cat("Summary of GCP Object:\n")
  cat("Number of points:", nrow(object), "\n")
  cat("Source Coordinates Range:\n")
  cat("  X:", range(object$source_x, na.rm = TRUE), "\n")
  cat("  Y:", range(object$source_y, na.rm = TRUE), "\n")
  cat("Target Coordinates Range:\n")
  cat("  X:", range(object$target_x, na.rm = TRUE), "\n")
  cat("  Y:", range(object$target_y, na.rm = TRUE), "\n")
  cat("Displacement Vectors Range:\n")
  cat("  dx:", range(object$dx, na.rm = TRUE), "\n")
  cat("  dy:", range(object$dy, na.rm = TRUE), "\n")
  cat("Mean Displacement:\n")
  cat("  Mean dx:", mean(object$dx, na.rm = TRUE), "\n")
  cat("  Mean dy:", mean(object$dy, na.rm = TRUE), "\n")
  cat("Standard Deviation of Displacement:\n")
  cat("  SD dx:", sd(object$dx, na.rm = TRUE), "\n")
  cat("  SD dy:", sd(object$dy, na.rm = TRUE), "\n")
  cat("2D RMSE of Displacement:\n")
  rmse_2d <- sqrt(mean(object$dx^2 + object$dy^2, na.rm = TRUE))
  cat("  RMSE:", rmse_2d, "\n")
  invisible(object)
}


#' Plot Method for gcp Objects
#' @description Creates a visualization of the displacement vectors from source
#'   (distorted) to target (true) coordinates for a set of homologous points.
#' @details This function is a key exploratory tool for understanding the nature
#'   and magnitude of positional error in a dataset before correction. It plots
#'   arrows that originate at the distorted `source` coordinates and point to
#'   the correct `target` coordinates. This provides an immediate visual sense
#'   of the spatial patterns in the distortion (e.g., rotation, scaling, or
#'   non-linear warping).
#' @param x An object of class `gcp`.
#' @param title A character string for the plot's main title.
#' @param subtitle A character string for the plot's subtitle.
#' @param arrow_color A character string specifying the color of the
#'   displacement arrows.
#' @param point_color A character string specifying the color of the points
#'   marking the source locations.
#' @param exaggeration_factor A numeric value to scale the length of the
#'   displacement vectors. A value of 2, for instance, will double their
#'   plotted length, making subtle displacements more visible. Defaults to 1
#'   (no exaggeration).
#' @param ... Additional arguments (not used).
#'
#' @return A `ggplot` object, which can be further customized using standard
#'   `ggplot2` syntax.
#' @import ggplot2
#' @importFrom rlang .data
#' @importFrom grid arrow unit
#' @export
#' @examples
#' # Sample data
#' source_coords <- data.frame(source_x = c(10, 20, 20, 30, 15),
#'                             source_y = c(15, 25, 15, 10, 5))
#' target_coords <- data.frame(target_x = c(12.5, 19.8, 19.6, 32.2, 14.0),
#'                             target_y = c(14.5, 27.2, 15.9, 9.8, 4.9))
#' # Read and prepare GCPs
#' gcp_data <- read_gcp(
#'   source_x = source_coords$source_x,
#'   source_y = source_coords$source_y,
#'   target_x = target_coords$target_x,
#'   target_y = target_coords$target_y
#' )
#'
#' # Create the default displacement plot
#' plot(gcp_data)
#'
#' # Customize the plot with different titles and colors
#' plot(
#'   gcp_data,
#'   title = "My Custom Displacement Plot",
#'   subtitle = "Visualizing error vectors",
#'   arrow_color = "blue",
#'   point_color = "orange"
#' )
#'
#' # Exaggerate the displacement vectors for clearer visualization
#' plot(gcp_data, exaggeration_factor = 5)
plot.gcp <- function(x,
                     title = "Distortion Displacement Vectors",
                     subtitle = "Arrows point from distorted to true locations",
                     arrow_color = "darkred",
                     point_color = "red",
                     exaggeration_factor = 1,
                     ...) {

  plt <- ggplot2::ggplot(x) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = .data$source_x,
        y = .data$source_y,
        xend = .data$source_x +
          exaggeration_factor * (.data$target_x - .data$source_x),
        yend = .data$source_y +
          exaggeration_factor * (.data$target_y - .data$source_y)
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

  return(plt)
}

#' Convert a GCP Object to an sf Spatial Point Object
#'
#' Converts a `gcp` object containing homologous control points into an `sf`
#' spatial point collection using either source or target coordinates.
#'
#' @param x An object of class `gcp`.
#' @param coords A character string specifying which coordinates to use for
#'   the spatial geometry: `"source"` (default) or `"target"`.
#' @param crs Optional Coordinate Reference System. If `NULL` (default), uses
#'   the `crs` attribute of the `gcp` object if present.
#' @param ... Additional arguments passed to `sf::st_as_sf()`.
#'
#' @return An `sf` spatial point object containing the GCP coordinates and displacements.
#' @exportS3Method sf::st_as_sf
#' @examples
#' gcp_data <- read_gcp(
#'   source_x = c(10, 20, 30), source_y = c(15, 25, 35),
#'   target_x = c(12, 22, 32), target_y = c(17, 27, 37),
#'   crs = 3857
#' )
#' sf_pts <- sf::st_as_sf(gcp_data, coords = "source")
#' print(sf_pts)
st_as_sf.gcp <- function(x, coords = c("source", "target"), crs = NULL, ...) {
  coords <- match.arg(coords)
  coord_cols <- if (coords == "source") c("source_x", "source_y") else c("target_x", "target_y")
  if (is.null(crs)) {
    crs <- attr(x, "crs")
  }
  sf::st_as_sf(as.data.frame(x), coords = coord_cols, crs = crs, ...)
}

#' Get Coordinate Reference System of a GCP Object
#'
#' @param x An object of class `gcp`.
#' @param ... Additional arguments (not used).
#'
#' @return A `crs` object or `NA`.
#' @exportS3Method sf::st_crs
st_crs.gcp <- function(x, ...) {
  crs_attr <- attr(x, "crs")
  if (is.null(crs_attr)) {
    sf::st_crs(NA)
  } else {
    crs_attr
  }
}


