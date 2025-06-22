#' @title Apply a Trained PAI Model to Correct a Vector Map
#' @description Applies a trained `pai_model` object to an `sf` vector map,
#'   correcting the position of all its vertices based on the learned transformation.
#'
#' @details
#' This function is the final step in the PAI workflow, applying the learned
#' spatial correction to a target map. It uses a robust feature-by-feature
#' iteration that correctly handles all standard simple and multi-part geometry
#' types (`POINT`, `LINESTRING`, `POLYGON`, etc.).
#'
#' @param pai_model An object of class `pai_model` returned by `train_pai_model()`.
#' @param map An `sf` object representing the vector map to be corrected.
#'
#' @return A new `sf` object with the corrected geometry.
#'
#' @import sf
#' @import dplyr
#' @importFrom stats predict
#' @export
#' @examples
#' # This example demonstrates a full workflow with POINT geometry.
#'
#' # --- 1. Load Data and Train Model ---
#' data(gcps) # gcps is an sf object with POINT geometry
#' gam_model <- train_pai_model(gcps, method = "gam")
#'
#' # --- 2. Apply the Model to Correct the Points ---
#' corrected_points <- apply_pai_model(gam_model, gcps)
#'
#' # --- 3. Inspect and Visualize ---
#' # The coordinates of the corrected points should be different.
#' head(sf::st_coordinates(gcps))
#' head(sf::st_coordinates(corrected_points))
#'
#' # Visually confirm the points have moved
#' plot(sf::st_geometry(gcps), col = 'grey', pch = 4, cex=0.5,
#'      main = "Original (Grey) vs. Corrected (Red) Points")
#' plot(sf::st_geometry(corrected_points), col = 'red', pch = 16, cex=0.5, add = TRUE)
#'
apply_pai_model <- function(pai_model, map) {
  # --- 1. Input Validation ---
  if (!inherits(pai_model, "pai_model")) stop("`pai_model` must be an object of class 'pai_model'.", call. = FALSE)
  if (!inherits(map, "sf")) stop("`map` must be a valid `sf` object.", call. = FALSE)

  message("Applying PAI model to map features...")

  original_geom_col <- sf::st_geometry(map)
  new_geom_list <- vector("list", length = length(original_geom_col))

  # --- 2. Iterate through each feature one-by-one ---
  for (i in seq_along(original_geom_col)) {
    feature <- original_geom_col[[i]]
    geom_type <- as.character(sf::st_geometry_type(feature))

    # Use st_coordinates, which reliably extracts vertices into a standard matrix format
    original_coords <- sf::st_coordinates(feature)

    # Handle empty geometries gracefully
    if (nrow(original_coords) == 0) {
      new_geom_list[[i]] <- feature
      next
    }

    # Prepare data for prediction from the standardized matrix
    source_coords_df <- as.data.frame(original_coords[, 1:2, drop = FALSE])
    names(source_coords_df) <- c("source_x", "source_y")

    # Get displacements and calculate new coordinates
    displacements <- predict(pai_model, newdata = source_coords_df)
    corrected_coords <- original_coords
    corrected_coords[, 1] <- corrected_coords[, 1] + displacements$dx
    corrected_coords[, 2] <- corrected_coords[, 2] + displacements$dy

    # --- 3. Reconstruct the geometry based on its original type ---
    new_sfg <- switch(geom_type,
                      "POINT" = sf::st_point(corrected_coords[1, 1:2]),
                      "LINESTRING" = sf::st_linestring(corrected_coords[, 1:2]),
                      "POLYGON" = {
                        rings <- split.data.frame(corrected_coords[, 1:2], f = corrected_coords[, "L1"])
                        sf::st_polygon(lapply(rings, as.matrix))
                      },
                      "MULTIPOINT" = sf::st_multipoint(corrected_coords[, 1:2]),
                      "MULTILINESTRING" = {
                        lines <- split.data.frame(corrected_coords[, 1:2], f = corrected_coords[, "L1"])
                        sf::st_multilinestring(lapply(lines, as.matrix))
                      },
                      "MULTIPOLYGON" = {
                        polys <- split.data.frame(corrected_coords, f = corrected_coords[, "L2"])
                        rebuilt_polys <- lapply(polys, function(p) {
                          rings <- split.data.frame(p[, 1:2], f = p[, "L1"])
                          sf::st_polygon(lapply(rings, as.matrix))
                        })
                        sf::st_multipolygon(rebuilt_polys)
                      },
                      {
                        warning(paste("Unsupported geometry type:", geom_type, "at feature", i, ". Keeping original."), call. = FALSE)
                        feature
                      }
    )
    new_geom_list[[i]] <- new_sfg
  }

  # --- 4. Update SF Object Geometry ---
  new_sfc <- sf::st_sfc(new_geom_list, crs = sf::st_crs(map))
  corrected_map <- sf::st_set_geometry(map, new_sfc)

  # --- 5. Calculate new area for polygons ---
  if (any(grepl("POLYGON", sf::st_geometry_type(map)))) {
    message("Calculating area of corrected polygons...")
    corrected_map$area_new <- sf::st_area(corrected_map)
  }

  message("Correction complete.")
  return(corrected_map)
}
