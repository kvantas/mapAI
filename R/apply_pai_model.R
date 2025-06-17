#' @title Apply a Trained PAI Model to Correct a Vector Map
#' @description Applies a trained `pai_model` object to an `sf` vector map,
#'   correcting the position of all its vertices based on the learned transformation.
#' @details This function is the final step in the PAI workflow. It iterates through
#'   each feature of the input `map`, uses a recursive method to handle arbitrarily
#'   complex geometries (e.g., MULTIPOLYGON with holes), predicts the necessary `dx`
#'   and `dy` correction for each vertex, and then reconstructs the map's geometry
#'   with the corrected coordinates.
#'
#' @param pai_model An object of class `pai_model` returned by `train_pai_model()`.
#' @param map An `sf` object representing the vector map to be corrected.
#'
#' @return A new `sf` object with the corrected geometry. Original attributes are
#'   preserved, and an `area_new` column is added for polygon features.
#'
#' @import sf
#' @import dplyr
#' @importFrom units set_units
#' @importFrom rlang .data
#' @export
apply_pai_model <- function(pai_model, map) {
  # --- 1. Input Validation ---
  if (!inherits(pai_model, "pai_model")) {
    stop("`pai_model` must be an object of class 'pai_model', created by `train_pai_model()`.", call. = FALSE)
  }
  if (!inherits(map, "sf")) {
    stop("`map` must be a valid `sf` object.", call. = FALSE)
  }

  message("Applying PAI model: correcting map features...")

  # --- 2. Setup Helper and Recursive Functions ---
  transform_coords <- function(coord_matrix) {
    if (is.null(coord_matrix) || nrow(coord_matrix) == 0) return(coord_matrix)

    # THE DEFINITIVE FIX: Construct the data frame with named columns directly.
    # This is robust and avoids the as.data.frame() and names() fragility.
    data_for_prediction <- data.frame(
      source_x = coord_matrix[, 1],
      source_y = coord_matrix[, 2]
    )

    predictions <- predict(pai_model, newdata = data_for_prediction)

    coord_matrix[, 1] <- coord_matrix[, 1] + predictions$dx
    coord_matrix[, 2] <- coord_matrix[, 2] + predictions$dy
    return(coord_matrix)
  }

  recursive_transform <- function(g_part) {
    # If the part is a simple numeric vector (i.e., a POINT), convert it to a 1-row matrix first.
    if (is.vector(g_part) && is.numeric(g_part)) {
      g_part <- matrix(g_part, nrow = 1)
    }

    if (is.matrix(g_part)) {
      # Base Case: This is a coordinate matrix (from a POINT, LINESTRING, or POLYGON ring)
      return(transform_coords(g_part))
    } else if (is.list(g_part)) {
      # Recursive Step: This is a list of parts (e.g., a POLYGON or MULTI* geometry)
      return(lapply(g_part, recursive_transform))
    }

    # Fallback for any other type
    return(g_part)
  }

  # --- 3. Iterate, Transform, and Rebuild Geometries ---
  original_geom_col <- sf::st_geometry(map)
  new_geom_list <- vector("list", length = length(original_geom_col))

  for (i in seq_along(original_geom_col)) {
    feature <- original_geom_col[[i]]
    corrected_coords <- recursive_transform(feature)

    geom_type <- sf::st_geometry_type(feature)

    new_sfg <- switch(as.character(geom_type),
                      "POINT"           = sf::st_point(corrected_coords),
                      "LINESTRING"      = sf::st_linestring(corrected_coords),
                      "POLYGON"         = sf::st_polygon(corrected_coords),
                      "MULTIPOINT"      = sf::st_multipoint(corrected_coords),
                      "MULTILINESTRING" = sf::st_multilinestring(corrected_coords),
                      "MULTIPOLYGON"    = sf::st_multipolygon(corrected_coords),
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
