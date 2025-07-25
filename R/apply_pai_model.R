#' @title Apply a Trained PAI Model to Correct a Vector Map
#' @description Applies a trained `pai_model` object to an `sf` vector map,
#'   correcting the position of all its vertices based on the learned
#'   transformation.
#'
#' @details
#' This function is the final step in the PAI workflow, applying the learned
#' spatial correction to a target map. It uses a robust feature-by-feature
#' iteration that correctly handles all standard simple and multi-part geometry
#' types (`POINT`, `LINESTRING`, `POLYGON`, etc.).
#'
#' If an Area of Interest (`aoi`) is provided, the transformation is only
#' applied to the parts of the map features that fall within the `aoi` polygon.
#' The features outside the `aoi` are kept in their original, untransformed
#' state.
#'
#' @param pai_model An object of class `pai_model` returned by
#'        `train_pai_model()`.
#' @param map An `sf` object representing the vector map to be corrected.
#' @param aoi An optional `sf` polygon object representing the Area of Interest.
#'        If provided, the transformation is only applied within this area.
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
#' gam_model <- train_pai_model(gcps, pai_method = "gam")
#'
#' # --- 2. Apply the Model to Correct the Points ---
#' corrected_points <- apply_pai_model(gam_model, gcps)
#'
#' # --- 3. Inspect  ---
#' # The coordinates of the corrected points should be different.
#' head(sf::st_coordinates(gcps))
#' head(sf::st_coordinates(corrected_points))
#'
apply_pai_model <- function(pai_model, map, aoi = NULL) {
  # --- 1. Input Validation ---
  if (!inherits(pai_model, "pai_model")) {
    stop("`pai_model` must be an object of class 'pai_model'.", call. = FALSE)
  }

  if (!inherits(map, "sf")) {
    stop("`map` must be a valid `sf` object.", call. = FALSE)
  }

  if (!is.null(aoi)) {
    if (!inherits(aoi, "sf") || !any(sf::st_geometry_type(aoi) %in% c("POLYGON", "MULTIPOLYGON"))) {
      stop("`aoi` must be a valid `sf` object with POLYGON or MULTIPOLYGON geometry.", call. = FALSE)
    }
    # Ensure AOI has the same CRS as the map
    if (sf::st_crs(aoi) != sf::st_crs(map)) {
      aoi <- sf::st_transform(aoi, sf::st_crs(map))
      message("Transformed `aoi` CRS to match `map` CRS.")
    }
  }

  message("Applying PAI model to map features...")

  # --- Handle AOI ---
  if (!is.null(aoi)) {
    # Separate the map into parts inside and outside the AOI
    map_inside_aoi <- suppressWarnings(sf::st_intersection(map, aoi))
    map_outside_aoi <- suppressWarnings(sf::st_difference(map, aoi))
  } else {
    # If no AOI, the whole map is "inside"
    map_inside_aoi <- map
    map_outside_aoi <- NULL
  }

  # --- Transform the part of the map inside the AOI ---
  if (nrow(map_inside_aoi) > 0) {
    original_geom_col <- sf::st_geometry(map_inside_aoi)
    new_geom_list <- vector("list", length = length(original_geom_col))

    for (i in seq_along(original_geom_col)) {
      feature <- original_geom_col[[i]]
      geom_type <- as.character(sf::st_geometry_type(feature))
      original_coords <- sf::st_coordinates(feature)

      if (nrow(original_coords) == 0) {
        new_geom_list[[i]] <- feature
        next
      }

      source_coords_df <- as.data.frame(original_coords[, 1:2, drop = FALSE])
      names(source_coords_df) <- c("source_x", "source_y")

      displacements <- predict(pai_model, newdata = source_coords_df)
      corrected_coords <- original_coords
      corrected_coords[, 1] <- corrected_coords[, 1] + displacements$dx
      corrected_coords[, 2] <- corrected_coords[, 2] + displacements$dy

      new_sfg <- switch(
        geom_type,
        "POINT" = sf::st_point(corrected_coords[1, 1:2]),
        "LINESTRING" = sf::st_linestring(corrected_coords[, 1:2]),
        "POLYGON" = {
          rings <- split.data.frame(corrected_coords[, 1:2], f = corrected_coords[, "L1"])
          closed_rings <- lapply(rings, function(ring) {
            ring_mat <- as.matrix(ring)
            ring_mat[nrow(ring_mat), ] <- ring_mat[1, ]
            ring_mat
          })
          sf::st_polygon(closed_rings)
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
            closed_rings <- lapply(rings, function(ring) {
              ring_mat <- as.matrix(ring)
              ring_mat[nrow(ring_mat), ] <- ring_mat[1, ]
              ring_mat
            })
            sf::st_polygon(closed_rings)
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

    new_sfc <- sf::st_sfc(new_geom_list, crs = sf::st_crs(map_inside_aoi))
    corrected_map_inside <- sf::st_set_geometry(map_inside_aoi, new_sfc)
  } else {
    corrected_map_inside <- NULL
  }

  # --- Combine corrected inside part with original outside part ---
  if (!is.null(corrected_map_inside) && !is.null(map_outside_aoi) && nrow(map_outside_aoi) > 0) {
    # Ensure columns match before binding
    common_cols <- intersect(names(corrected_map_inside), names(map_outside_aoi))
    corrected_map <- rbind(corrected_map_inside[, common_cols], map_outside_aoi[, common_cols])
  } else if (!is.null(corrected_map_inside)) {
    corrected_map <- corrected_map_inside
  } else if (!is.null(map_outside_aoi) && nrow(map_outside_aoi) > 0) {
    corrected_map <- map_outside_aoi
  } else {
    # This case handles when the original map was empty or fully outside AOI
    corrected_map <- map
  }


  # --- 5. Calculate new area for polygons ---
  if (any(grepl("POLYGON", sf::st_geometry_type(map)))) {
    message("Calculating area of corrected polygons...")
    # Check if the area column from the original map exists, if so, remove it to avoid conflicts
    if ("area_new" %in% names(corrected_map)) {
      corrected_map$area_new <- NULL
    }
    corrected_map$area_new <- sf::st_area(corrected_map)
  }

  message("Correction complete.")
  return(corrected_map)
}
