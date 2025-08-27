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
#' @importFrom sf st_geometry st_geometry_type st_coordinates st_sfc
#' @importFrom stats predict
#' @export
#' @examples
#' # This example demonstrates a full workflow
#'
#' # Load Data and Train Model
#' demo_data <- create_demo_data()
#' map_to_correct <- demo_data$map
#' gam_model <- train_pai_model(demo_data$gcp, method = "gam_biv")
#'
#' # Apply the Model to the demo map ---
#' corrected_map <- transform_map(gam_model, demo_data$map)
#'
#' # Inspect results
#' library(ggplot2)
#'
#' # For easy plotting, add a 'status' column and combine the maps
#' map_to_correct$status <- "Original (Distorted)"
#' corrected_map$status <- "Corrected"
#' comparison_data <- rbind(map_to_correct[, "status"], corrected_map[, "status"])
#'
#' # Create the final comparison plot
#' ggplot(comparison_data) +
#' geom_sf(aes(color = status, linetype = status), fill = NA, linewidth = 0.7) +
#'   scale_color_manual(
#'     name = "Map Status",
#'     values = c("Original (Distorted)" = "grey50", "Corrected" = "#e41a1c")) +
#'   scale_linetype_manual(
#'     name = "Map Status",
#'     values = c("Original (Distorted)" = "dashed", "Corrected" = "solid")) +
#'   labs(
#'    title = "Positional Correction of a Distorted Grid",
#'     subtitle = "Overlay of original (dashed) and corrected (solid) geometries") +
#'   theme_minimal()
transform_map <- function(pai_model, map, aoi = NULL) {

  # --- 1. Input Validation ---
  validate_map_transform(pai_model, map, aoi)

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
    original_geom <- sf::st_geometry(map_inside_aoi)
    geom_types <- sf::st_geometry_type(original_geom)
    n_features <- length(original_geom)
    coords_list <- vector("list", n_features)
    n_points <- integer(n_features)

    for (i in seq_len(n_features)) {
      crd <- sf::st_coordinates(original_geom[[i]])
      coords_list[[i]] <- crd
      n_points[i] <- nrow(crd)
    }

    total_points <- sum(n_points)
    new_geom_list <- vector("list", n_features)

    if (total_points > 0) {
      all_source_df <- data.frame(
        source_x = do.call(c, lapply(coords_list, function(crd) if (nrow(crd) > 0) crd[, 1] else numeric(0))),
        source_y = do.call(c, lapply(coords_list, function(crd) if (nrow(crd) > 0) crd[, 2] else numeric(0)))
      )

      displacements <- stats::predict(pai_model, all_source_df)

      # Apply displacements back to coords_list
      idx <- 1
      for (i in seq_len(n_features)) {
        if (n_points[i] == 0) {
          new_geom_list[[i]] <- original_geom[[i]]
          next
        }
        end <- idx + n_points[i] - 1
        coords_list[[i]][, 1] <- coords_list[[i]][, 1] + displacements$dx[idx:end]
        coords_list[[i]][, 2] <- coords_list[[i]][, 2] + displacements$dy[idx:end]
        idx <- end + 1

        # Rebuild geometry
        corrected_coords <- coords_list[[i]]
        geom_type <- as.character(geom_types[i])

        new_sfg <- switch(
          geom_type,
          "POINT" = sf::st_point(corrected_coords[1, 1:2]),
          "LINESTRING" = sf::st_linestring(corrected_coords[, 1:2]),
          "POLYGON" = {
            rings <- split.data.frame(corrected_coords[, 1:2], f = corrected_coords[, "L1"])
            closed_rings <- lapply(rings, function(ring) {
              ring_mat <- as.matrix(ring)
              # Since sf coordinates are already closed, overwrite is redundant but harmless
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
                # Since sf coordinates are already closed, overwrite is redundant but harmless
                ring_mat[nrow(ring_mat), ] <- ring_mat[1, ]
                ring_mat
              })
              sf::st_polygon(closed_rings)
            })
            sf::st_multipolygon(rebuilt_polys)
          },
          {
            warning(paste("Unsupported geometry type:", geom_type, "at feature", i, ". Keeping original."), call. = FALSE)
            original_geom[[i]]
          }
        )
        new_geom_list[[i]] <- new_sfg
      }
    } else {
      # If no points, keep all original
      new_geom_list <- as.list(original_geom)
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
  if (any(grepl("POLYGON", sf::st_geometry_type(corrected_map)))) {
    message("Calculating area of corrected polygons...")
    # Check if the area column from the original map exists, if so, remove it
    # to avoid conflicts
    if ("area_new" %in% names(corrected_map)) {
      corrected_map$area_new <- NULL
    }
    corrected_map$area_new <- sf::st_area(corrected_map)
  }

  message("Correction complete.")
  return(corrected_map)
}
