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
    if (!inherits(aoi, "sf") ||
        !any(sf::st_geometry_type(aoi) %in% c("POLYGON", "MULTIPOLYGON"))) {
      stop(
        "`aoi` must be a valid `sf` object with POLYGON or MULTIPOLYGON geometry.",
        call. = FALSE
      )
    }
    # Ensure AOI has the same CRS as the map
    if (sf::st_crs(aoi) != sf::st_crs(map)) {
      aoi <- sf::st_transform(aoi, sf::st_crs(map))
      message("Transformed `aoi` CRS to match `map` CRS.")
    }
  }

  message("Applying PAI model to map features...")

  # --- 2. Handle AOI ---
  if (!is.null(aoi)) {
    # Add a unique row ID to track original features for later dissolve
    map$.pai_original_row_id <- seq_len(nrow(map))
    original_feature_count <- nrow(map)

    # Separate the map into parts inside and outside the AOI
    map_inside_aoi <- suppressWarnings(sf::st_intersection(map, aoi))
    map_outside_aoi <- suppressWarnings(sf::st_difference(map, aoi))
  } else {
    # If no AOI, the whole map is "inside"
    map_inside_aoi <- map
    map_outside_aoi <- NULL
    original_feature_count <- NULL
  }

  # --- 3. Transform the part of the map inside the AOI ---
  if (nrow(map_inside_aoi) > 0) {
    original_geom_col <- sf::st_geometry(map_inside_aoi)

    # Identify which geometries are empty vs non-empty
    is_empty <- sf::st_is_empty(original_geom_col)
    non_empty_indices <- which(!is_empty)

    # Initialize output list (empty geometries will be kept as-is)
    new_geom_list <- as.list(original_geom_col)

    if (length(non_empty_indices) > 0) {
      # Extract only non-empty geometries for coordinate processing
      non_empty_geom <- original_geom_col[non_empty_indices]

      # Try to extract ALL coordinates from non-empty features at once
      # This fails for mixed geometry types (sfc_GEOMETRY), so we use tryCatch
      all_coords <- tryCatch(
        sf::st_coordinates(non_empty_geom),
        error = function(e) {
          # For mixed geometry types, extract coords per feature and combine
          # We need to normalize columns since different geometry types have different L columns
          coords_list <- lapply(seq_along(non_empty_geom), function(j) {
            feat_coords <- sf::st_coordinates(non_empty_geom[[j]])
            if (nrow(feat_coords) > 0) {
              # Keep only X and Y, add a consistent feature index column
              # This normalizes across different geometry types
              data.frame(
                X = feat_coords[, "X"],
                Y = feat_coords[, "Y"],
                Lfeat = j
              )
            } else {
              NULL
            }
          })
          # Remove NULL entries and combine
          coords_list <- coords_list[!sapply(coords_list, is.null)]
          if (length(coords_list) > 0) {
            as.matrix(do.call(rbind, coords_list))
          } else {
            matrix(ncol = 3, nrow = 0, dimnames = list(NULL, c("X", "Y", "Lfeat")))
          }
        }
      )

      if (nrow(all_coords) > 0) {
        # Single predict call for all coordinates (major performance gain)
        source_coords_df <- data.frame(
          source_x = all_coords[, "X"],
          source_y = all_coords[, "Y"]
        )
        displacements <- predict(pai_model, newdata = source_coords_df)

        # Apply displacements to all coords at once
        all_coords[, "X"] <- all_coords[, "X"] + displacements$dx
        all_coords[, "Y"] <- all_coords[, "Y"] + displacements$dy

        # Determine the feature index column name based on geometry type
        # For sfc, st_coordinates uses the highest L column as feature index
        # For mixed geometry fallback, we use "Lfeat"
        coord_colnames <- colnames(all_coords)
        if ("Lfeat" %in% coord_colnames) {
          # Mixed geometry fallback path
          feature_idx_col <- "Lfeat"
        } else {
          l_cols <- coord_colnames[grepl("^L[0-9]+$", coord_colnames)]
          feature_idx_col <- if (length(l_cols) > 0) max(l_cols) else NULL
        }

        # Rebuild geometries for non-empty features using pre-computed corrected coords
        for (j in seq_along(non_empty_indices)) {
          i <- non_empty_indices[j] # Original index in full geometry list
          feature <- original_geom_col[[i]]
          geom_type <- as.character(sf::st_geometry_type(feature))

          # Extract this feature's corrected coordinates
          # j is the index within non_empty_geom, which matches feature index in all_coords
          if (!is.null(feature_idx_col)) {
            feature_mask <- all_coords[, feature_idx_col] == j
            corrected_coords <- all_coords[feature_mask, , drop = FALSE]
          } else {
            # Single POINT case - no L columns
            corrected_coords <- all_coords[j, , drop = FALSE]
          }

          if (nrow(corrected_coords) == 0) {
            # Keep original if no coords found (shouldn't happen for non-empty)
            next
          }

          new_sfg <- switch(geom_type,
                            "POINT" = sf::st_point(corrected_coords[1, c("X", "Y")]),
                            "LINESTRING" = sf::st_linestring(corrected_coords[, c("X", "Y")]),
                            "POLYGON" = {
                              if ("L1" %in% colnames(corrected_coords)) {
                                # Standard path with L1 column
                                rings <- split.data.frame(corrected_coords[, c("X", "Y")], f = corrected_coords[, "L1"])
                                closed_rings <- lapply(rings, function(ring) {
                                  ring_mat <- as.matrix(ring)
                                  ring_mat[nrow(ring_mat), ] <- ring_mat[1, ]
                                  ring_mat
                                })
                                sf::st_polygon(closed_rings)
                              } else {
                                # Mixed geometry fallback: use original structure with new coords
                                orig_coords <- sf::st_coordinates(feature)
                                rings <- split.data.frame(
                                  data.frame(X = corrected_coords[, "X"], Y = corrected_coords[, "Y"]),
                                  f = orig_coords[, "L1"]
                                )
                                closed_rings <- lapply(rings, function(ring) {
                                  ring_mat <- as.matrix(ring)
                                  ring_mat[nrow(ring_mat), ] <- ring_mat[1, ]
                                  ring_mat
                                })
                                sf::st_polygon(closed_rings)
                              }
                            },
                            "MULTIPOINT" = sf::st_multipoint(corrected_coords[, c("X", "Y")]),
                            "MULTILINESTRING" = {
                              if ("L1" %in% colnames(corrected_coords)) {
                                lines <- split.data.frame(corrected_coords[, c("X", "Y")], f = corrected_coords[, "L1"])
                                sf::st_multilinestring(lapply(lines, as.matrix))
                              } else {
                                orig_coords <- sf::st_coordinates(feature)
                                lines <- split.data.frame(
                                  data.frame(X = corrected_coords[, "X"], Y = corrected_coords[, "Y"]),
                                  f = orig_coords[, "L1"]
                                )
                                sf::st_multilinestring(lapply(lines, as.matrix))
                              }
                            },
                            "MULTIPOLYGON" = {
                              if ("L2" %in% colnames(corrected_coords)) {
                                polys <- split.data.frame(corrected_coords, f = corrected_coords[, "L2"])
                                rebuilt_polys <- lapply(polys, function(p) {
                                  rings <- split.data.frame(p[, c("X", "Y")], f = p[, "L1"])
                                  closed_rings <- lapply(rings, function(ring) {
                                    ring_mat <- as.matrix(ring)
                                    ring_mat[nrow(ring_mat), ] <- ring_mat[1, ]
                                    ring_mat
                                  })
                                  sf::st_polygon(closed_rings)
                                })
                                sf::st_multipolygon(rebuilt_polys)
                              } else {
                                # Mixed geometry fallback: use original structure with new coords
                                orig_coords <- sf::st_coordinates(feature)
                                corrected_df <- data.frame(X = corrected_coords[, "X"], Y = corrected_coords[, "Y"])
                                polys <- split(seq_len(nrow(corrected_df)), f = orig_coords[, "L2"])
                                rebuilt_polys <- lapply(polys, function(idx) {
                                  p_orig <- orig_coords[idx, , drop = FALSE]
                                  p_new <- corrected_df[idx, , drop = FALSE]
                                  rings <- split(seq_len(nrow(p_new)), f = p_orig[, "L1"])
                                  closed_rings <- lapply(rings, function(ring_idx) {
                                    ring_mat <- as.matrix(p_new[ring_idx, ])
                                    ring_mat[nrow(ring_mat), ] <- ring_mat[1, ]
                                    ring_mat
                                  })
                                  sf::st_polygon(closed_rings)
                                })
                                sf::st_multipolygon(rebuilt_polys)
                              }
                            },
                            {
                              warning(paste("Unsupported geometry type:", geom_type, "at feature", i, ". Keeping original."), call. = FALSE)
                              feature
                            }
          )
          new_geom_list[[i]] <- new_sfg
        }
      }
    }

    new_sfc <- sf::st_sfc(new_geom_list, crs = sf::st_crs(map_inside_aoi))
    corrected_map_inside <- sf::st_set_geometry(map_inside_aoi, new_sfc)
  } else {
    corrected_map_inside <- NULL
  }

  # --- 4. Combine corrected inside part with original outside part ---
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

  # --- 5. Dissolve fragments back to original features when AOI was used ---
  if (!is.null(aoi) && ".pai_original_row_id" %in% names(corrected_map)) {
    if (nrow(corrected_map) > original_feature_count) {
      message("Dissolving AOI-split fragments back to original features...")

      # Group by original row ID and union geometries
      corrected_map <- corrected_map |>
        dplyr::group_by(.data$.pai_original_row_id) |>
        dplyr::summarise(
          dplyr::across(
            dplyr::where(~ !inherits(.x, "sfc")),
            ~ dplyr::first(.x)
          ),
          geometry = sf::st_union(geometry),
          .groups = "drop"
        )
    }

    # Remove the tracking column
    corrected_map$.pai_original_row_id <- NULL
  }


  # --- 6. Calculate new area for polygons ---
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
