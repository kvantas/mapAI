#' @title Apply a Trained PAI Model to Correct a Vector Map (Optimized)
#' @description Applies a trained `pai_model` object to an `sf` vector map,
#'   correcting the position of all its vertices based on the learned
#'   transformation.
#'
#' @param pai_model An object of class `pai_model` returned by
#' `train_pai_model()`.
#' @param map An `sf` object representing the vector map to be corrected.
#' @param aoi An optional `sf` polygon object representing the Area of Interest.
#'
#' @return A new `sf` object with the corrected geometry.
#'
#' @importFrom sf st_geometry st_geometry_type st_coordinates st_sfc
#' st_set_geometry st_intersection st_difference st_bbox st_as_sf st_area
#' st_crs st_point st_linestring st_polygon st_multipoint st_multilinestring
#' st_multipolygon
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
# ggplot(comparison_data) +
# geom_sf(aes(color = status, linetype = status), fill = NA, linewidth = 0.7) +
#   scale_color_manual(
#     name = "Map Status",
#     values = c("Original (Distorted)" = "grey50", "Corrected" = "#e41a1c")) +
#   scale_linetype_manual(
#     name = "Map Status",
#     values = c("Original (Distorted)" = "dashed", "Corrected" = "solid")) +
#   labs(
#    title = "Positional Correction of a Distorted Grid",
#     subtitle = "Overlay of original (dashed) and corrected (solid) geometries") +
#   theme_minimal()
transform_map <- function(pai_model, map, aoi = NULL) {

  # --- 1. Input Validation ---
  validate_map_transform(pai_model, map, aoi)

  message("Applying PAI model to map features...")

  # --- 2. Handle AOI (Splitting Map) ---
  if (!is.null(aoi)) {
    map_bbox <- sf::st_bbox(map)
    aoi_bbox <- sf::st_bbox(aoi)

    # Fast pre-check for disjoint bounding boxes
    if (aoi_bbox["xmin"] > map_bbox["xmax"] ||
        aoi_bbox["xmax"] < map_bbox["xmin"] ||
        aoi_bbox["ymin"] > map_bbox["ymax"] ||
        aoi_bbox["ymax"] < map_bbox["ymin"]) {
      map_inside_aoi <- map[0, ] # Empty slice
      map_outside_aoi <- map
    } else {
      map_inside_aoi <- suppressWarnings(sf::st_intersection(map, aoi))
      map_outside_aoi <- suppressWarnings(sf::st_difference(map, aoi))
    }
  } else {
    map_inside_aoi <- map
    map_outside_aoi <- NULL
  }

  # --- 3. Transform features inside AOI ---
  if (nrow(map_inside_aoi) > 0) {

    geom_col <- sf::st_geometry(map_inside_aoi)
    geom_types <- as.character(sf::st_geometry_type(geom_col))
    n_features <- length(geom_col)

    # Extract coordinates as a list of matrices
    # Note: st_coordinates returns X,Y for points; X,Y,L1,L2 for polygons, etc.
    coords_list <- lapply(geom_col, sf::st_coordinates)

    # Identify non-empty geometries (rows > 0)
    # Empty geometries produce 0-row matrices
    is_valid <- vapply(coords_list, function(x) nrow(x) > 0, logical(1))

    # Only process valid geometries
    valid_coords_list <- coords_list[is_valid]
    n_points_per_valid <- vapply(valid_coords_list, nrow, integer(1))

    # Prepare the final list with original geometries
    new_geom_list <- as.list(geom_col)

    if (length(valid_coords_list) > 0) {

      # --- ROBUSTNESS FIX: Handle Mixed Column Counts ---
      # Points have 2 cols (X,Y), Polygons have 4 (X,Y,L1,L2).
      # rbind fails if cols mismatch. We pad smaller matrices with NA to
      # allow batching.
      ncols <- vapply(valid_coords_list, ncol, integer(1))
      max_cols <- max(ncols)

      if (any(ncols < max_cols)) {
        valid_coords_list <- lapply(valid_coords_list, function(m) {
          if (ncol(m) < max_cols) {
            # Pad with NA columns
            cbind(m, matrix(NA_real_,
                            nrow = nrow(m),
                            ncol = max_cols - ncol(m)))
          } else {
            m
          }
        })
      }

      # Bind to one big matrix for vectorized prediction
      all_coords_mat <- do.call(rbind, valid_coords_list)

      # Predict displacements (using only X and Y columns)
      pred_df <- data.frame(
        source_x = all_coords_mat[, 1],
        source_y = all_coords_mat[, 2]
      )

      displacements <- stats::predict(pai_model, pred_df)

      # Apply displacements
      all_coords_mat[, 1] <- all_coords_mat[, 1] + displacements$dx
      all_coords_mat[, 2] <- all_coords_mat[, 2] + displacements$dy

      # Split back into list based on the valid features
      # We create a splitting factor
      split_factor <- rep(seq_along(valid_coords_list),
                          times = n_points_per_valid)
      corrected_valid_list <- unname(split.data.frame(all_coords_mat,
                                                      split_factor))

      # Get indices of valid features in the original map
      valid_indices <- which(is_valid)

      # Loop only over valid items to rebuild geometries
      # Use lapply over the sequence of valid items
      rebuilt_geoms <- lapply(seq_along(valid_indices), function(k) {
        original_idx <- valid_indices[k]
        mat <- corrected_valid_list[[k]]
        type <- geom_types[original_idx]

        # Reconstruction Logic
        if (type == "POINT") {
          sf::st_point(mat[1, 1:2])

        } else if (type == "LINESTRING") {
          sf::st_linestring(mat[, 1:2])

        } else if (type == "POLYGON") {
          # Use L1 if available, otherwise assume single ring
          # L1 is usually column 3 (X, Y, L1) or column 4 (X, Y, L1, L2)
          if (ncol(mat) > 2 && !all(is.na(mat[, 3]))) {
            # Column 3 is typically L1
            rings <- split.data.frame(mat[, 1:2], mat[, 3]) # 3 is L1
          } else {
            rings <- list(mat[, 1:2])
          }

          # Close rings
          rings <- lapply(rings, function(r) {
            if (!isTRUE(all.equal(r[1,], r[nrow(r),]))) {
              rbind(r, r[1, ])
            } else r
          })
          sf::st_polygon(rings)

        } else if (type == "MULTIPOINT") {
          sf::st_multipoint(mat[, 1:2])

        } else if (type == "MULTILINESTRING") {
          # L1 is usually col 3
          if (ncol(mat) > 2 && !all(is.na(mat[, 3]))) {
            lines <- split.data.frame(mat[, 1:2], mat[, 3])
          } else {
            lines <- list(mat[, 1:2])
          }
          sf::st_multilinestring(lapply(lines, as.matrix))

        } else if (type == "MULTIPOLYGON") {
          # L2 (col 4) is Poly ID, L1 (col 3) is Ring ID
          if (ncol(mat) >= 4 && !all(is.na(mat[, 4]))) {
            polys <- split.data.frame(mat, mat[, 4]) # L2

            rebuilt_polys <- lapply(polys, function(p) {
              rings <- split.data.frame(p[, 1:2], p[, 3]) # L1
              rings <- lapply(rings, function(r) {
                if (!isTRUE(all.equal(r[1,], r[nrow(r),]))) {
                  rbind(r, r[1, ])
                } else r
              })
              sf::st_polygon(rings)
            })
            sf::st_multipolygon(rebuilt_polys)
          } else {
            # Fallback if structure is weird
            sf::st_multipolygon(list())
          }
        } else {
          # Fallback
          geom_col[[original_idx]]
        }
      })

      # Place rebuilt geometries back into the full list
      new_geom_list[valid_indices] <- rebuilt_geoms
    }

    # Create new SFC column
    new_sfc <- sf::st_sfc(new_geom_list, crs = sf::st_crs(map_inside_aoi))
    map_inside_aoi <- sf::st_set_geometry(map_inside_aoi, new_sfc)
  }

  # --- 4. Recombine Map Parts ---
  if (is.null(map_outside_aoi) || nrow(map_outside_aoi) == 0) {
    corrected_map <- map_inside_aoi
  } else if (is.null(map_inside_aoi) || nrow(map_inside_aoi) == 0) {
    corrected_map <- map_outside_aoi
  } else {
    corrected_map <- rbind(map_inside_aoi, map_outside_aoi)
  }

  # --- 5. Update Area ---
  if (any(grepl("POLYGON",
                sf::st_geometry_type(corrected_map, by_geometry = FALSE)))) {
    message("Recalculating polygon areas...")
    corrected_map$area_new <- sf::st_area(corrected_map)
  }

  message("Correction complete.")
  return(corrected_map)
}
