#' @title Apply a Trained PAI Model to Correct a Vector Map
#' @description Applies a trained `pai_model` object to an `sf` vector map,
#'   correcting the position of all its vertices based on the learned transformation.
#' @details This function is the final step in the PAI workflow. It iterates through
#'   each feature of the input `map`, uses the trained `pai_model` to predict the
#'   necessary `dx` and `dy` correction for each vertex, and then reconstructs the
#'   map's geometry with the corrected coordinates. This feature-by-feature approach
#'   is robust and correctly handles simple and complex geometries (e.g., MULTIPOLYGON).
#'
#'   The prediction logic correctly handles the different output structures of the
#'   supported model types:
#'   \itemize{
#'     \item \strong{`gam`}: Predicts `dx` and `dy` simultaneously from the bivariate model.
#'     \item \strong{`lm`}: Predicts `dx` and `dy` from two separate linear models.
#'     \item \strong{`rf`}: Predicts `dx` and `dy` from two separate `ranger` models.
#'   }
#'
#'   If the input `map` contains `POLYGON` or `MULTIPOLYGON` geometries, the function
#'   also calculates the area of the newly corrected features and adds it to a column
#'   named `area_new`.
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
#' @examples
#' \dontrun{
#' # --- 1. Generate demo data and read it into R ---
#' demo_files <- create_demo_data(type = "complex", seed = 123)
#' gcp_data <- read_gcps(gcp_path = demo_files$gcp_path, crs = 3857)
#' map_to_correct <- read_map(shp_path = demo_files$shp_path)
#'
#' # --- 2. Train a PAI model ---
#' rf_model <- train_pai_model(gcp_data, method = "rf")
#'
#' # --- 3. Apply the trained model to the map ---
#' corrected_map <- apply_pai_model(pai_model = rf_model, map = map_to_correct)
#'
#' # --- 4. Visualize the results ---
#' plot(sf::st_geometry(map_to_correct), border = 'grey', lty = 2,
#'      main = "Original (Grey) vs. Corrected (Red)")
#' plot(sf::st_geometry(corrected_map), border = 'red', add = TRUE)
#' }
apply_pai_model <- function(pai_model, map) {

  # --- 1. Input Validation ---
  if (!inherits(pai_model, "pai_model")) {
    stop("`pai_model` must be an object of class 'pai_model', created by `train_pai_model()`.", call. = FALSE)
  }
  if (!inherits(map, "sf")) {
    stop("`map` must be a valid `sf` object.", call. = FALSE)
  }

  message("Applying PAI model: correcting map features one by one...")

  # --- 2. Prepare for Feature-by-Feature Iteration ---
  original_geom_col <- sf::st_geometry(map)
  new_geom_list <- vector("list", length = length(original_geom_col))

  # Helper function to transform a single coordinate matrix
  transform_coords <- function(coord_matrix) {
    if (is.null(coord_matrix) || nrow(coord_matrix) == 0) return(coord_matrix)

    data_for_prediction <- as.data.frame(coord_matrix) %>%
      dplyr::select(source_x = 1, source_y = 2)

    if (pai_model$method == "gam") {
      predictions <- predict(pai_model$model, newdata = data_for_prediction)
      pred_dx <- predictions[, 1]
      pred_dy <- predictions[, 2]
    } else {
      pred_dx_obj <- predict(pai_model$model$model_dx, data = data_for_prediction)
      pred_dy_obj <- predict(pai_model$model$model_dy, data = data_for_prediction)
      pred_dx <- if (inherits(pred_dx_obj, "ranger.prediction")) pred_dx_obj$predictions else pred_dx_obj
      pred_dy <- if (inherits(pred_dy_obj, "ranger.prediction")) pred_dy_obj$predictions else pred_dy_obj
    }

    coord_matrix[, 1] <- coord_matrix[, 1] + pred_dx
    coord_matrix[, 2] <- coord_matrix[, 2] + pred_dy
    return(coord_matrix)
  }

  # --- 3. Iterate through each feature ---
  for (i in seq_along(original_geom_col)) {
    feature <- original_geom_col[[i]]
    geom_type <- sf::st_geometry_type(feature)

    if (geom_type %in% c("POLYGON", "LINESTRING")) {
      new_rings <- lapply(feature, transform_coords)
      new_geom_list[[i]] <- if (geom_type == "POLYGON") sf::st_polygon(new_rings) else sf::st_linestring(new_rings[[1]])
    } else if (geom_type %in% c("MULTIPOLYGON", "MULTILINESTRING")) {
      new_parts <- lapply(feature, function(part) {
        lapply(part, transform_coords)
      })
      new_geom_list[[i]] <- if (geom_type == "MULTIPOLYGON") sf::st_multipolygon(new_parts) else sf::st_multilinestring(new_parts)
    } else if (geom_type %in% c("POINT")) {
      new_geom_list[[i]] <- sf::st_point(transform_coords(as.matrix(feature)))
    } else if (geom_type %in% c("MULTIPOINT")) {
      new_geom_list[[i]] <- sf::st_multipoint(transform_coords(as.matrix(feature)))
    } else {
      warning(paste("Geometry type", geom_type, "at feature", i, "is not currently supported for correction."), call. = FALSE)
      new_geom_list[[i]] <- feature # Keep original if unsupported
    }
  }

  # --- 4. Update SF Object Geometry ---
  new_sfc <- sf::st_sfc(new_geom_list, crs = sf::st_crs(map))
  corrected_map <- sf::st_set_geometry(map, new_sfc)

  # --- 5. Calculate new area for polygons ---
  if (any(sf::st_geometry_type(map) %in% c("POLYGON", "MULTIPOLYGON"))) {
    message("Calculating area of corrected polygons...")
    corrected_map$area_new <- sf::st_area(corrected_map)
  }

  message("Correction complete.")
  return(corrected_map)
}
