#' Description: Internal utility functions for input validation and error
#' handling for the pai_model object
#' @keywords internal
#' @noRd
pai_model_val <- function(object,
                          n_grid,
                          plot_gcp,
                          dx_range,
                          dy_range) {
  if (!inherits(object, "pai_model")) {
    stop("The 'object' must be of class 'pai_model'.", call. = FALSE)
  }
  if (!is.numeric(n_grid) || n_grid <= 0 || n_grid != round(n_grid)) {
    stop("n_grid must be a positive integer.", call. = FALSE)
  }
  if (!is.logical(plot_gcp) || length(plot_gcp) != 1) {
    stop("plot_gcp must be a single logical value (TRUE or FALSE).",
         call. = FALSE)
  }
  if (!is.null(dx_range) && (!is.numeric(dx_range) || length(dx_range) != 2)) {
    stop("dx_range must be a numeric vector of length 2 or NULL.",
         call. = FALSE)
  }
  if (!is.null(dy_range) && (!is.numeric(dy_range) || length(dy_range) != 2)) {
    stop("dy_range must be a numeric vector of length 2 or NULL.",
         call. = FALSE)
  }

  invisible(NULL)

}

#' Validate Input for Helmert  and create_gcps function
#' @keywords internal
#' @noRd
input_validation <- function(source_x, source_y,
                             target_x, target_y) {

  if (length(source_x) != length(source_y) ||
      length(source_x) != length(target_x) ||
      length(source_x) != length(target_y)) {
    stop("All input vectors must have the same length.", call. = FALSE)
  }
  if (length(source_x) < 2) {
    stop("At least two points are required to compute a transformation.",
         call. = FALSE)
  }
  if (any(!is.finite(c(source_x, source_y, target_x, target_y)))) {
    stop("All input coordinates must be finite numbers.", call. = FALSE)
  }
  # if (length(unique(source_x)) < 2 || length(unique(source_y)) < 2) {
  #   stop("Source points must not be co-located.", call. = FALSE)
  # }
  # if (length(unique(target_x)) < 2 || length(unique(target_y)) < 2) {
  #   stop("Target points must not be co-located.", call. = FALSE)
  # }

  # If all checks pass, return invisibly
  invisible(NULL)
}

#' Validate newdata for predict.pai_train function
#' @keywords internal
#' @noRd
new_data_validation <- function(newdata) {


  if (!all(c("source_x", "source_y") %in% names(newdata))) {
    stop("`newdata` must contain 'source_x' and 'source_y' columns.",
         call. = FALSE)
  }

  if (any(!is.finite(c(newdata$source_x, newdata$source_y)))) {
    stop("All coordinates in `newdata` must be finite numbers.",
         call. = FALSE)
  }

  # If all checks pass, return invisibly
  invisible(NULL)

}

#' Validate analyze_distortion function
#' @keywords internal
#' @noRd
an_dist_validation <- function(pai_model, reference_scale){


  if (!inherits(pai_model, "pai_model")) {
    stop("`pai_model` must be an object of class 'pai_model'.", call. = FALSE)
  }


  if (!is.numeric(reference_scale) ||
      length(reference_scale) != 1 ||
      reference_scale <= 0) {
    stop("`reference_scale` must be a single positive numeric value.",
         call. = FALSE)
  }

  # If all checks pass, return invisibly
  invisible(NULL)

}

#' Validate input for plot.distortion function
#' @keywords internal
#' @noRd
plot_input_validation <- function(
    x,
    metric,
    palette,
    diverging,
    value_range,
    add_points) {

  if (!inherits(x, "distortion")) {
    stop("`x` must be an object of class 'distortion'.", call. = FALSE)
  }

  valid_metrics <- c("a", "b", "area_scale", "log2_area_scale", "max_shear",
                     "max_angular_distortion", "theta_a", "airy_kavrayskiy")
  if (!metric %in% valid_metrics) {
    stop(paste("`metric` must be one of:",
               paste(valid_metrics, collapse = ", ")), call. = FALSE)
  }
  if (!is.character(palette) || length(palette) != 1) {
    stop("`palette` must be a single character string.", call. = FALSE)
  }
  viridis_palettes <- c("viridis", "magma", "plasma", "inferno", "cividis",
                        "rocket", "turbo", "mako")
  if(!palette %in% viridis_palettes) {
    stop("`palette` must be a valid viridis color name.", call. = FALSE)
  }

  if (!is.logical(diverging) || length(diverging) != 1) {
    stop("`diverging` must be a single logical value.", call. = FALSE)
  }
  if (!is.null(value_range)) {
    if (!is.numeric(value_range) || length(value_range) != 2 ||
        value_range[1] >= value_range[2]) {
      stop(
        "`value_range` must be an increasing numeric vector of length 2.",
        call. = FALSE)
    }
  }
  if (!is.logical(add_points) || length(add_points) != 1) {
    stop("`add_points` must be a single logical value.", call. = FALSE)
  }

  # If all checks pass, return invisibly
  invisible(NULL)
}

#' Validate input for indicatrices.distortion function
#' @keywords internal
#' @noRd
indicatrices_validation <- function(object,
                                    scale_factor,
                                    fill_color,
                                    border_color){
  if (!inherits(object, "distortion")) {
    stop("`object` must be a distortion object.", call. = FALSE)
  }
  required_cols <- c("source_x", "source_y", "a", "b", "theta_a")
  if (!all(required_cols %in% names(object))) {
    stop("Input `object` is missing required columns: ",
         paste(setdiff(required_cols, names(object)), collapse = ", "),
         call. = FALSE)
  }
  # Allow NULL for automatic calculation
  if (
    !is.null(scale_factor) &&
    (!is.numeric(scale_factor) ||
     length(scale_factor) != 1 ||
     scale_factor <= 0)) {
    stop("`scale_factor` must be NULL or a single positive numeric value.",
         call. = FALSE)
  }
  if (!is.character(fill_color) || length(fill_color) != 1) {
    stop("`fill_color` must be a single character string.", call. = FALSE)
  }
  if (!is.character(border_color) || length(border_color) != 1) {
    stop("`border_color` must be a single character string.", call. = FALSE)
  }

  # If all checks pass, return invisibly
  invisible(NULL)
}


#' Validate Inputs for the assess_pai_model function
#' @keywords internal
#' @noRd
validate_assessment_inputs <- function(gcp_data, pai_method, validation_type,
                                       k_folds, train_split_ratio) {

  # --- 1. gcp_data Validation ---
  if (!inherits(gcp_data, "gcp")) {
    stop("`gcp_data` must be an object of class 'gcp'.", call. = FALSE)
  }

  # Check for missing values in essential columns
  required_cols <- c("source_x", "source_y", "dx", "dy")
  if (any(!stats::complete.cases(gcp_data[, required_cols]))) {
    stop("`gcp_data` contains NA values in one of the required columns: ",
         "source_x, source_y, dx, dy.", call. = FALSE)
  }

  if (nrow(gcp_data) < 10) {
    stop(
      "`gcp_data` must have at least 10 rows to perform a reliable assessment.",
      call. = FALSE)
  }

  # --- 2. pai_method Validation ---
  if (!is.character(pai_method) && !is.list(pai_method)) {
    stop(
      "`pai_method` must be a character string (e.g., 'lm') or a list for custom models.",
         call. = FALSE)
  }


  # --- 3. validation_type Validation ---
  supported_validation <- c("random", "spatial", "probability", "stratified")
  if (!validation_type %in% supported_validation) {
    stop(paste0("Invalid `validation_type`. Must be one of: '",
                paste(supported_validation, collapse = "', '"), "'."),
         call. = FALSE)
  }

  # --- 4. Contextual Validation for k_folds and train_split_ratio ---
  if (validation_type %in% c("random", "spatial")) {
    # Validate k_folds
    if (!is.numeric(k_folds) || length(k_folds) != 1 || k_folds < 2 || k_folds %% 1 != 0) {
      stop(
        "For cross-validation, `k_folds` must be a single integer greater than or equal to 2.",
        call. = FALSE)
    }
    if (nrow(gcp_data) < k_folds) {
      stop(
        "The number of data points must be greater than or equal to `k_folds`.",
        call. = FALSE)
    }
  } else if (validation_type %in% c("probability", "stratified")) {
    # Validate train_split_ratio
    if (!is.numeric(train_split_ratio) || length(train_split_ratio) != 1 ||
        train_split_ratio <= 0 || train_split_ratio >= 1) {
      stop(
        "For single-split validation, `train_split_ratio` must be a single number between 0 and 1.",
        call. = FALSE)
    }
  }

  # If all checks pass, return invisibly
  invisible(NULL)
}


#' Validate Inputs for the create_demo_data function
#'
#' This is an internal helper function that checks the validity of all arguments
#' passed to `create_demo_data`. It stops execution with an informative error
#' message if any check fails.
#'
#' @keywords internal
#' @noRd
validate_demo_data_inputs <- function(type, noise_sd, n_points, seed,
                                      grid_limits, helmert_params,
                                      poly_params, gauss_params) {

  # --- Nested Helper for Validating Parameter Lists ---
  check_param_list <- function(param_list, required_names, param_name) {
    if (!is.list(param_list)) {
      stop(paste0("`", param_name, "` must be a list."), call. = FALSE)
    }

    missing <- setdiff(required_names, names(param_list))
    if (length(missing) > 0) {
      stop(paste0("`", param_name, "` is missing required elements: ",
                  paste(missing, collapse = ", ")), call. = FALSE)
    }

    all_numeric_scalar <- all(vapply(param_list[required_names], function(x) {
      is.numeric(x) && length(x) == 1
    }, logical(1)))

    if (!all_numeric_scalar) {
      stop(
        paste0("All elements in `",
               param_name,
               "` must be single numeric values."),
        call. = FALSE)
    }
  }

  # --- 1. Main Argument Validation ---

  # Validate `type`
  supported_types <- c("helmert", "nonlinear", "complex")
  if (!is.character(type) || length(type) != 1 || !type %in% supported_types) {
    stop("`type` must be one of 'helmert', 'nonlinear', or 'complex'.",
         call. = FALSE)
  }

  # Validate `noise_sd`
  if (!is.numeric(noise_sd) || length(noise_sd) != 1 || noise_sd < 0) {
    stop("`noise_sd` must be a single, non-negative numeric value.",
         call. = FALSE)
  }

  # Validate `n_points`
  if (!is.numeric(n_points) ||
      length(n_points) != 1 ||
      n_points %% 1 != 0 ||
      n_points < 2) {
    stop("`n_points` must be a single integer greater than or equal to 2.",
         call. = FALSE)
  }

  # Validate `seed`
  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1)) {
    stop("`seed` must be NULL or a single numeric value.", call. = FALSE)
  }

  # Validate `grid_limits`
  if (!is.numeric(grid_limits) || length(grid_limits) != 4) {
    stop(
      "`grid_limits` must be a numeric vector of 4 elements (xmin, xmax, ymin, ymax).",
      call. = FALSE)
  }
  if (grid_limits[1] >= grid_limits[2] || grid_limits[3] >= grid_limits[4]) {
    stop("In `grid_limits`, xmin must be less than xmax, and ymin must be less than ymax.",
         call. = FALSE)
  }

  # --- 2. Parameter List Validation using the Helper ---

  # Validate `helmert_params`
  check_param_list(helmert_params, c("s", "angle_deg", "tx", "ty"), "helmert_params")
  if (helmert_params$s <= 0) {
    stop("The scale factor `s` in `helmert_params` must be positive.", call. = FALSE)
  }

  # Validate `poly_params`
  check_param_list(poly_params, c("cE1", "cE2", "cN1", "cN2"), "poly_params")

  # Validate `gauss_params`
  check_param_list(gauss_params, c("A", "Ec", "Nc", "sigma2"), "gauss_params")
  if (gauss_params$sigma2 <= 0) {
    stop("The variance `sigma2` in `gauss_params` must be positive.", call. = FALSE)
  }

  # If all checks pass, return invisibly
  invisible(NULL)
}


#' Validate Inputs for the map_transform function
#'
#' This is an internal helper function that checks the validity of all arguments
#' passed to `map_transform`. It stops execution with an informative error
#' message if any check fails.
#'
#' @keywords internal
#' @noRd
validate_map_transform <- function(pai_model, map, aoi){

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

  # If all checks pass, return invisibly
  invisible(NULL)

}


validate_write_map <- function(map, file_path, overwrite) {

  # --- Input Validation ---
  if (!inherits(map, "sf")) stop("`map` must be a valid `sf` object.",
                                 call. = FALSE)
  if (!is.character(file_path) || length(file_path) != 1) {
    stop("`file_path` must be a single character string.", call. = FALSE)
  }
  if (!is.logical(overwrite) || length(overwrite) != 1) {
    stop("`overwrite` must be a single logical value (TRUE or FALSE).",
         call. = FALSE)
  }


  # If all checks pass, return invisibly
  invisible(NULL)
}
