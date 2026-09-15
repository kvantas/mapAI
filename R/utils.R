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

  valid_metrics <- c("a", "b", "area_scale", "signed_area_scale", "det_J", "is_inverted",
                     "log2_area_scale", "max_angular_distortion",
                     "theta_a", "airy_kavrayskiy")
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
                                       k_folds, train_split_ratio, n_strata = 4) {

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
  supported_validation <- c("random", "spatial", "probability", "stratified",
                            "spatial_block", "spatial_buffered")
  if (!validation_type %in% supported_validation) {
    stop(paste0("Invalid `validation_type`. Must be one of: '",
                paste(supported_validation, collapse = "', '"), "'."),
         call. = FALSE)
  }

  # --- 4. Contextual Validation for k_folds and train_split_ratio ---
  if (validation_type %in% c("random", "spatial", "stratified", "spatial_block", "spatial_buffered")) {
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
    if (validation_type == "stratified") {
      if (!is.numeric(n_strata) || length(n_strata) != 1 || n_strata < 2 || n_strata %% 1 != 0) {
        stop("`n_strata` must be a single integer greater than or equal to 2.", call. = FALSE)
      }
    }
  } else if (validation_type == "probability") {
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


#' Assert that spatial data uses a projected coordinate system
#'
#' Issues a warning if the coordinate reference system is geographic (degrees),
#' since PAI transformations operate in planar Cartesian coordinate space.
#'
#' @keywords internal
#' @noRd
assert_projected_crs <- function(x, name = "map") {
  if (inherits(x, c("sf", "sfc", "gcp"))) {
    crs_val <- if (inherits(x, "gcp")) attr(x, "crs") else sf::st_crs(x)
    if (is.null(crs_val) || is.na(crs_val)) {
      return(invisible(NULL))
    }
    if (isTRUE(sf::st_is_longlat(crs_val))) {
      warning(
        sprintf("The CRS of `%s` is geographic (longitude/latitude in degrees). PAI transformations assume planar Cartesian units (e.g., meters); consider projecting using sf::st_transform().", name),
        call. = FALSE
      )
    }
  } else if (inherits(x, "SpatRaster")) {
    crs_val <- terra::crs(x)
    if (nzchar(crs_val) && isTRUE(terra::is.lonlat(x))) {
      warning(
        sprintf("The CRS of `%s` is geographic (longitude/latitude in degrees). PAI transformations assume planar Cartesian units (e.g., meters); consider projecting using terra::project().", name),
        call. = FALSE
      )
    }
  }
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

  assert_projected_crs(map, name = "map")

  if (!is.null(aoi)) {
    if (!inherits(aoi, "sf") || !any(sf::st_geometry_type(aoi) %in% c("POLYGON", "MULTIPOLYGON"))) {
      stop("`aoi` must be a valid `sf` object with POLYGON or MULTIPOLYGON geometry.", call. = FALSE)
    }
    # Ensure AOI has the same CRS as the map. Reprojecting here would only update
    # a local copy that the caller never sees, so return the transformed AOI and
    # let the caller substitute it.
    if (sf::st_crs(aoi) != sf::st_crs(map)) {
      aoi <- sf::st_transform(aoi, sf::st_crs(map))
      message("Transformed `aoi` CRS to match `map` CRS.")
    }
    assert_projected_crs(aoi, name = "aoi")
  }

  # Return the (possibly reprojected) AOI so the caller can use it.
  invisible(aoi)

}


validate_write_map <- function(map, file_path, overwrite) {

  # --- Input Validation ---
  if (!inherits(map, c("sf", "SpatRaster"))) {
    stop("`map` must be a valid `sf` or `SpatRaster` object.", call. = FALSE)
  }
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

#' Validate a control point network for model fitting
#'
#' @description
#' Shared structural checks on a `gcp` object, used by both [train_pai_model()]
#' and [assess_pai_model()] so the two entry points agree about what counts as
#' usable data. Catches the degeneracies that otherwise produce a fitted model
#' whose coefficients are `NA` -- a model that predicts `NA` at
#' [transform_map()] or [apply_pai_raster()] time, far from the cause.
#'
#' @param gcp_data An object of class `gcp`.
#' @param min_points Minimum number of control points required.
#' @param context Short label naming the caller, used in messages.
#' @param check_rank Require the source coordinates to span two dimensions.
#'   `TRUE` for any model carrying a bivariate linear term (`lm`, `tps`,
#'   `gam_biv`, the TIN family), where collinear points give a rank-deficient
#'   fit. `FALSE` for the rigid Helmert transform, which remains estimable from
#'   collinear control points, and for custom plugins whose requirements are
#'   unknown.
#' @return Invisibly `NULL`; called for its side effect of raising errors.
#' @noRd
validate_gcp_network <- function(gcp_data, min_points = 3L,
                                 context = "train_pai_model()",
                                 check_rank = TRUE) {

  if (!inherits(gcp_data, "gcp")) {
    stop("`gcp_data` must be an object of class 'gcp'.", call. = FALSE)
  }

  required_cols <- c("source_x", "source_y", "dx", "dy")
  missing_cols <- setdiff(required_cols, names(gcp_data))
  if (length(missing_cols) > 0) {
    stop("`gcp_data` is missing required column(s): ",
         paste(missing_cols, collapse = ", "), ".", call. = FALSE)
  }

  # NA control points were previously dropped silently by stats::lm(), so the
  # model was fitted on fewer points than the user supplied without saying so.
  incomplete <- !stats::complete.cases(gcp_data[, required_cols])
  if (any(incomplete)) {
    stop(sprintf(
      paste0("`gcp_data` contains NA values in %d of %d control point(s) ",
             "(columns source_x, source_y, dx, dy). Remove or repair them ",
             "before fitting."),
      sum(incomplete), nrow(gcp_data)), call. = FALSE)
  }

  if (!all(vapply(gcp_data[, required_cols], is.numeric, logical(1)))) {
    stop("Columns source_x, source_y, dx and dy must all be numeric.",
         call. = FALSE)
  }

  if (any(!is.finite(as.matrix(gcp_data[, required_cols])))) {
    stop("`gcp_data` contains non-finite (Inf or NaN) values in source_x, ",
         "source_y, dx or dy.", call. = FALSE)
  }

  n <- nrow(gcp_data)
  if (n < min_points) {
    stop(sprintf("%s needs at least %d control points; %d supplied.",
                 context, min_points, n), call. = FALSE)
  }

  # Rank of the source coordinates. Co-located or collinear control points carry
  # no 2D information, so any model with a bivariate linear term is rank
  # deficient and returns NA slopes.
  span_x <- diff(range(gcp_data$source_x))
  span_y <- diff(range(gcp_data$source_y))
  scale_xy <- max(1, max(abs(range(gcp_data$source_x))),
                  max(abs(range(gcp_data$source_y))))
  tol <- .Machine$double.eps^0.5 * scale_xy

  if (span_x <= tol && span_y <= tol) {
    stop("All control points share the same source coordinates. ",
         "A transformation cannot be estimated from co-located points.",
         call. = FALSE)
  }

  n_unique <- nrow(unique(gcp_data[, c("source_x", "source_y")]))
  if (n_unique < min_points) {
    stop(sprintf(
      paste0("`gcp_data` has only %d distinct source location(s) among %d ",
             "control points; at least %d are required."),
      n_unique, n, min_points), call. = FALSE)
  }

  # Collinearity: all points on one line means one coordinate direction carries
  # no information, so any bivariate linear term is rank deficient and its
  # coefficient comes back NA. Measured as the spread perpendicular to the
  # principal axis (the second singular value of the centred coordinates).
  if (isTRUE(check_rank) && n_unique >= 3) {
    xy <- as.matrix(unique(gcp_data[, c("source_x", "source_y")]))
    xy <- sweep(xy, 2, colMeans(xy), "-")
    sv <- svd(xy)$d
    if (length(sv) >= 2 && sv[2] <= .Machine$double.eps^0.5 * max(1, sv[1])) {
      stop("All control points are collinear in source space. A bivariate ",
           "transformation cannot be estimated from a single line of points; ",
           "method = \"helmert\" is estimable from collinear points if a rigid ",
           "similarity transform is sufficient.", call. = FALSE)
    }
  }

  invisible(NULL)
}

#' Requirements of a built-in method for its control point network
#'
#' @description
#' Minimum control point count and whether the source coordinates must span two
#' dimensions. Custom plugins are treated permissively, since their parameter
#' count is unknown: they get the universal checks (NA, non-finite, co-location)
#' but no rank or count assumption.
#'
#' @noRd
gcp_requirements <- function(method) {
  label <- if (is.character(method)) method else NA_character_

  if (identical(label, "helmert")) {
    # 4 parameters estimated from 2n equations, and estimable from collinear
    # points because the normal matrix depends only on the summed squared
    # distances from the centroid.
    list(min_points = 2L, check_rank = FALSE)
  } else if (is.character(label) && !is.na(label)) {
    # lm is 6-parameter affine; tps, gam_biv and the TIN family all need a
    # genuinely 2D point set.
    list(min_points = 3L, check_rank = TRUE)
  } else {
    list(min_points = 2L, check_rank = FALSE)
  }
}
