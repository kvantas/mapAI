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
  if (length(unique(source_x)) < 2 || length(unique(source_y)) < 2) {
    stop("Source points must not be co-located.", call. = FALSE)
  }
  if (length(unique(target_x)) < 2 || length(unique(target_y)) < 2) {
    stop("Target points must not be co-located.", call. = FALSE)
  }

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


#' Validate input for plot.distortion function
#' @keywords internal
#' @noRd
plot_input_validation <- function(x, metric, palette, diverging, value_range, add_points) {

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
indicatrices_validation <- function(object, scale_factor, fill_color, border_color){
  if (!inherits(object, "distortion")) {
    stop("`object` must be a distortion object.", call. = FALSE)
  }
  required_cols <- c("source_x", "source_y", "a", "b", "theta_a")
  if (!all(required_cols %in% names(object))) {
    stop("Input `object` is missing required columns: ",
         paste(setdiff(required_cols, names(object)), collapse = ", "), call. = FALSE)
  }
  # Allow NULL for automatic calculation
  if (!is.null(scale_factor) && (!is.numeric(scale_factor) || length(scale_factor) != 1 || scale_factor <= 0)) {
    stop("`scale_factor` must be NULL or a single positive numeric value.", call. = FALSE)
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
    stop("`gcp_data` must have at least 10 rows to perform a reliable assessment.",
         call. = FALSE)
  }

  # --- 2. pai_method Validation ---
  if (!is.character(pai_method) && !is.list(pai_method)) {
    stop("`pai_method` must be a character string (e.g., 'lm') or a list for custom models.",
         call. = FALSE)
  }


  # --- 3. validation_type Validation ---
  supported_validation <- c("random", "spatial", "probability", "stratified")
  if (!validation_type %in% supported_validation) {
    stop(paste0("Invalid `validation_type`. Must be one of: '",
                paste(supported_validation, collapse = "', '"), "'."), call. = FALSE)
  }

  # --- 4. Contextual Validation for k_folds and train_split_ratio ---
  if (validation_type %in% c("random", "spatial")) {
    # Validate k_folds
    if (!is.numeric(k_folds) || length(k_folds) != 1 || k_folds < 2 || k_folds %% 1 != 0) {
      stop("For cross-validation, `k_folds` must be a single integer greater than or equal to 2.", call. = FALSE)
    }
    if (nrow(gcp_data) < k_folds) {
      stop("The number of data points must be greater than or equal to `k_folds`.", call. = FALSE)
    }
  } else if (validation_type %in% c("probability", "stratified")) {
    # Validate train_split_ratio
    if (!is.numeric(train_split_ratio) || length(train_split_ratio) != 1 ||
        train_split_ratio <= 0 || train_split_ratio >= 1) {
      stop("For single-split validation, `train_split_ratio` must be a single number between 0 and 1.", call. = FALSE)
    }
  }

  # If all checks pass, return invisibly
  invisible(NULL)
}
