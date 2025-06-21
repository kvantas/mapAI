#' @title Perform a Differential Distortion Analysis
#' @description Computes detailed distortion metrics for a PAI model at specified locations.
#' @details
#' This function implements a differential analysis based on Tissot's
#' indicatrix theory... (rest of details section is unchanged)
#'
#' @param pai_model A model object of class `pai_model` from `train_pai_model()`.
#' @param points_to_analyze An `sf` object of **points** where the analysis should be performed.
#' @param reference_scale A single numeric value used to normalize the area scale calculation. Defaults to `1`.
#'
#' @return An `sf` object containing the original points and new columns with all
#'   calculated distortion metrics.
#'
#' @import sf
#' @import dplyr
#' @importFrom stats predict
#' @importFrom magrittr %>%
#' @export
#' @examples
#' # --- 1. Train a GAM model for a rich, spatially varying analysis ---
#' library(magrittr)
#' data(gcps)
#' gam_model <- train_pai_model(gcps, method = "gam")
#'
#' # --- 2. Analyze distortion on a regular grid of points ---
#' analysis_points <- sf::st_make_grid(gcps, n = c(20, 20)) %>%
#' sf::st_centroid() %>%
#'   sf::st_sf()
#'
#' distortion_results <- analyze_distortion(gam_model, analysis_points)
#'
#' # --- 3. View the new metric columns in the output ---
#' head(distortion_results)
#'
analyze_distortion <- function(pai_model, points_to_analyze, reference_scale = 1) {

  # --- Input Validation ---
  if (!inherits(pai_model, "pai_model")) stop("`pai_model` must be an object of class 'pai_model'.", call. = FALSE)
  if (!inherits(points_to_analyze, "sf")) stop("`points_to_analyze` must be an sf object.", call. = FALSE)

  coords_df <- as.data.frame(sf::st_coordinates(points_to_analyze))
  names(coords_df) <- c("source_x", "source_y")
  n_points <- nrow(coords_df)

  # --- Numerical Derivatives Calculation ---
  message(paste("Calculating distortion metrics for", pai_model$method, "model..."))

  # ... (Numerical differentiation logic remains the same) ...
  coord_range <- max(c(diff(range(coords_df$source_x)), diff(range(coords_df$source_y))), na.rm = TRUE)
  h <- coord_range * 1e-6
  coords_x_plus_h <- coords_df; coords_x_plus_h$source_x <- coords_df$source_x + h
  coords_x_minus_h <- coords_df; coords_x_minus_h$source_x <- coords_df$source_x - h
  coords_y_plus_h <- coords_df; coords_y_plus_h$source_y <- coords_df$source_y + h
  coords_y_minus_h <- coords_df; coords_y_minus_h$source_y <- coords_df$source_y - h
  T_x_plus  <- data.frame(predict(pai_model, newdata = coords_x_plus_h)) + coords_x_plus_h
  T_x_minus <- data.frame(predict(pai_model, newdata = coords_x_minus_h)) + coords_x_minus_h
  T_y_plus  <- data.frame(predict(pai_model, newdata = coords_y_plus_h)) + coords_y_plus_h
  T_y_minus <- data.frame(predict(pai_model, newdata = coords_y_minus_h)) + coords_y_minus_h
  names(T_x_plus) <- names(T_x_minus) <- names(T_y_plus) <- names(T_y_minus) <- c("target_x", "target_y")
  dfx_dx <- (T_x_plus$target_x - T_x_minus$target_x) / (2 * h)
  dfy_dx <- (T_x_plus$target_y - T_x_minus$target_y) / (2 * h)
  dfx_dy <- (T_y_plus$target_x - T_y_minus$target_x) / (2 * h)
  dfy_dy <- (T_y_plus$target_y - T_y_minus$target_y) / (2 * h)

  # --- Common calculation block ---
  message("Finalizing metrics from derivatives...")
  E <- dfx_dx^2 + dfy_dx^2
  G <- dfx_dy^2 + dfy_dy^2
  F_metric <- dfx_dx * dfx_dy + dfy_dx * dfy_dy
  sqrt_term <- sqrt(pmax(0, (E - G)^2 + 4 * F_metric^2))
  a <- sqrt(0.5 * (E + G + sqrt_term))
  b <- sqrt(0.5 * (E + G - sqrt_term))
  area_scale <- a * b
  max_shear_rad <- asin((a - b) / (a + b))
  theta_xp <- atan2(dfy_dx, dfx_dx)
  alpha_p <- atan2(2 * F_metric, E - G) / 2
  theta_a <- theta_xp - alpha_p

  # --- NEW: Add log2sigma and 2Omega metrics ---
  log2_area_scale <- log2(area_scale / (reference_scale^2))
  max_angular_distortion <- 2 * asin((a - b) / (a + b)) # This is 2*Omega

  # --- Add all metrics to the output sf object ---
  results_sf <- points_to_analyze %>%
    dplyr::mutate(
      a = a,
      b = b,
      area_scale = area_scale,
      log2_area_scale = log2_area_scale,
      max_shear = max_shear_rad * 180 / pi,
      max_angular_distortion = max_angular_distortion,
      theta_a = theta_a * 180 / pi
    )

  message("Distortion analysis complete.")
  return(results_sf)
}
