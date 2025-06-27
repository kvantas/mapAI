#' @title Perform a Differential Distortion Analysis
#' @description Computes a comprehensive set of detailed distortion metrics for
#'   a PAI model at specified locations, based on Tissot's indicatrix theory.
#'
#' @details This function is the core analytical engine of the `mapAI` package.
#'   It implements a differential analysis by calculating the first partial
#'   derivatives of the spatial transformation learned by a `pai_model`. This is
#'   achieved using a  **numerical differentiation** (finite difference) method
#'   that is universally applicable to all models in the package (`lm`, `rf`,
#'   `gam`, `helmert`).
#'
#'   From these derivatives, it calculates key distortion metrics that describe
#'   how shape, area, and angles are warped at every point.
#'
#' **Interpreting Results by Model Type:**
#'
#'   The nature of the output is **highly dependent** on the model used:
#' \itemize{
#'   \item \strong{`gam` & `tps` (Recommended for this analysis)}: Produce a
#'   smooth, differentiable surface. The distortion metrics will be
#'     **spatially variable** and provide a rich, meaningful understanding of
#'     how distortion changes across the map.
#'   \item \strong{`helmert` & `lm`}: Represent global transformations.
#'   The distortion metrics will be **constant for every point**.
#'   \item \strong{`rf`}: Creates a step-like surface. The local derivatives may
#'    be effectively zero, resulting in metrics indicating no local distortion
#'    (e.g.,  `area_scale` = 1, `max_shear` = 0).
#' }
#'
#' @param pai_model A model object of class `pai_model` from
#'   `train_pai_model()`.
#' @param points_to_analyze An `sf` object of **points** where the analysis
#'   should be performed.
#' @param reference_scale A single numeric value used to normalize the area
#'   scale calculation. Defaults to `1` (no normalization).
#'
#' @return An `sf` object containing the original points and new columns with
#'   all calculated distortion metrics: \item{a, b}{The semi-major and
#'   semi-minor axes of the Tissot indicatrix.} \item{area_scale}{The areal
#'   distortion factor (`a * b`).} \item{log2_area_scale}{The base-2 logarithm
#'   of `area_scale`, a symmetric metric centered at 0.} \item{max_shear}{The
#'   maximum angular distortion in degrees.} \item{max_angular_distortion}{The
#'   maximum angular distortion in radians (the `2Omega` metric).}
#'   \item{airy_kavrayskiy}{The Airy-Kavrayskiy measure, a balanced metric
#'   combining areal and angular distortion.} \item{theta_a}{The orientation of
#'   the axis of maximum scale (in degrees).}
#'
#' @import sf
#' @import dplyr
#' @importFrom stats predict
#' @importFrom magrittr %>%
#' @export
#' @examples
#' # This example showcases the full analytical workflow.
#'
#' library(magrittr)
#'
#' # --- 1. Load data and train a GAM model ---
#' data(gcps)
#' gam_model <- train_pai_model(gcps, method = "gam")
#'
#' # --- 2. Create a regular grid of POINTS for analysis ---
#' analysis_points <- sf::st_make_grid(gcps, n = c(25, 25)) %>%
#'   sf::st_centroid() %>%
#'   sf::st_sf()
#'
#' # --- 3. Run the distortion analysis ---
#' distortion_results <- analyze_distortion(gam_model, analysis_points)
#'
#' # --- 4. Visualize the area scale ---
#' plot_distortion_surface(
#'   distortion_results,
#'   metric = "area_scale")
#'
analyze_distortion <- function(pai_model, points_to_analyze, reference_scale = 1) {

  # --- Input Validation ---
  if (!inherits(pai_model, "pai_model")) {
    stop("`pai_model` must be an object of class 'pai_model'.", call. = FALSE)
  }
  if (!inherits(points_to_analyze, "sf")) {
    stop("`points_to_analyze` must be an sf object.", call. = FALSE)
  }

  coords_df <- as.data.frame(sf::st_coordinates(points_to_analyze))
  names(coords_df) <- c("source_x", "source_y")
  n_points <- nrow(coords_df)

  # --- Numerical Derivatives Calculation ---
  message(paste("Calculating distortion metrics for",
                pai_model$method, "model..."))

  coord_range <- max(c(diff(range(coords_df$source_x)),
                       diff(range(coords_df$source_y))), na.rm = TRUE)
  h <- coord_range * 1e-6
  coords_x_plus_h <- coords_df
  coords_x_plus_h$source_x <- coords_df$source_x + h

  coords_x_minus_h <- coords_df
  coords_x_minus_h$source_x <- coords_df$source_x - h

  coords_y_plus_h <- coords_df
  coords_y_plus_h$source_y <- coords_df$source_y + h

  coords_y_minus_h <- coords_df
  coords_y_minus_h$source_y <- coords_df$source_y - h

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

  # --- compute log2sigma, 2*Omega, Airy-Kavrayskiy metrics
  log2_area_scale <- log2(area_scale / (reference_scale^2))
  max_angular_distortion <- 2 * asin((a - b) / (a + b)) # This is 2*Omega
  airy_kavrayskiy <- 0.5 * (log(a)^2 + log(b)^2)

  # --- Add all metrics to the output sf object ---
  results_sf <- points_to_analyze %>%
    dplyr::mutate(
      a = a,
      b = b,
      area_scale = area_scale,
      log2_area_scale = log2_area_scale,
      max_shear = max_shear_rad * 180 / pi,
      max_angular_distortion = max_angular_distortion,
      airy_kavrayskiy = airy_kavrayskiy,
      theta_a = theta_a * 180 / pi
    )

  message("Distortion analysis complete.")
  return(results_sf)
}
