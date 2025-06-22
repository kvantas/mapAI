#' @title Swiss Control Points for Distortion Analysis
#' @description A dataset of 343 Control Points (CPs) based on the sample dataset
#'   from the MapAnalyst distortion analysis software. It's ideal for analyzing
#'   complex, non-linear distortions.
#'
#' @details
#' This dataset is derived from the sample data provided with the MapAnalyst
#' software (<http://mapanalyst.cartography.ch>).
#'
#' The defining characteristic of this dataset is that the source coordinates
#' (`source_x`, `source_y`) have already been globally aligned to the target
#' coordinates using a Helmert transformation. The remaining differences (`dx`, `dy`)
#' therefore represent the complex, non-linear residual distortions.
#'
#' This makes the dataset an excellent test case for evaluating the ability of
#' models like `gam` and `rf` to model and correct these challenging error patterns,
#' which a simple `helmert` or `lm` model would not be able to address.
#'
#' @format An `sf` data frame with 300 rows (points) and 7 columns:
#' \describe{
#'   \item{source_x}{Numeric. The X-coordinate on the source map (already globally aligned).}
#'   \item{source_y}{Numeric. The Y-coordinate on the source map (already globally aligned).}
#'   \item{target_x}{Numeric. The X-coordinate on the reference map.}
#'   \item{target_y}{Numeric. The Y-coordinate on the reference map.}
#'   \item{dx}{Numeric. The residual difference in X (target_x - source_x).}
#'   \item{dy}{Numeric. The residual difference in Y (target_y - source_y).}
#'   \item{geometry}{`sfc_POINT`. The `sf` point geometry representing the `source_x`
#'     and `source_y` locations in the Swiss CH1903 / LV03 coordinate system (EPSG:21781).}
#' }
#' @source Data originally provided as a sample dataset for the MapAnalyst
#'   distortion analysis software. See <http://mapanalyst.cartography.ch>.
#'
#' @importFrom magrittr %>%
#' @examples
#' # This example demonstrates a powerful use case for the swiss_cps dataset:
#' # 1. Load the data.
#' # 2. Train a GAM model to learn the complex, non-linear residual errors.
#' # 3. Visualize the learned distortion surface.
#'
#' library(mapAI)
#' library(sf)
#'
#' # Load the dataset
#' data(swiss_cps)
#'
#' # Train a GAM model. It will learn the non-linear patterns that remain
#' # after the initial Helmert alignment.
#' gam_model <- train_pai_model(swiss_cps, method = "gam")
#'
#' # Analyze the distortion on a regular grid of points
#' analysis_grid <- sf::st_make_grid(swiss_cps, n = c(20, 20)) %>%
#'   sf::st_centroid() %>%
#'   sf::st_sf()
#'
#' distortion_results <- analyze_distortion(gam_model, analysis_grid)
#'
#' # Plot the learned 'log2_area_scale'. This is a symmetric metric centered
#' # at 0, making it ideal for a diverging palette. Red areas were expanded,
#' # blue areas were contracted.
#' plot_distortion_surface(
#'   distortion_results,
#'   metric = "log2_area_scale",
#'   diverging = TRUE
#' )
#'
"swiss_cps"
