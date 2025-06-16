#' @title Ground Control Points (GCPs) for Map Correction
#'
#' @description A dataset containing Ground Control Points (GCPs) used for correcting
#'   a vector map. These points represent homologous locations identified on both
#'   the source (uncorrected) map and the target (reference) map.
#'
#' @format An `sf` object with 1106 features and 6 variables:
#' \describe{
#'   \item{source_x}{Numeric. The X-coordinate of the GCP on the source map.}
#'   \item{source_y}{Numeric. The Y-coordinate of the GCP on the source map.}
#'   \item{target_x}{Numeric. The X-coordinate of the GCP on the target map.}
#'   \item{target_y}{Numeric. The Y-coordinate of the GCP on the target map.}
#'   \item{dx}{Numeric. The difference in X-coordinates (target_x - source_x).}
#'   \item{dy}{Numeric. The difference in Y-coordinates (target_y - source_y).}
#'   \item{geometry}{`sfc_POINT`. The `sf` point geometry representing the GCPs
#'     in the target CRS (EPSG:2100).}
#' }
#' @source Generated from `data_raw/homologous.csv` using `read_gcps()` in
#'   `data_raw/create_kastoria_data.R`.
#' @examples
#' if (interactive()) {
#'   plot(sf::st_geometry(gcps))
#' }
"gcps"
