#' @title Ground Control Points (GCPs) for Map Correction
#'
#' @description A dataset containing Ground Control Points (GCPs) used for
#'   correcting the vector map parcels of the package. These points represent
#'   homologous locations identified on both the source (uncorrected) map and a
#'   modern land survey.
#'
#' @format An `sf` object with 300 features and 6 variables:
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
#' @source Part of the dataset is used in the paper
#'    https://doi.org/10.1111/tgis.70076
#' @examples
#' if (interactive()) {
#'   # plot the difference in X-coordinates dx
#'   plot((gcps["dx"]))
#' }
"gcps"
