#' @title Vector Map Parcels for Correction
#'
#' @description A dataset containing vector map parcels (e.g., from a cadastral map)
#'   that are intended to be corrected using geospatial transformation methods.
#'
#' @format An `sf` object with 493 parcel polygon geometries and several variables:
#' \describe{
#'   \item{KAK}{Numeric. A unique identifier for each parcel.}
#'   \item{area_old}{`units` object. The original area of each polygon feature,
#'     calculated by `read_map()`}
#'   \item{geometry}{`sfc_POLYGON` or `sfc_MULTIPOLYGON`. The `sf` polygon geometry
#'     representing the parcels in the target CRS (EPSG:2100).}
#' }
#' @source Generated from `data_raw/cad1925.shp` using `read_map()` in
#'   `data_raw/create_kastoria_data.R`.
#' @examples
#' if (interactive()) {
#'   plot(sf::st_geometry(parcels))
#' }
"parcels"
