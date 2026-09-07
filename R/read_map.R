#' @title Read a Map (Vector or Raster) for Correction
#' @description Reads a vector map (e.g., shapefile, GeoPackage) or a raster
#'   map (e.g., GeoTIFF) that is intended to be corrected.
#' @details This function automatically detects whether the input file is a
#'   vector dataset or a raster dataset:
#'   \itemize{
#'     \item \strong{Vector maps} (e.g., `.shp`, `.gpkg`, `.geojson`): Read using
#'       `sf::st_read()`. If the file contains polygon geometries and lacks an
#'       `area_old` column, initial feature areas are calculated and added.
#'     \item \strong{Raster maps} (e.g., `.tif`, `.tiff`, `.geotiff`, `.asc`, `.grd`):
#'       Read using `terra::rast()`, returning a `terra::SpatRaster` object.
#'   }
#'
#' @param map_path A character string specifying the path to the input map file
#'   (e.g., a shapefile or GeoTIFF).
#' @param shp_path An alias for `map_path` kept for backward compatibility.
#' @param ... Arguments passed to `sf::st_read` (for vector maps) or `terra::rast`
#'   (for raster maps).
#'
#' @return An `sf` object (for vector maps) or a `terra::SpatRaster` object (for
#'   raster maps).
#'
#' @import sf
#' @importFrom terra rast
#' @importFrom tools file_ext
#' @importFrom units set_units
#' @export
#' @examples
#' \dontrun{
#' # First, create demo files (creates both vector and raster maps)
#' demo_files <- create_demo_data()
#'
#' # Read the vector map that needs correction
#' vector_map <- read_map(demo_files$shp_path, quiet = TRUE)
#'
#' # Read the raster map that needs correction
#' raster_map <- read_map(demo_files$raster_path)
#' print(raster_map)
#' }
read_map <- function(map_path, shp_path = NULL, ...) {
  # Support both map_path and legacy shp_path argument
  if (missing(map_path) && !is.null(shp_path)) {
    map_path <- shp_path
  }
  if (missing(map_path) || is.null(map_path)) {
    stop("`map_path` must be specified.", call. = FALSE)
  }

  # --- Input Validation ---
  if (!file.exists(map_path)) {
    stop("Map file not found at the specified path: ", map_path, call. = FALSE)
  }

  # --- Detect File Type ---
  ext <- tolower(tools::file_ext(map_path))
  raster_exts <- c("tif", "tiff", "geotiff", "png", "jpg", "jpeg", "asc", "grd", "img", "vrt")

  if (ext %in% raster_exts) {
    message(paste("Reading raster map from:", map_path))
    return(terra::rast(map_path, ...))
  }

  # --- Read Vector Data ---
  map_to_correct <- sf::st_read(map_path, ...)

  # --- Add Area for Polygons ---
  # Check if the map has polygon geometries and lacks an 'area_old' column
  if (
    any(sf::st_geometry_type(map_to_correct) %in% c("POLYGON", "MULTIPOLYGON")) &&
      !("area_old" %in% names(map_to_correct))) {
    message("Calculating area for polygon features...")
    map_to_correct$area_old <- sf::st_area(map_to_correct)
  }

  return(map_to_correct)
}
