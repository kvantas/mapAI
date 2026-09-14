#' @title Write a Spatial Object (Vector or Raster) to a File
#' @description A robust and user-friendly wrapper around `sf::st_write` (for
#'   vector maps) and `terra::writeRaster` (for raster maps) to save spatial
#'   objects to disk.
#'
#' @details This function provides a straightforward way to save the output of
#' the `mapAI` workflow (or any `sf` or `terra::SpatRaster` object) to a file on
#' disk.
#'
#' For vector data (`sf`), it supports any file format that `sf::st_write` can
#' handle and automatically infers the driver from the file extension
#' (e.g., `.shp` -> `"ESRI Shapefile"`, `.gpkg` -> `"GPKG"`, `.geojson` -> `"GeoJSON"`).
#'
#' For raster data (`terra::SpatRaster`), it writes the raster using
#' `terra::writeRaster`, supporting formats such as GeoTIFF (`.tif`),
#' GPKG raster (`.gpkg`), and others.
#'
#' For safety, the function defaults to `overwrite = FALSE`, which will prevent
#' accidentally overwriting an existing file. Advanced users can pass additional
#' arguments directly to the underlying writing functions (`sf::st_write` or
#' `terra::writeRaster`) via the `...` parameter.
#'
#' @param map An `sf` object or a `terra` `SpatRaster` object to be written to a file.
#' @param file_path A character string specifying the path and filename for the
#'   output file (e.g., `"path/to/my_corrected_map.shp"` or `"path/to/map.tif"`).
#' @param overwrite A logical value. If `TRUE`, it will overwrite an existing
#'   file at the specified path. Defaults to `FALSE`.
#' @param ... Additional arguments to be passed directly to `sf::st_write` or
#'   `terra::writeRaster`.
#'
#' @return Invisibly returns the input `map` object, allowing it to be used in a
#'   pipe chain.
#'
#' @importFrom sf st_write
#' @importFrom terra writeRaster
#' @importFrom tools file_ext
#' @export
#' @examples
#' \dontrun{
#' # --- 1. Write an sf vector object ---
#' library(sf)
#' sample_map <- st_as_sf(data.frame(
#'  id = 1:3,
#'  wkt = c("POINT(1 1)", "POINT(2 2)", "POINT(3 3)")
#'  ), wkt = "wkt", crs = 4326)
#' output_path <- tempfile(fileext = ".shp")
#' write_map(sample_map, output_path, overwrite = TRUE)
#'
#' # --- 2. Write a SpatRaster object ---
#' library(terra)
#' r <- rast(nrows = 10, ncols = 10, vals = 1:100)
#' output_tif <- tempfile(fileext = ".tif")
#' write_map(r, output_tif, overwrite = TRUE)
#' }
write_map <- function(map, file_path, overwrite = FALSE, ...) {

  # validate input values
  validate_write_map(map, file_path, overwrite)

  message(paste("Writing map to:", file_path))

  # --- Handle SpatRaster ---
  if (inherits(map, "SpatRaster")) {
    if (!overwrite && file.exists(file_path)) {
      stop("File already exists: ", file_path, ". Use `overwrite = TRUE` to overwrite.", call. = FALSE)
    }

    terra::writeRaster(
      x = map,
      filename = file_path,
      overwrite = overwrite,
      ...
    )
  } else {
    # --- Robust Driver Detection for vector maps ---
    ext <- tolower(tools::file_ext(file_path))
    driver <- switch(ext,
                     "shp" = "ESRI Shapefile",
                     "gpkg" = "GPKG",
                     "geojson" = "GeoJSON",
                     "json" = "GeoJSON",
                     NULL # Let sf::st_write handle other cases or throw an error
    )
    if (is.null(driver)) {
      warning(paste("Could not automatically determine driver for extension '.",
                    ext,
                    "'. Letting sf::st_write attempt to guess.", sep=""),
              call. = FALSE)
    }

    # Call the core sf::st_write function, passing along all arguments
    sf::st_write(
      obj = map,
      dsn = file_path,
      driver = driver,
      delete_layer = overwrite,
      quiet = TRUE, # Suppress verbose GDAL messages by default
      ...          # Pass along any extra arguments
    )
  }

  message("Map successfully written.")
  invisible(map)
}
