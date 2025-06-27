#' @title Write a Spatial Object to a File
#' @description A robust and user-friendly wrapper around `sf::st_write` to save
#'   an `sf` object, for example, a corrected map.
#'
#' @details This function provides a straightforward way to save the output of
#' the `mapAI` workflow (or any `sf` object) to a file on disk. It supports any
#' file format that `sf::st_write` can handle. The function automatically infers
#' the correct driver from the file extension (e.g., `.shp` -> `"ESRI
#' Shapefile"`, `.gpkg` -> `"GPKG"`).
#'
#' For safety, the function defaults to `overwrite = FALSE`, which will prevent
#' accidentally overwriting an existing file. Advanced users can pass additional
#' arguments directly to `sf::st_write` via the `...` parameter.
#'
#' @param map An `sf` object to be written to a file.
#' @param file_path A character string specifying the path and filename for the
#'   output file (e.g., `"path/to/my_corrected_map.shp"`).
#' @param overwrite A logical value. If `TRUE`, it will overwrite an existing
#'   file at the specified path. Defaults to `FALSE`.
#' @param ... Additional arguments to be passed directly to `sf::st_write`
#'   (e.g., `layer_options`).
#'
#' @return Invisibly returns the input `map` object, allowing it to be used in a
#'   pipe chain.
#'
#' @import sf
#' @importFrom tools file_ext
#' @export
#' @examples
#' \dontrun{
#' # This example shows how to save data and ensure all created files are removed.
#'
#' # --- 1. Create a sample sf object to write ---
#' data(parcels)
#' sample_map <- parcels[1:5, ]
#'
#' # --- 2. Define a temporary file path ---
#' # Using tempfile() is best practice for examples.
#' output_path <- tempfile(fileext = ".shp")
#'
#' # --- 3. Write the map to the file ---
#' write_map(sample_map, output_path, overwrite = TRUE)
#'
#' # --- 4. Clean up ALL created files ---
#' # A shapefile creates multiple "sidecar" files (.dbf, .shx, .prj, etc.).
#' # This code finds all files with the same base name and removes them.
#' base_name <- tools::file_path_sans_ext(output_path)
#' files_to_remove <- list.files(dirname(base_name),
#'                               pattern = basename(base_name),
#'                               full.names = TRUE)
#' file.remove(files_to_remove)
#'
#' # Check that the files are gone
#' print(list.files(dirname(output_path), pattern = basename(base_name)))
#' }
write_map <- function(map, file_path, overwrite = FALSE, ...) {

  # --- Input Validation ---
  if (!inherits(map, "sf")) stop("`map` must be a valid `sf` object.", call. = FALSE)
  if (!is.character(file_path) || length(file_path) != 1) {
    stop("`file_path` must be a single character string.", call. = FALSE)
  }

  # --- Robust Driver Detection ---
  ext <- tolower(tools::file_ext(file_path))
  driver <- switch(ext,
                   "shp" = "ESRI Shapefile",
                   "gpkg" = "GPKG",
                   "geojson" = "GeoJSON",
                   "json" = "GeoJSON",
                   NULL # Let sf::st_write handle other cases or throw an error
  )
  if (is.null(driver)) {
    warning(paste("Could not automatically determine driver for extension '.", ext, "'. Letting sf::st_write attempt to guess.", sep=""), call. = FALSE)
  }

  message(paste("Writing map to:", file_path))

  # Call the core sf::st_write function, passing along all arguments
  sf::st_write(
    obj = map,
    dsn = file_path,
    driver = driver,
    delete_layer = overwrite,
    quiet = TRUE, # Suppress verbose GDAL messages by default
    ...          # Pass along any extra arguments
  )

  message("Map successfully written.")
  invisible(map)
}
