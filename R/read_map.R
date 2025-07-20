#' @title Read a Vector Map for Correction
#' @description Reads a vector map (e.g., shapefile) that is intended to be
#'   corrected.
#' @details This function reads the geospatial file using `sf::st_read`. If the
#'   file contains polygon geometries and does not already have an area column
#'   named 'area_old', this function will calculate the area of each feature and
#'   add it.
#'
#' @param shp_path A character string specifying the path to the input map file
#'   (e.g., a shapefile).
#' @param ... Arguments passed to `sf::st_read`
#'
#' @return An `sf` object of the map to be corrected.
#'
#' @import sf
#' @importFrom units set_units
#' @export
#' @examples
#' \dontrun{
#' # First, create a demo shapefile
#' demo_files <- create_demo_data()
#'
#' # Read the map that needs correction
#' map_to_correct <- read_map(shp_path = demo_files$shp_path, quiet = TRUE)
#'
#' # The output is now ready to be used in the `correct_map()` function
#' # after a model has been trained.
#' plot(sf::st_geometry(map_to_correct))
#' }
read_map <- function(shp_path, ...) {
  # --- Input Validation ---
  if (!file.exists(shp_path)) {
    stop("Map file not found at the specified path: ", shp_path)
  }

  # --- Read Data ---
  map_to_correct <- sf::st_read(shp_path, ...)

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
