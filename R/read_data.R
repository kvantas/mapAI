#' @title Load and Prepare Geospatial Data for Correction
#' @description Reads an uncorrected vector map and a set of homologous points,
#'   then prepares the data for positional accuracy improvement modeling.
#' @details This function takes a path to a shapefile and a path to a CSV file
#'   containing homologous points. The CSV must contain four columns:
#'   `source_x`, `source_y`, `target_x`, and `target_y`. It calculates the
#'   displacement vectors (dx, dy) which become the target variables for the models.
#' @param shp_path A character string specifying the path to the input shapefile.
#' @param gcp_path A character string specifying the path to the CSV file of homologous points.
#' @param crs The coordinate reference system (CRS) to assign if the input shapefile lacks one. Can be an EPSG code (e.g., `32632`) or a WKT string.
#' @return A list object of class `pai_data` containing:
#'   \item{map_to_correct}{An `sf` object of the original map.}
#'   \item{gcp_data}{An `sf` object of the homologous points with `dx` and `dy` displacements.}
#' @import sf
#' @import dplyr
#' @importFrom utils read.csv
#' @importFrom rlang .data
#' @export
read_data <- function(shp_path, gcp_path, crs = NA) {

  # --- Input Validation ---
  if (!file.exists(shp_path)) stop("Shapefile not found: ", shp_path)
  if (!file.exists(gcp_path)) stop("GCP file not found: ", gcp_path)

  # --- Read Data ---
  map_to_correct <- sf::st_read(shp_path, quiet = TRUE)
  gcp_df <- read.csv(gcp_path)

  # --- Validate GCP columns ---
  required_cols <- c("source_x", "source_y", "target_x", "target_y")
  if (!all(required_cols %in% names(gcp_df))) {
    stop("GCP file must contain columns: ", paste(required_cols, collapse = ", "))
  }

  # --- CRS Handling ---
  # Check if map_to_correct has no CRS and a CRS is provided
  if (is.na(sf::st_crs(map_to_correct)$epsg) && !is.na(crs)) {
    map_to_correct <- sf::st_set_crs(map_to_correct, crs)
  }

  # Determine the final CRS for gcp_data
  final_crs <- sf::st_crs(map_to_correct)

  # If map_to_correct still has no CRS after attempts, issue a warning
  if (is.na(final_crs$epsg)) {
    warning("Input shapefile has no CRS and none was provided. This may cause issues with projection-aware tools.", call. = FALSE)
  }

  # --- Prepare GCP data ---
  gcp_data <- gcp_df %>%
    dplyr::mutate(
      dx = .data$target_x - .data$source_x,
      dy = .data$target_y - .data$source_y
    ) %>%
    sf::st_as_sf(coords = c("source_x", "source_y"), crs = final_crs, remove = FALSE)

  # --- Create custom class for output ---
  output <- list(map_to_correct = map_to_correct, gcp_data = gcp_data)
  class(output) <- "pai_data"

  return(output)
}
