#' @title Read and Prepare Homologous Points (GCPs)
#' @description Reads a CSV file containing homologous points and prepares them
#'   for modeling by calculating displacement vectors and creating an `sf`
#'   object.
#' @details This function is the first step in the modeling workflow. It
#'   requires a CSV file with four specific columns: `source_x` and `source_y`
#'   (coordinates from the old/distorted map) and `target_x` and `target_y`
#'   (coordinates from the reference/true map). It calculates the `dx` and `dy`
#'   errors that the models will learn to predict.
#'
#' @param gcp_path A character string specifying the path to the CSV file of
#'   homologous points.
#' @param crs The coordinate reference system (CRS) of the `source_x` and
#'   `source_y` coordinates. This is a **required** parameter to ensure spatial
#'   context. It can be an EPSG code (e.g., `32632`) or a WKT string.
#'
#' @return An `sf` object of the homologous points, with point geometries based
#'   on the source coordinates and calculated `dx` and `dy` displacement
#'   columns.
#'
#' @import sf
#' @import dplyr
#' @importFrom utils read.csv
#' @importFrom rlang .data
#' @export
#' @examples
#' \dontrun{
#' # First, create a demo CSV file
#' demo_files <- create_demo_data()
#'
#' # Read the GCPs, providing the CRS for the source coordinates
#' # (The demo data uses a placeholder CRS of 3857)
#' gcp_data <- read_gcps(gcp_path = demo_files$gcp_path, crs = 3857)
#'
#' # The output is now ready for model training
#' print(gcp_data)
#' }
read_gcps <- function(gcp_path, crs) {

  # --- Input Validation ---
  if (!file.exists(gcp_path)) {
    stop("GCP file not found at the specified path: ",
         gcp_path)
  }
  if (missing(crs) || is.na(crs)) {
    stop("A coordinate reference system (CRS) must be provided for the GCPs.",
         call. = FALSE)
  }

  # --- Read and Validate Data ---
  gcp_df <- tryCatch(
    utils::read.csv(gcp_path),
    error = function(e) {
      stop("Failed to read GCP file. Please ensure it is a valid CSV. Error: ",
           conditionMessage(e))
    }
  )

  required_cols <- c("source_x", "source_y", "target_x", "target_y")
  if (!all(required_cols %in% names(gcp_df))) {
    stop("GCP file must contain columns: ",
         paste(required_cols, collapse = ", "))
  }

  # --- Prepare GCP data ---
  gcp_sf <- gcp_df %>%
    dplyr::mutate(
      dx = .data$target_x - .data$source_x,
      dy = .data$target_y - .data$source_y
    ) %>%
    sf::st_as_sf(coords = c("source_x", "source_y"), crs = crs, remove = FALSE)

  return(gcp_sf)
}
