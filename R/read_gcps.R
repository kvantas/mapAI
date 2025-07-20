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
#' @param gcp_path A character string specifying the path with the file name
#'   to the CSV file of homologous points.
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
#' # Read the GCPs
#' gcp_data <- read_gcps(gcp_path = demo_files$gcp_path)
#'
#' # The output is now ready for model training
#' print(gcp_data)
#' }
read_gcps <- function(gcp_path) {

  # --- Input Validation ---
  if (!file.exists(gcp_path)) {
    stop("GCP file not found at the specified path: ",
         gcp_path)
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
    sf::st_as_sf(coords = c("source_x", "source_y"), remove = FALSE)

  return(gcp_sf)
}
