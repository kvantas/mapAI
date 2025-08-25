# This helper script creates temporary data for tests.
# It will be sourced automatically by testthat.

library(testthat)
library(sf)
library(utils)

# Helper function to create a dummy shapefile for testing
create_dummy_shp <- function(file_path,
                             geometry_type = "POLYGON",
                             has_crs = TRUE,
                             has_area_old = FALSE) {
  if (geometry_type == "POLYGON") {
    # Create a simple polygon
    poly <- st_polygon(list(cbind(c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0))))
    sfc <- st_sfc(poly)
  } else if (geometry_type == "POINT") {
    # Create a simple point
    point <- st_point(c(0, 0))
    sfc <- st_sfc(point)
  } else {
    stop("Unsupported geometry type for dummy shapefile.")
  }

  if (has_crs) {
    st_crs(sfc) <- 4326 # WGS 84
  }

  df <- data.frame(id = 1)
  if (has_area_old) {
    df$area_old <- set_units(1, "m^2")
  }

  sf_obj <- st_sf(df, geometry = sfc)

  # Ensure the directory exists
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)

  st_write(sf_obj, file_path, delete_layer = TRUE, quiet = TRUE)
  return(file_path)
}


# Helper function to create dummy gcp_data
create_dummy_gcp_data <- function(n = 200) {
  set.seed(123) # for reproducibility of dummy data
  df <- data.frame(
    source_x = runif(n, 0, 1000),
    source_y = runif(n, 0, 1000)
  )

  # make target coordinates a simple function of sources with noise
  df$target_x <- 1.05 * df$source_x + 0.0005 * df$source_x^2  + runif(n, 0, 1)
  df$target_y <- 1.25 * df$source_y - 0.00006 * df$source_y^2  + runif(n, 0, 1)

  # compute displacements
  df$dx <- df$target_x - df$source_x
  df$dy <- df$target_y - df$source_y

  class(df) <- c("gcp", "data.frame")

  return(df)

}

# Function to create a simple square polygon for area tests
create_test_polygon <- function(path) {
  poly_coords <- matrix(c(0,0, 0,10, 10,10, 10,0, 0,0), ncol = 2, byrow = TRUE)
  poly <- sf::st_polygon(list(poly_coords))
  poly_sf <- sf::st_sf(id = 1, geometry = sf::st_sfc(poly, crs = 3857))
  sf::st_write(poly_sf, path, delete_layer = TRUE, quiet = TRUE)
  return(path)
}
