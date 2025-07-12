# This helper script creates temporary data for tests.
# It will be sourced automatically by testthat.

library(testthat)
library(sf)
library(dplyr)
library(utils)
library(mockery)

# Helper function to create a dummy shapefile for testing
create_dummy_shp <- function(file_path, geometry_type = "POLYGON", has_crs = TRUE, has_area_old = FALSE) {
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
    df$area_old <- set_units(100, "m^2")
  }

  sf_obj <- st_sf(df, geometry = sfc)

  # Ensure the directory exists
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)

  st_write(sf_obj, file_path, delete_layer = TRUE, quiet = TRUE)
  return(file_path)
}

# Helper function to create a dummy GCP CSV file
create_dummy_gcp_csv <- function(path, data = NULL) {
  if (is.null(data)) {
    data <- data.frame(
      source_x = c(10, 20, 30),
      source_y = c(100, 110, 120),
      target_x = c(12, 23, 35),
      target_y = c(101, 112, 124)
    )
  }
  utils::write.csv(data, path, row.names = FALSE)
}

# Helper function to create dummy gcp_data
create_dummy_gcp_data <- function(n = 500) {
  set.seed(123) # for reproducibility of dummy data
  data.frame(
    source_x = runif(n, 0, 1000), # Increased range for source_x and source_y
    source_y = runif(n, 0, 1000),
    dx = rnorm(n, 0, 5), # Increased variance for dx and dy
    dy = rnorm(n, 0, 5)
  ) %>%
    sf::st_as_sf(coords = c("source_x", "source_y"), crs = 4326, remove = FALSE)
}

# Function to create a simple square polygon for area tests
create_test_polygon <- function(path) {
  poly_coords <- matrix(c(0,0, 0,10, 10,10, 10,0, 0,0), ncol = 2, byrow = TRUE)
  poly <- sf::st_polygon(list(poly_coords))
  poly_sf <- sf::st_sf(id = 1, geometry = sf::st_sfc(poly, crs = 3857))
  sf::st_write(poly_sf, path, delete_layer = TRUE, quiet = TRUE)
  return(path)
}
