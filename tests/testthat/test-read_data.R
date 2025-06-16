library(testthat)
library(sf)
library(dplyr)
library(mapAI) # Assuming mapAI package is loaded for read_data function

# Source the functions directly for testing
source("../../R/read_data.R")

# Helper function to create a dummy shapefile for testing
create_dummy_shapefile <- function(temp_dir) {
  # Create a simple point feature
  point <- st_point(c(10, 20))
  sfc <- st_sfc(point) # No CRS initially
  sf_obj <- st_sf(data.frame(id = 1), geometry = sfc)

  # Define shapefile path
  shp_path <- file.path(temp_dir, "dummy_map.shp")

  # Write shapefile
  st_write(sf_obj, shp_path, quiet = TRUE, delete_layer = TRUE)
  return(shp_path)
}

# Helper function to create a dummy polygon shapefile for testing
create_dummy_polygon_shapefile <- function(temp_dir) {
  # Create a simple square polygon
  polygon_coords <- matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE)
  polygon_sfc <- st_polygon(list(polygon_coords))
  sf_obj <- st_sf(data.frame(id = 1), geometry = st_sfc(polygon_sfc, crs = 4326)) # WGS 84

  # Define shapefile path
  shp_path <- file.path(temp_dir, "dummy_polygon_map.shp")

  # Write shapefile
  st_write(sf_obj, shp_path, quiet = TRUE, delete_layer = TRUE)
  return(shp_path)
}

# Helper function to create a dummy GCP CSV for testing
create_dummy_gcp_csv <- function(temp_dir, valid = TRUE, missing_col = NULL) {
  if (valid) {
    gcp_data <- data.frame(
      source_x = c(10, 11, 12),
      source_y = c(20, 21, 22),
      target_x = c(10.1, 11.2, 12.3),
      target_y = c(20.1, 21.2, 22.3)
    )
  } else {
    gcp_data <- data.frame(
      source_x = c(10, 11, 12),
      source_y = c(20, 21, 22),
      target_x = c(10.1, 11.2, 12.3)
    )
    if (!is.null(missing_col)) {
      gcp_data[[missing_col]] <- NULL
    }
  }
  csv_path <- file.path(temp_dir, "dummy_gcp.csv")
  write.csv(gcp_data, csv_path, row.names = FALSE)
  return(csv_path)
}

test_that("read_data loads valid data and returns pai_data object", {
  # Setup temporary directory and files
  temp_dir <- tempdir()
  shp_path <- create_dummy_shapefile(temp_dir)
  gcp_path <- create_dummy_gcp_csv(temp_dir)

  # Test with valid inputs
  suppressWarnings(
    result <- read_data(shp_path, gcp_path)
    )

  # Assertions
  expect_s3_class(result, "pai_data")
  expect_true("map_to_correct" %in% names(result))
  expect_true("gcp_data" %in% names(result))
  expect_s3_class(result$map_to_correct, "sf")
  expect_s3_class(result$gcp_data, "sf")
  expect_true("dx" %in% names(result$gcp_data))
  expect_true("dy" %in% names(result$gcp_data))

  # Verify dx and dy calculations
  expect_equal(result$gcp_data$dx, c(0.1, 0.2, 0.3))
  expect_equal(result$gcp_data$dy, c(0.1, 0.2, 0.3))

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("read_data adds area for polygon shapefiles", {
  temp_dir <- tempdir()
  shp_path <- create_dummy_polygon_shapefile(temp_dir)
  gcp_path <- create_dummy_gcp_csv(temp_dir)

  result <- read_data(shp_path, gcp_path)

  # Assertions
  expect_true("area_old" %in% names(result$map_to_correct))
  expect_s3_class(result$map_to_correct$area, "units")
  # The area of a 1x1 square in degrees (WGS84) is not 1, but a small value.
  # We can check if it's a positive value and not NA.
  expect_true(as.numeric(result$map_to_correct$area) > 0)
  expect_false(is.na(as.numeric(result$map_to_correct$area)))

  unlink(temp_dir, recursive = TRUE)
})

test_that("read_data handles non-existent shapefile path", {
  temp_dir <- tempdir()
  gcp_path <- create_dummy_gcp_csv(temp_dir)
  non_existent_shp <- file.path(temp_dir, "non_existent.shp")

  expect_error(read_data(non_existent_shp, gcp_path), "Shapefile not found")

  unlink(temp_dir, recursive = TRUE)
})

test_that("read_data handles non-existent GCP file path", {
  temp_dir <- tempdir()
  shp_path <- create_dummy_shapefile(temp_dir)
  non_existent_gcp <- file.path(temp_dir, "non_existent.csv")

  expect_error(read_data(shp_path, non_existent_gcp), "GCP file not found")

  unlink(temp_dir, recursive = TRUE)
})

test_that("read_data handles invalid GCP CSV (missing columns)", {
  temp_dir <- tempdir()
  shp_path <- create_dummy_shapefile(temp_dir)
  invalid_gcp_path <- create_dummy_gcp_csv(temp_dir, valid = FALSE)

  expect_error(read_data(shp_path, invalid_gcp_path), "GCP file must contain columns")

  unlink(temp_dir, recursive = TRUE)
})

test_that("read_data assigns CRS correctly when provided", {
  temp_dir <- tempdir()
  shp_path <- create_dummy_shapefile(temp_dir)
  gcp_path <- create_dummy_gcp_csv(temp_dir)

  # Test with CRS provided
  suppressWarnings(
    # WGS 84 / UTM zone 32N
    result_with_crs <- read_data(shp_path, gcp_path, crs = 32632)
  )
  expect_equal(sf::st_crs(result_with_crs$map_to_correct)$epsg, 32632)
  expect_equal(sf::st_crs(result_with_crs$gcp_data)$epsg, 32632)

  unlink(temp_dir, recursive = TRUE)
})

test_that("read_data issues warning when no CRS and none provided", {
  temp_dir <- tempdir()
  # Create a shapefile without CRS initially, ensuring it truly has no CRS
  point <- st_point(c(10, 20))
  sfc <- st_sfc(point) # No CRS
  sf_obj <- st_sf(data.frame(id = 1), geometry = sfc)
  shp_path_no_crs <- file.path(temp_dir, "dummy_map_no_crs.shp")
  # Explicitly set CRS to NA before writing to ensure it's truly missing
  sf::st_crs(sf_obj) <- NA
  st_write(sf_obj, shp_path_no_crs, quiet = TRUE, delete_layer = TRUE)

  gcp_path <- create_dummy_gcp_csv(temp_dir)

  expect_warning(read_data(shp_path_no_crs, gcp_path), "Input shapefile has no CRS and none was provided.")

  unlink(temp_dir, recursive = TRUE)
})

# --- New tests for read_gcps function ---
test_that("read_gcps reads valid CSV and calculates dx/dy correctly", {
  temp_dir <- tempdir()
  gcp_path <- create_dummy_gcp_csv(temp_dir)
  crs_val <- 3857 # Example CRS

  gcp_sf <- read_gcps(gcp_path = gcp_path, crs = crs_val)

  expect_s3_class(gcp_sf, "sf")
  expect_true("dx" %in% names(gcp_sf))
  expect_true("dy" %in% names(gcp_sf))
  expect_equal(gcp_sf$dx, c(0.1, 0.2, 0.3))
  expect_equal(gcp_sf$dy, c(0.1, 0.2, 0.3))
  expect_equal(sf::st_crs(gcp_sf)$epsg, crs_val)

  unlink(temp_dir, recursive = TRUE)
})

test_that("read_gcps throws error for non-existent file", {
  temp_dir <- tempdir()
  non_existent_path <- file.path(temp_dir, "non_existent_gcp.csv")
  expect_error(read_gcps(gcp_path = non_existent_path, crs = 3857), "GCP file not found")
  unlink(temp_dir, recursive = TRUE)
})

test_that("read_gcps throws error for missing or NA CRS", {
  temp_dir <- tempdir()
  gcp_path <- create_dummy_gcp_csv(temp_dir)

  expect_error(read_gcps(gcp_path = gcp_path), "A coordinate reference system \\(CRS\\) must be provided for the GCPs.")
  expect_error(read_gcps(gcp_path = gcp_path, crs = NA), "A coordinate reference system \\(CRS\\) must be provided for the GCPs.")

  unlink(temp_dir, recursive = TRUE)
})

test_that("read_gcps throws error for missing required columns", {
  temp_dir <- tempdir()
  # Test with missing source_x
  gcp_path_missing_sx <- create_dummy_gcp_csv(temp_dir, valid = FALSE, missing_col = "source_x")
  expect_error(read_gcps(gcp_path = gcp_path_missing_sx, crs = 3857), "GCP file must contain columns: source_x, source_y, target_x, target_y")

  # Test with missing target_y (already handled by valid = FALSE)
  gcp_path_missing_ty <- create_dummy_gcp_csv(temp_dir, valid = FALSE)
  expect_error(read_gcps(gcp_path = gcp_path_missing_ty, crs = 3857), "GCP file must contain columns: source_x, source_y, target_x, target_y")

  unlink(temp_dir, recursive = TRUE)
})

test_that("read_gcps throws error for invalid CSV format", {
  temp_dir <- tempdir()
  invalid_csv_path <- file.path(temp_dir, "invalid.txt")
  writeLines("This is not a CSV file", invalid_csv_path)
  # Adjusted expectation: the column validation catches it first
  expect_error(read_gcps(gcp_path = invalid_csv_path, crs = 3857), "GCP file must contain columns: source_x, source_y, target_x, target_y")
  unlink(temp_dir, recursive = TRUE)
})

# --- New tests for read_map function ---
# Helper function to create a dummy shapefile for testing (modified to explicitly set CRS to NA)
create_dummy_shapefile <- function(temp_dir) {
  # Create a simple point feature
  point <- st_point(c(10, 20))
  sfc <- st_sfc(point) # No CRS initially
  sf_obj <- st_sf(data.frame(id = 1), geometry = sfc)
  sf::st_crs(sf_obj) <- NA # Explicitly set CRS to NA

  # Define shapefile path
  shp_path <- file.path(temp_dir, "dummy_map.shp")

  # Write shapefile
  st_write(sf_obj, shp_path, quiet = TRUE, delete_layer = TRUE)
  return(shp_path)
}

test_that("read_map reads valid shapefile (point) correctly", {
  temp_dir <- tempdir()
  shp_path <- create_dummy_shapefile(temp_dir)

  map_sf <- read_map(shp_path = shp_path)

  expect_s3_class(map_sf, "sf")
  expect_equal(as.character(sf::st_geometry_type(map_sf)[1]), "POINT")
  # No CRS expected initially from create_dummy_shapefile
  expect_true(is.na(sf::st_crs(map_sf)$epsg))

  unlink(temp_dir, recursive = TRUE)
})

test_that("read_map reads valid shapefile (polygon) and calculates area", {
  temp_dir <- tempdir()
  shp_path <- create_dummy_polygon_shapefile(temp_dir)

  map_sf <- read_map(shp_path = shp_path)

  expect_s3_class(map_sf, "sf")
  expect_equal(as.character(sf::st_geometry_type(map_sf)[1]), "POLYGON")
  expect_true("area_old" %in% names(map_sf))
  expect_s3_class(map_sf$area_old, "units")
  expect_true(as.numeric(map_sf$area_old) > 0) # Area should be positive

  unlink(temp_dir, recursive = TRUE)
})

test_that("read_map throws error for non-existent file", {
  temp_dir <- tempdir()
  non_existent_path <- file.path(temp_dir, "non_existent_map.shp")
  expect_error(read_map(shp_path = non_existent_path), "Map file not found")
  unlink(temp_dir, recursive = TRUE)
})

test_that("read_map assigns CRS when input lacks one but provided", {
  temp_dir <- tempdir()
  shp_path_no_crs <- create_dummy_shapefile(temp_dir) # This creates a shapefile without CRS
  crs_val <- 32632 # WGS 84 / UTM zone 32N

  map_sf <- read_map(shp_path = shp_path_no_crs, crs = crs_val)

  expect_equal(sf::st_crs(map_sf)$epsg, crs_val)

  unlink(temp_dir, recursive = TRUE)
})

test_that("read_map issues warning when no CRS and none provided", {
  temp_dir <- tempdir()
  shp_path_no_crs <- create_dummy_shapefile(temp_dir) # This creates a shapefile without CRS

  expect_warning(read_map(shp_path = shp_path_no_crs), "Input map has no CRS and none was provided.")

  unlink(temp_dir, recursive = TRUE)
})
