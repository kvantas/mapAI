library(testthat)
library(sf)
library(dplyr)
library(mapAI) # Assuming mapAI package is loaded for read_data function

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
create_dummy_gcp_csv <- function(temp_dir, valid = TRUE) {
  if (valid) {
    gcp_data <- data.frame(
      source_x = c(10, 11, 12),
      source_y = c(20, 21, 22),
      target_x = c(10.1, 11.2, 12.3),
      target_y = c(20.1, 21.2, 22.3)
    )
  } else {
    # Invalid GCP data (missing target_y)
    gcp_data <- data.frame(
      source_x = c(10, 11, 12),
      source_y = c(20, 21, 22),
      target_x = c(10.1, 11.2, 12.3)
    )
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
