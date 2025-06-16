library(testthat)
library(sf)
library(units)

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

test_that("read_map stops if shp_path does not exist", {
  expect_error(read_map("non_existent_file.shp"), "Map file not found")
})

test_that("read_map returns an sf object for a valid shapefile", {
  temp_dir <- tempdir()
  shp_file <- file.path(temp_dir, "valid_map.shp")
  create_dummy_shp(shp_file)

  map <- read_map(shp_file)
  expect_s3_class(map, "sf")
  expect_true(file.exists(shp_file)) # Ensure the dummy file was created

  # Clean up
  file.remove(list.files(temp_dir, pattern = "valid_map.*", full.names = TRUE))
})

test_that("read_map assigns CRS if missing from file but provided by user", {
  temp_dir <- tempdir()
  shp_file <- file.path(temp_dir, "map_no_crs.shp")
  create_dummy_shp(shp_file, has_crs = FALSE)

  # this warning is expected
  suppressWarnings( map <- read_map(shp_file, crs = 32632) )
  expect_equal(sf::st_crs(map)$epsg, 32632)

  # Clean up
  file.remove(list.files(temp_dir, pattern = "map_no_crs.*", full.names = TRUE))
})

test_that("read_map warns if CRS is still missing", {
  temp_dir <- tempdir()
  shp_file <- file.path(temp_dir, "map_no_crs_no_param.shp")
  create_dummy_shp(shp_file, has_crs = FALSE)

  expect_warning(map <- read_map(shp_file), "Input map has no CRS and none was provided.")
  expect_true(is.na(sf::st_crs(map)$epsg))

  # Clean up
  file.remove(list.files(temp_dir, pattern = "map_no_crs_no_param.*", full.names = TRUE))
})

test_that("read_map calculates area_old for polygons without it", {
  temp_dir <- tempdir()
  shp_file <- file.path(temp_dir, "poly_no_area.shp")
  create_dummy_shp(shp_file, geometry_type = "POLYGON", has_area_old = FALSE)

  map <- read_map(shp_file)
  expect_true("area_old" %in% names(map))
  expect_s3_class(map$area_old, "units")

  # Clean up
  file.remove(list.files(temp_dir, pattern = "poly_no_area.*", full.names = TRUE))
})

test_that("read_map does not calculate area_old if it already exists", {
  temp_dir <- tempdir()
  shp_file <- file.path(temp_dir, "poly_with_area.shp")
  create_dummy_shp(shp_file, geometry_type = "POLYGON", has_area_old = TRUE)

  map <- read_map(shp_file)
  expect_true("area_old" %in% names(map))
  # When reading from a shapefile, the 'units' class might be lost, but the numeric value should be preserved.
  # The function's logic is to not recalculate if the column exists.
  expect_equal(as.numeric(map$area_old), 100) # Check the value set in create_dummy_shp
  expect_type(map$area_old, "double") # It should be a numeric type after reading from shp

  # Clean up
  file.remove(list.files(temp_dir, pattern = "poly_with_area.*", full.names = TRUE))
})

test_that("read_map does not calculate area_old for non-polygon geometries", {
  temp_dir <- tempdir()
  shp_file <- file.path(temp_dir, "point_no_area.shp")
  create_dummy_shp(shp_file, geometry_type = "POINT")

  map <- read_map(shp_file)
  expect_false("area_old" %in% names(map))

  # Clean up
  file.remove(list.files(temp_dir, pattern = "point_no_area.*", full.names = TRUE))
})
