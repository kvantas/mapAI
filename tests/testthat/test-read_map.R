test_that("read_map() works and handles errors", {
  temp_dir <- withr::local_tempdir()
  demo_files <- create_demo_data(output_dir = temp_dir)
  map_data <- read_map(shp_path = demo_files$shp_path)
  expect_s3_class(map_data, "sf")
  expect_error(read_map("non_existent_map.shp"), "Map file not found")
})

test_that("read_map() calculates 'area_old' for polygons", {
  temp_dir <- withr::local_tempdir()
  polygon_file <- create_test_polygon(file.path(temp_dir, "test_polygon.shp"))
  poly_map <- read_map(polygon_file)
  expect_true("area_old" %in% names(poly_map))
  expect_s3_class(poly_map$area_old, "units")
  expect_equal(as.numeric(poly_map$area_old[1]), 100)
})

test_that("read_map stops if shp_path does not exist", {
  expect_error(read_map("non_existent_file.shp"), "Map file not found")
})

test_that("read_map returns an sf object for a valid shapefile", {
  temp_dir <- withr::local_tempdir()
  shp_file <- file.path(temp_dir, "valid_map.shp")
  create_dummy_shp(shp_file)

  map <- read_map(shp_file)
  expect_s3_class(map, "sf")
  expect_true(file.exists(shp_file)) # Ensure the dummy file was created
})

test_that("read_map can pass arguments to st_read", {
  temp_dir <- withr::local_tempdir()
  shp_file <- file.path(temp_dir, "map.shp")
  create_dummy_shp(shp_file)

  expect_output(read_map(shp_file, quiet = FALSE), "Reading layer")
})

test_that("read_map calculates area_old for polygons without it", {
  temp_dir <- withr::local_tempdir()
  shp_file <- file.path(temp_dir, "poly_no_area.shp")
  create_dummy_shp(shp_file, geometry_type = "POLYGON", has_area_old = FALSE)

  map <- read_map(shp_file)
  expect_true("area_old" %in% names(map))
  expect_s3_class(map$area_old, "units")
})

test_that("read_map does not calculate area_old if it already exists", {
  temp_dir <- withr::local_tempdir()
  shp_file <- file.path(temp_dir, "poly_with_area.shp")
  create_dummy_shp(shp_file, geometry_type = "POLYGON", has_area_old = TRUE)

  map <- read_map(shp_file)
  expect_true("area_old" %in% names(map))
  # When reading from a shapefile, the 'units' class might be lost, but the numeric value should be preserved.
  # The function's logic is to not recalculate if the column exists.
  expect_equal(as.numeric(map$area_old), 100) # Check the value set in create_dummy_shp
  expect_type(map$area_old, "double") # It should be a numeric type after reading from shp
})

test_that("read_map does not calculate area_old for non-polygon geometries", {
  temp_dir <- withr::local_tempdir()
  shp_file <- file.path(temp_dir, "point_no_area.shp")
  create_dummy_shp(shp_file, geometry_type = "POINT")

  map <- read_map(shp_file)
  expect_false("area_old" %in% names(map))
})
