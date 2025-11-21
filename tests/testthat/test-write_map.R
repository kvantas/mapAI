# This file contains tests for the write_map() utility function.

# --- Setup: Get some sample data to write ---

test_that("write_map creates a valid file that can be read back", {

  # create a simple sf object for testing
  sample_map <- st_as_sf(data.frame(
   id = 1:3,
   wkt = c("POINT(1 1)", "POINT(2 2)", "POINT(3 3)")
   ), wkt = "wkt", crs = 4326)

  # write the file
  output_path <- withr::local_tempdir(fileext = ".shp")
  expect_no_error(write_map(sample_map, output_path, overwrite = TRUE))


  # ASSERT:
  expect_true(file.exists(output_path))

  read_back_map <- sf::st_read(output_path, quiet = TRUE)

  expect_s3_class(read_back_map, "sf")
  expect_equal(nrow(read_back_map), nrow(sample_map))

  expect_true(sf::st_crs(read_back_map) == sf::st_crs(sample_map))
  read_back_df <- sf::st_drop_geometry(read_back_map)

  # We must explicitly drop units from the original data for a fair comparison.
  parcels_no_units <- units::drop_units(sf::st_drop_geometry(sample_map))
  expect_equal(read_back_df, parcels_no_units)

  # 3.3. Test that the geometries themselves are identical.
  expect_identical(
    sf::st_as_text(sf::st_geometry(read_back_map)),
    sf::st_as_text(sf::st_geometry(sample_map))
  )
})

test_that("overwrite argument works as expected", {

  # create a simple sf object for testing
  sample_map <- st_as_sf(data.frame(
    id = 1:3,
    wkt = c("POINT(1 1)", "POINT(2 2)", "POINT(3 3)")
  ), wkt = "wkt", crs = 4326)

  temp_dir <- withr::local_tempdir()
  temp_gpkg_path <- file.path(temp_dir, "parcels.gpkg")

  write_map(sample_map, temp_gpkg_path)
  expect_error(write_map(sample_map, temp_gpkg_path))
  expect_no_error(write_map(sample_map, temp_gpkg_path, overwrite = TRUE))
  read_back_map <- sf::st_read(temp_gpkg_path, quiet = TRUE)
  expect_equal(nrow(read_back_map), nrow(sample_map))
})

test_that("write_map handles invalid inputs gracefully", {

  sample_map <- st_as_sf(data.frame(
    id = 1:3,
    wkt = c("POINT(1 1)", "POINT(2 2)", "POINT(3 3)")
  ), wkt = "wkt", crs = 4326)

  temp_dir <- withr::local_tempdir()
  temp_path <- file.path(temp_dir, "parcels.gpkg")
  expect_error(write_map(as.data.frame(sample_map), temp_path, overwrite = "a"),
               "`map` must be a valid `sf` object.")
  expect_error(write_map(as.data.frame(sample_map), temp_path),
               "`map` must be a valid `sf` object.")
  expect_error(write_map(sample_map, file_path = 12345),
               "`file_path` must be a single character string.")
  temp_path_xxx <- file.path(temp_dir, "parcels.xxx")
  expect_error(suppressWarnings( write_map(sample_map, temp_path_xxx)) )
})
