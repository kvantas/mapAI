# This file contains tests for the write_map() utility function.

# --- Setup: Get some sample data to write ---
data(parcels)

test_that("write_map creates a valid file that can be read back", {
  # This test relies on the 'units' package to fairly compare data frames.
  # If 'units' is not installed, the test is safely skipped.
  testthat::skip_if_not_installed("units")

  temp_dir <- withr::local_tempdir()
  # 1. ARRANGE: Define a temporary path for a GeoPackage.
  temp_gpkg_path <- file.path(temp_dir, "parcels.gpkg")

  # 2. ACT: Write the file.
  expect_no_error(
    write_map(parcels, temp_gpkg_path, overwrite = TRUE)
  )

  # 3. ASSERT:
  expect_true(file.exists(temp_gpkg_path))
  read_back_map <- sf::st_read(temp_gpkg_path, quiet = TRUE)
  expect_s3_class(read_back_map, "sf")
  expect_equal(nrow(read_back_map), nrow(parcels))

  # 3.1. Test that the projections are functionally equivalent.
  expect_true(sf::st_crs(read_back_map) == sf::st_crs(parcels))

  # 3.2. Test that the non-spatial data attributes are identical.

  # CRITICAL FIX: The re-read data frame has plain numeric columns because
  # file formats don't store R's 'units' metadata.
  read_back_df <- sf::st_drop_geometry(read_back_map)

  # We must explicitly drop units from the original data for a fair comparison.
  parcels_no_units <- units::drop_units(sf::st_drop_geometry(parcels))

  # Now, this comparison will work because both are plain data frames.
  expect_equal(read_back_df, parcels_no_units)

  # 3.3. Test that the geometries themselves are identical.
  expect_identical(
    sf::st_as_text(sf::st_geometry(read_back_map)),
    sf::st_as_text(sf::st_geometry(parcels))
  )
})

test_that("overwrite argument works as expected", {
  temp_dir <- withr::local_tempdir()
  temp_gpkg_path <- file.path(temp_dir, "parcels.gpkg")
  write_map(parcels[1:10, ], temp_gpkg_path)
  expect_error(write_map(parcels, temp_gpkg_path))
  expect_no_error(write_map(parcels, temp_gpkg_path, overwrite = TRUE))
  read_back_map <- sf::st_read(temp_gpkg_path, quiet = TRUE)
  expect_equal(nrow(read_back_map), nrow(parcels))
})

test_that("write_map handles invalid inputs gracefully", {
  temp_dir <- withr::local_tempdir()
  temp_path <- file.path(temp_dir, "parcels.gpkg")
  expect_error(write_map(as.data.frame(parcels), temp_path), "`map` must be a valid `sf` object.")
  expect_error(write_map(parcels, file_path = 12345), "`file_path` must be a single character string.")
  temp_path_xxx <- file.path(temp_dir, "parcels.xxx")
  expect_error(suppressWarnings( write_map(parcels, temp_path_xxx)) )
})