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
  expect_error(write_map(as.data.frame(parcels), temp_path), "`map` must be a valid `sf` object")
  expect_error(write_map(parcels, file_path = 12345), "`file_path` must be a single character string.")
  temp_path_xxx <- file.path(temp_dir, "parcels.xxx")
  expect_error(suppressWarnings( write_map(parcels, temp_path_xxx)) )
})

test_that("write_map writes and reads back a raster file correctly", {
  temp_dir <- withr::local_tempdir()
  temp_tif <- file.path(temp_dir, "test_raster.tif")

  r <- terra::rast(nrows = 20, ncols = 20, crs = "EPSG:3857")
  terra::values(r) <- seq_len(terra::ncell(r))
  names(r) <- "layer1"

  # ACT: write raster
  ret <- expect_no_error(write_map(r, temp_tif, overwrite = TRUE))

  # Invisibly returns input
  expect_s4_class(ret, "SpatRaster")

  # ASSERT: file exists and can be read back
  expect_true(file.exists(temp_tif))
  r_back <- terra::rast(temp_tif)
  expect_s4_class(r_back, "SpatRaster")
  expect_equal(terra::nrow(r_back), terra::nrow(r))
  expect_equal(terra::ncol(r_back), terra::ncol(r))
  expect_equal(terra::nlyr(r_back), 1)
  expect_equal(terra::values(r_back)[, 1], terra::values(r)[, 1])
})

test_that("write_map handles overwrite correctly for rasters", {
  temp_dir <- withr::local_tempdir()
  temp_tif <- file.path(temp_dir, "test_raster_overwrite.tif")

  r <- terra::rast(nrows = 10, ncols = 10, crs = "EPSG:3857")
  terra::values(r) <- 1:100

  write_map(r, temp_tif)
  expect_true(file.exists(temp_tif))

  # Should fail when overwrite = FALSE
  expect_error(write_map(r, temp_tif, overwrite = FALSE))

  # Should succeed when overwrite = TRUE
  expect_no_error(write_map(r, temp_tif, overwrite = TRUE))
})

test_that("write_map writes multi-band rasters correctly", {
  temp_dir <- withr::local_tempdir()
  temp_tif <- file.path(temp_dir, "test_multiband.tif")

  r1 <- terra::rast(nrows = 10, ncols = 10, crs = "EPSG:3857")
  r2 <- terra::rast(nrows = 10, ncols = 10, crs = "EPSG:3857")
  terra::values(r1) <- 1:100
  terra::values(r2) <- 101:200
  r_multi <- c(r1, r2)
  names(r_multi) <- c("band1", "band2")

  write_map(r_multi, temp_tif, overwrite = TRUE)

  expect_true(file.exists(temp_tif))
  r_back <- terra::rast(temp_tif)
  expect_equal(terra::nlyr(r_back), 2)
  expect_equal(names(r_back), c("band1", "band2"))
  expect_equal(terra::values(r_back)[, 1], 1:100)
  expect_equal(terra::values(r_back)[, 2], 101:200)
})