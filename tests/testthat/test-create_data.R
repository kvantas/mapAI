# This test suite assumes that the 'create_demo_data' function is loaded
# into the environment, and the 'testthat', 'sf', and 'dplyr' packages are installed.

library(testthat)
library(sf)
library(dplyr)

# Parent temporary directory for all tests in this file.
# Subdirectories will be created inside this for isolation.
base_temp_dir <- tempdir(check = TRUE)

test_that("Function executes with default parameters and creates files", {
  test_dir <- file.path(base_temp_dir, "default_test")
  dir.create(test_dir, showWarnings = FALSE)

  # Capture messages to check both are produced in a single run
  msgs <- capture_messages({
    demo_files <- create_demo_data(output_dir = test_dir, seed = 1)
  })

  # Check for expected messages
  expect_true(any(grepl("Homologous points saved to:", msgs)))
  expect_true(any(grepl("Distorted map saved to:", msgs)))

  # Check that the returned object is a list with the correct names
  expect_type(demo_files, "list")
  expect_named(demo_files, c("shp_path", "gcp_path"))

  # Verify that the files were actually created at the specified paths
  expect_true(file.exists(demo_files$shp_path))
  expect_true(file.exists(demo_files$gcp_path))

  # Clean up the created files and directory
  unlink(test_dir, recursive = TRUE)
})

test_that("Output files have the correct structure and content", {
  test_dir <- file.path(base_temp_dir, "structure_test")
  dir.create(test_dir, showWarnings = FALSE)

  n_pts <- 5 # Use a smaller number for faster testing
  demo_files <- create_demo_data(n_points = n_pts, output_dir = test_dir, seed = 101)

  # Test GCP CSV file
  gcp_data <- read.csv(demo_files$gcp_path)
  expect_s3_class(gcp_data, "data.frame")
  expect_named(gcp_data, c("source_x", "source_y", "target_x", "target_y"))
  expect_equal(nrow(gcp_data), n_pts^2)

  # Test Map Shapefile
  map_data <- sf::st_read(demo_files$shp_path, quiet = TRUE)
  expect_s3_class(map_data, "sf")
  expect_s3_class(sf::st_geometry(map_data), "sfc_LINESTRING")
  # Should be n horizontal + n vertical lines
  expect_equal(nrow(map_data), n_pts * 2)

  # Clean up
  unlink(test_dir, recursive = TRUE)
})


test_that("All distortion types run without error", {
  # Test each type in a loop to keep code DRY
  for (type in c("helmert", "nonlinear", "complex")) {
    test_dir <- file.path(base_temp_dir, paste0(type, "_test"))
    dir.create(test_dir, showWarnings = FALSE)

    # Use expect_silent to ensure no errors or warnings are thrown
    expect_message(
      files <- create_demo_data(type = type, output_dir = test_dir, seed = which(c("helmert", "nonlinear", "complex") == type))
    )
    expect_true(file.exists(files$gcp_path))
    expect_true(file.exists(files$shp_path))

    # Clean up
    unlink(test_dir, recursive = TRUE)
  }
})


test_that("Custom parameters are correctly applied", {
  test_dir <- file.path(base_temp_dir, "custom_params_test")
  dir.create(test_dir, showWarnings = FALSE)

  # Test custom grid limits
  custom_limits <- c(1000, 1100, 2000, 2100)
  files <- create_demo_data(grid_limits = custom_limits, output_dir = test_dir, seed = 202)
  gcp_data <- read.csv(files$gcp_path)

  # The "target" coordinates should be within the new custom limits
  expect_gte(min(gcp_data$target_x), custom_limits[1])
  expect_lte(max(gcp_data$target_x), custom_limits[2])
  expect_gte(min(gcp_data$target_y), custom_limits[3])
  expect_lte(max(gcp_data$target_y), custom_limits[4])

  # Clean up
  unlink(test_dir, recursive = TRUE)
})


test_that("Seed ensures reproducibility", {
  # Create separate directories for each run to ensure isolation
  dir1 <- file.path(base_temp_dir, "seed_test_1")
  dir2 <- file.path(base_temp_dir, "seed_test_2")
  dir3 <- file.path(base_temp_dir, "seed_test_3")
  dir.create(dir1); dir.create(dir2); dir.create(dir3)

  # Generate two datasets with the same seed in different directories
  files1 <- create_demo_data(output_dir = dir1, seed = 42)
  files2 <- create_demo_data(output_dir = dir2, seed = 42)

  # The content of the GCP files should be identical
  expect_identical(read.csv(files1$gcp_path), read.csv(files2$gcp_path))

  # Generate a third dataset with a different seed
  files3 <- create_demo_data(output_dir = dir3, seed = 43)

  # The content should NOT be identical to the first one
  expect_false(identical(read.csv(files1$gcp_path), read.csv(files3$gcp_path)))

  # Clean up all directories
  unlink(dir1, recursive = TRUE)
  unlink(dir2, recursive = TRUE)
  unlink(dir3, recursive = TRUE)
})

test_that("Function stops with invalid `type` parameter", {
  # Expect the function to throw a specific error for a wrong type
  expect_error(
    create_demo_data(type = "invalid_type"),
    "Invalid 'type'. Choose from 'helmert', 'nonlinear', or 'complex'."
  )
})

