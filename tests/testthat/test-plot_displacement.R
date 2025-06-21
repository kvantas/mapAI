# This file contains tests for the plot_displacement() visualization function.
# It relies on the DEMO_FILES object created in tests/testthat/helper-data.R

test_that("plot_displacement returns a valid ggplot object with correct data", {
  # Setup: Load valid test data
  gcp_data <- read_gcps(gcp_path = DEMO_FILES$gcp_path, crs = 3857)

  # Run the function
  p <- plot_displacement(gcp_data)

  # Test 1: The output must be a ggplot object
  expect_s3_class(p, "ggplot")
})
