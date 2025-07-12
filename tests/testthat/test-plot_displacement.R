# This file contains tests for the plot_displacement() visualization function.

test_that("plot_displacement returns a valid ggplot object with correct data", {
  withr::with_tempdir({
    demo_files <- create_demo_data(output_dir = ".")
    # Setup: Load valid test data
    gcp_data <- read_gcps(gcp_path = demo_files$gcp_path, crs = 3857)

    # Run the function
    p <- plot_displacement(gcp_data)

    # Test 1: The output must be a ggplot object
    expect_s3_class(p, "ggplot")
  })
})