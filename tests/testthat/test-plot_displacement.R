# This file contains tests for the plot_displacement() visualization function.

test_that("plot_displacement returns a valid ggplot object with correct data", {
  withr::with_tempdir({
    demo_files <- create_demo_data(output_dir = ".")
    # Setup: Load valid test data
    gcp_data <- read_gcps(gcp_path = demo_files$gcp_path)

    # Run the function
    p <- plot_displacement(gcp_data)

    # Test 1: The output must be a ggplot object
    expect_s3_class(p, "ggplot")

    # Test 2: Check if exaggeration factor works
    exaggeration_factor <- 2
    p_exaggerated <- plot_displacement(
      gcp_data,
      exaggeration_factor = exaggeration_factor
    )
    plot_data <- ggplot2::layer_data(p_exaggerated, 1)

    # Calculate expected endpoints
    expected_xend <- gcp_data$source_x +
      exaggeration_factor * (gcp_data$target_x - gcp_data$source_x)
    expected_yend <- gcp_data$source_y +
      exaggeration_factor * (gcp_data$target_y - gcp_data$source_y)

    # Check if the calculated endpoints in the plot match the expected ones
    expect_equal(plot_data$xend, expected_xend)
    expect_equal(plot_data$yend, expected_yend)
  })
})
