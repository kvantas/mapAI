# This file contains tests for the plot_distortion_surface() visualization function.

# --- Setup: Train a model once for all tests in this file ---
data(gcps)
test_gam_model <- train_pai_model(gcps, pai_method = "gam")

# --- Test Scenarios ---

test_that("plot_distortion_surface correctly identifies scattered points and uses geom_point", {
  # SETUP
  distortion_at_gcps <- analyze_distortion(test_gam_model, gcps)
  # ACTION & ASSERTIONS
  expect_message(plot_distortion_surface(distortion_at_gcps, metric = "area_scale"), "Scattered points detected")
  p_scatter <- suppressMessages(plot_distortion_surface(distortion_at_gcps, metric = "area_scale"))
  expect_s3_class(p_scatter$layers[[1]]$geom, "GeomPoint")
})

test_that("plot_distortion_surface correctly identifies a regular grid and uses geom_raster", {
  # SETUP
  analysis_grid <- sf::st_make_grid(gcps, n = c(5, 5)) %>% sf::st_centroid() %>% sf::st_sf()
  distortion_on_grid <- analyze_distortion(test_gam_model, analysis_grid)
  # ACTION & ASSERTIONS
  expect_message(plot_distortion_surface(distortion_on_grid, metric = "area_scale"), "Regular grid detected")
  p_grid <- suppressMessages(plot_distortion_surface(distortion_on_grid, metric = "area_scale"))
  expect_s3_class(p_grid$layers[[1]]$geom, "GeomRaster")
})

test_that("plot_distortion_surface throws errors for invalid inputs", {
  # SETUP
  distortion_at_gcps <- analyze_distortion(test_gam_model, gcps)
  # ASSERTIONS
  expect_error(plot_distortion_surface(distortion_at_gcps, metric = "invalid"), "`metric` must be one of:")
  distortion_sf_subset <- dplyr::select(distortion_at_gcps, -max_shear)
  expect_error(plot_distortion_surface(distortion_sf_subset, metric = "max_shear"), "Metric max_shear not found")
})

test_that("diverging and gcp_data arguments work correctly", {
  # 1. SETUP
  analysis_grid <- sf::st_make_grid(gcps, n = c(5, 5)) %>% sf::st_centroid() %>% sf::st_sf()
  distortion_on_grid <- analyze_distortion(test_gam_model, analysis_grid)

  # 2. ACTION
  # Create a plot with a diverging scale.
  p_diverging <- suppressMessages(
    plot_distortion_surface(
      distortion_on_grid,
      metric = "area_scale",
      diverging = TRUE
    )
  )

  # 3. ASSERTIONS

  # CRITICAL FIX: This is the robust test using the scale's public $map() method.
  # Extract the scale object from the plot
  scale_object <- p_diverging$scales$scales[[1]]

  # Test that a data value equal to the midpoint (1 for area_scale)
  # is mapped to the middle color of our palette ('#F1F1F1').
  expect_equal(scale_object$map(1), "#F1F1F1")

  # Test with GCPs overlay (this part of the test was already correct)
  p_with_gcps <- suppressMessages(
    plot_distortion_surface(distortion_on_grid, metric = "area_scale", gcp_data = gcps)
  )
  expect_length(p_with_gcps$layers, 2)
  expect_s3_class(p_with_gcps$layers[[2]]$geom, "GeomSf")
})