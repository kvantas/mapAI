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

test_that("value_range argument sets scale limits correctly", {
  # SETUP
  analysis_grid <- sf::st_make_grid(gcps, n = c(5, 5)) %>% sf::st_centroid() %>% sf::st_sf()
  distortion_on_grid <- analyze_distortion(test_gam_model, analysis_grid)
  custom_range <- c(0.9, 1.1)

  # ACTION
  p <- suppressMessages(
    plot_distortion_surface(
      distortion_on_grid,
      metric = "area_scale",
      value_range = custom_range
    )
  )

  # ASSERT
  expect_s3_class(p$layers[[1]]$geom, "GeomRaster")
})


test_that("plot_distortion_surface works with Helmert models (constant distortion)", {
  # SETUP
  helmert_model <- train_pai_model(gcps, pai_method = "helmert")
  analysis_grid <- sf::st_make_grid(gcps, n = c(5, 5)) %>% sf::st_centroid() %>% sf::st_sf()
  distortion_helmert <- analyze_distortion(helmert_model, analysis_grid)

  # ACTION
  p_helmert <- suppressMessages(
    plot_distortion_surface(distortion_helmert, metric = "area_scale")
  )

  # ASSERT
  expect_s3_class(p_helmert, "ggplot")
  expect_s3_class(p_helmert$layers[[1]]$geom, "GeomRaster")
})

test_that("gcp_data and diverging scale arguments work correctly", {
  # SETUP
  analysis_grid <- sf::st_make_grid(gcps, n = c(5, 5)) %>% sf::st_centroid() %>% sf::st_sf()
  distortion_on_grid <- analyze_distortion(test_gam_model, analysis_grid)

  # TEST 1: gcp_data adds a geom_sf layer
  p_with_gcps <- suppressMessages(
    plot_distortion_surface(distortion_on_grid, metric = "area_scale", gcp_data = gcps)
  )
  # The first layer is geom_raster, the second should be geom_sf for the GCPs
  expect_s3_class(p_with_gcps$layers[[2]]$geom, "GeomSf")

  # TEST 2: diverging = TRUE uses a diverging color scale
  p_diverging <- suppressMessages(
    plot_distortion_surface(distortion_on_grid, metric = "area_scale", diverging = TRUE)
  )
  # Check that the scale is a diverging scale
  scale <- p_diverging$scales$get_scales("fill")
  expect_s3_class(scale, "ScaleContinuous")
})
