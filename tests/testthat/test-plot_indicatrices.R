# This file contains tests for the plot_indicatrices() visualization function.

# --- Setup: Train a model and analyze distortion once for all tests ---
# This is efficient as the model training is the slowest part.
data(gcps)
# We use a GAM model as it provides the best spatially varying distortion for testing.
test_gam_model <- train_pai_model(gcps, method = "gam")
# We analyze distortion at the original GCP locations.
distortion_at_gcps <- analyze_distortion(test_gam_model, gcps)

# --- Test Scenarios ---

test_that("plot_indicatrices returns a valid ggplot object", {
  # 1. ACTION: Call the function with valid inputs.
  p <- plot_indicatrices(
    distortion_sf = distortion_at_gcps,
    scale_factor = 1 # scale_factor can be small for non-visual tests
  )

  # 2. ASSERTION: The output must be a ggplot object.
  expect_s3_class(p, "ggplot")
})

test_that("plot_indicatrices generates the correct geometry properties", {
  # 1. ACTION: Call the function and capture the plot object.
  p <- plot_indicatrices(
    distortion_sf = distortion_at_gcps,
    scale_factor = 1
  )

  # 2. ASSERTIONS:
  # The plot should contain exactly one layer (the geom_sf for the ellipses).
  expect_length(p$layers, 1)

  # Extract the sf object of the ellipses from the plot's data layer.
  ellipse_sf <- p$layers[[1]]$data

  # The number of generated ellipses must equal the number of input points.
  expect_equal(nrow(ellipse_sf), nrow(distortion_at_gcps))

  # The geometry type of the output must be POLYGON.
  # We check the unique types to ensure they are all polygons.
  geom_types <- unique(sf::st_geometry_type(ellipse_sf))
  expect_equal(as.character(geom_types), "POLYGON")
})

test_that("plot_indicatrices throws errors for invalid inputs", {
  # Error when distortion_sf is not an sf object.
  expect_error(
    plot_indicatrices(as.data.frame(distortion_at_gcps)),
    "`distortion_sf` must be an sf object."
  )

  # Error when required columns are missing from distortion_sf.
  distortion_missing_cols <- dplyr::select(distortion_at_gcps, -a)
  expect_error(
    plot_indicatrices(distortion_missing_cols, test_gam_model)
  )
})

test_that("plot_indicatrices arguments for color and scale work correctly", {
  # 1. SETUP: Define custom colors.
  custom_fill <- "navy"
  custom_border <- "goldenrod"

  # 2. ACTION: Create a plot with custom arguments.
  p_custom <- plot_indicatrices(
    distortion_sf = distortion_at_gcps,
    scale_factor = 1,
    fill_color = custom_fill,
    border_color = custom_border
  )

  # 3. ASSERTIONS:
  # Check that the fill and color arguments were passed to the geom's aesthetics.
  # The colors are set directly, so they appear in the `aes_params` slot of the layer.
  expect_equal(p_custom$layers[[1]]$aes_params$fill, custom_fill)
  expect_equal(p_custom$layers[[1]]$aes_params$colour, custom_border)

  # Test that scale_factor changes the output geometry.
  p_scaled <- plot_indicatrices(
    distortion_sf = distortion_at_gcps,
    scale_factor = 100 # A much larger scale factor
  )

  # The bounding box of the scaled ellipses should be different from the unscaled ones.
  bbox_original <- sf::st_bbox(p_custom$layers[[1]]$data)
  bbox_scaled <- sf::st_bbox(p_scaled$layers[[1]]$data)

  expect_false(identical(bbox_original, bbox_scaled))
})

