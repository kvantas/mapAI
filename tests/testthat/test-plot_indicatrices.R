# This file contains tests for the plot_indicatrices() visualization function.

# --- Setup: Train a model and analyze distortion once for all tests ---
# This is efficient as the model training is the slowest part.
data(gcps)
# We use a GAM model as it provides the best spatially varying distortion for testing.
test_gam_model <- train_pai_model(gcps, pai_method = "gam")
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

test_that("plot_indicatrices handles zero-row inputs gracefully", {
  # ARRANGE: Create an empty version of the distortion_sf object
  empty_distortion_sf <- distortion_at_gcps[0, ]

  # ACTION & ASSERT: The function should run without errors and return a ggplot object
  # with no data layers, or a data layer with zero rows.
  p_empty <- expect_no_error(plot_indicatrices(empty_distortion_sf))
  expect_s3_class(p_empty, "ggplot")

  # Check that the data in the plot layer is empty
  expect_equal(nrow(p_empty$layers[[1]]$data), 0)
})
