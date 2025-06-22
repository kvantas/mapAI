# Tests for apply_pai_model()

# --- Setup for all tests in this file ---
gcp_data <- read_gcps(gcp_path = DEMO_FILES$gcp_path, crs = 3857)
map_to_correct <- read_map(shp_path = DEMO_FILES$shp_path)
poly_map_to_correct <- read_map(POLYGON_FILE)
model_rf <- train_pai_model(gcp_data, method = "rf")

test_that("apply_pai_model() returns valid sf with correct number of rows", {
  corrected_map <- apply_pai_model(pai_model = model_rf, map = map_to_correct)

  expect_s3_class(corrected_map, "sf")
  expect_equal(nrow(corrected_map), nrow(map_to_correct))
  expect_false(identical(sf::st_geometry(map_to_correct), sf::st_geometry(corrected_map)))
})

test_that("apply_pai_model() adds 'area_new' for polygons", {
  corrected_poly <- apply_pai_model(pai_model = model_rf, map = poly_map_to_correct)

  expect_true("area_new" %in% names(corrected_poly))
  expect_s3_class(corrected_poly$area_new, "units")

  # FIX: Convert the result of the absolute difference to a numeric value
  # before comparing it with the numeric literal 0.
  area_diff <- abs(corrected_poly$area_new[1] - corrected_poly$area_old[1])
  expect_gt(as.numeric(area_diff), 0)
})

test_that("apply_pai_model() works with different model types", {
  model_gam <- train_pai_model(gcp_data, method = "gam")
  corrected_map_gam <- apply_pai_model(pai_model = model_gam, map = map_to_correct)
  expect_s3_class(corrected_map_gam, "sf")
  expect_equal(nrow(corrected_map_gam), nrow(map_to_correct))
})

test_that("apply_pai_model() throws errors for invalid inputs", {
  expect_error(
    apply_pai_model(pai_model = list(), map = map_to_correct),
    "`pai_model` must be an object of class 'pai_model'"
  )
  expect_error(
    apply_pai_model(pai_model = model_rf, map = data.frame()),
    "`map` must be a valid `sf` object"
  )
})

# This test block verifies that apply_pai_model works with POINT geometry.

test_that("apply_pai_model works correctly with POINT geometry", {

  # 1. ARRANGE: Load the necessary data and train a model.
  # We use the built-in 'gcps' data, which is an sf object with POINT geometry.
  data(gcps)
  # A GAM model is used because it produces a non-trivial (non-linear) transformation.
  gam_model <- train_pai_model(gcps, method = "gam")

  # 2. ACT: Apply the trained model to the gcps point data itself.
  # The function should run without error.
  corrected_points <- expect_no_error(
    apply_pai_model(pai_model = gam_model, map = gcps)
  )

  # 3. ASSERT: Check the validity and correctness of the output.

  # Assertion 3.1: The output should be a valid sf object with the same number of rows.
  expect_s3_class(corrected_points, "sf")
  expect_equal(nrow(corrected_points), nrow(gcps))

  # For points, no 'area_new' column should be added.
  expect_equal(ncol(corrected_points), ncol(gcps))

  # Assertion 3.2: The geometry type should still be POINT.
  geom_types <- unique(sf::st_geometry_type(corrected_points))
  expect_equal(as.character(geom_types), "POINT")

  # Assertion 3.3: The coordinates of the output points must have changed.
  # We compare the original source coordinates with the new geometry coordinates.
  original_coords <- sf::st_coordinates(gcps)
  new_coords <- sf::st_coordinates(corrected_points)
  expect_false(identical(original_coords, new_coords))

  # Assertion 3.4: The new coordinates should match what predict() would calculate.
  # This is a strong check to ensure the internal logic is correct.
  predicted_displacements <- predict(gam_model, newdata = sf::st_drop_geometry(gcps))
  expected_target_x <- gcps$source_x + predicted_displacements$dx
  expected_target_y <- gcps$source_y + predicted_displacements$dy

  # Check that the X and Y coordinates of the output match the expected values.
  # We use a tolerance because of potential floating-point arithmetic differences.
  expect_equal(new_coords[, 1], expected_target_x, tolerance = 1e-9)
  expect_equal(new_coords[, 2], expected_target_y, tolerance = 1e-9)
})
