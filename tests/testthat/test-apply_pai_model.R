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
