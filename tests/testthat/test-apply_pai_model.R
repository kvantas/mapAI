gcp_data <- read_gcps(gcp_path = DEMO_FILES$gcp_path, crs = 3857)
map_to_correct <- read_map(shp_path = DEMO_FILES$shp_path)
poly_map_to_correct <- read_map(POLYGON_FILE)
model_rf <- train_pai_model(gcp_data, method = "rf")

test_that("apply_pai_model() returns valid sf with correct number of rows", {
  corrected_map <- apply_pai_model(pai_model = model_rf, map = map_to_correct)

  # The crucial test: checks that the number of features is preserved
  expect_equal(nrow(corrected_map), nrow(map_to_correct))

  expect_s3_class(corrected_map, "sf")
  # Check that geometry has actually been modified
  expect_false(identical(sf::st_geometry(map_to_correct), sf::st_geometry(corrected_map)))
})

test_that("apply_pai_model() adds 'area_new' for polygons", {
  corrected_poly <- apply_pai_model(pai_model = model_rf, map = poly_map_to_correct)
  expect_true("area_new" %in% names(corrected_poly))
  expect_s3_class(corrected_poly$area_new, "units")
  # Area should have changed due to correction
  expect_gt(abs(corrected_poly$area_new[1] - corrected_poly$area_old[1]), 0)
})

test_that("apply_pai_model() throws errors for invalid inputs", {
  expect_error(apply_pai_model(pai_model = list(), map = map_to_correct))
  expect_error(apply_pai_model(pai_model = model_rf, map = data.frame()))
})
