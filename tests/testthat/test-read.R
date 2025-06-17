# Tests for read_gcps() and read_map()

test_that("read_gcps() works and handles errors", {
  gcp_data <- read_gcps(gcp_path = DEMO_FILES$gcp_path, crs = 3857)
  expect_s3_class(gcp_data, "sf")
  expect_true(all(c("source_x", "dx", "dy", "geometry") %in% names(gcp_data)))
  expect_equal(sf::st_crs(gcp_data), sf::st_crs(3857))
  expect_error(read_gcps(DEMO_FILES$gcp_path), "A coordinate reference system")
})

test_that("read_map() works and handles errors", {
  map_data <- read_map(shp_path = DEMO_FILES$shp_path, crs = 3857)
  expect_s3_class(map_data, "sf")
  expect_false(is.na(sf::st_crs(map_data)))
  expect_error(read_map("non_existent_map.shp"), "Map file not found")
})

test_that("read_map() calculates 'area_old' for polygons", {
  poly_map <- read_map(POLYGON_FILE, crs = 3857)
  expect_true("area_old" %in% names(poly_map))
  expect_s3_class(poly_map$area_old, "units")
  expect_equal(as.numeric(poly_map$area_old[1]), 100)
})
