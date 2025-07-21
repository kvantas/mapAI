# Tests for apply_pai_model()

test_that("apply_pai_model() returns valid sf with correct number of rows", {
  withr::with_tempdir({
    demo_files <- create_demo_data(output_dir = ".")
    gcp_data <- read_gcps(gcp_path = demo_files$gcp_path)
    map_to_correct <- read_map(shp_path = demo_files$shp_path)
    model_rf <- train_pai_model(gcp_data, pai_method = "rf")

    corrected_map <- apply_pai_model(pai_model = model_rf, map = map_to_correct)

    expect_s3_class(corrected_map, "sf")
    expect_equal(nrow(corrected_map), nrow(map_to_correct))
    expect_false(identical(sf::st_geometry(map_to_correct), sf::st_geometry(corrected_map)))
  })
})

test_that("apply_pai_model() adds 'area_new' for polygons", {
  withr::with_tempdir({
    demo_files <- create_demo_data(output_dir = ".")
    gcp_data <- read_gcps(gcp_path = demo_files$gcp_path)
    polygon_file <- create_test_polygon("test_polygon.shp")
    poly_map_to_correct <- read_map(polygon_file)
    model_rf <- train_pai_model(gcp_data, pai_method = "rf")

    corrected_poly <- apply_pai_model(pai_model = model_rf, map = poly_map_to_correct)

    expect_true("area_new" %in% names(corrected_poly))
    expect_s3_class(corrected_poly$area_new, "units")

    # FIX: Convert the result of the absolute difference to a numeric value
    # before comparing it with the numeric literal 0.
    area_diff <- abs(corrected_poly$area_new[1] - corrected_poly$area_old[1])
    expect_gt(as.numeric(area_diff), 0)
  })
})

test_that("apply_pai_model() works with different model types", {
  withr::with_tempdir({
    demo_files <- create_demo_data(output_dir = ".")
    gcp_data <- read_gcps(gcp_path = demo_files$gcp_path)
    map_to_correct <- read_map(shp_path = demo_files$shp_path)
    model_gam <- train_pai_model(gcp_data, pai_method = "gam")
    corrected_map_gam <- apply_pai_model(pai_model = model_gam, map = map_to_correct)
    expect_s3_class(corrected_map_gam, "sf")
    expect_equal(nrow(corrected_map_gam), nrow(map_to_correct))
  })
})

test_that("apply_pai_model() throws errors for invalid inputs", {
  withr::with_tempdir({
    demo_files <- create_demo_data(output_dir = ".")
    gcp_data <- read_gcps(gcp_path = demo_files$gcp_path)
    map_to_correct <- read_map(shp_path = demo_files$shp_path)
    model_rf <- train_pai_model(gcp_data, pai_method = "rf")

    expect_error(
      apply_pai_model(pai_model = list(), map = map_to_correct),
      "`pai_model` must be an object of class 'pai_model'"
    )
    expect_error(
      apply_pai_model(pai_model = model_rf, map = data.frame()),
      "`map` must be a valid `sf` object"
    )
  })
})


data(gcps)
test_gam_model <- train_pai_model(gcps, pai_method = "gam")


# --- Test 1: POINT Geometry (from previous debugging) ---
test_that("apply_pai_model works correctly with POINT geometry", {
  # ACT: Apply the model to the gcps point data itself.
  corrected_points <- expect_no_error(
    apply_pai_model(pai_model = test_gam_model, map = gcps)
  )
  # ASSERT:
  expect_s3_class(corrected_points, "sf")
  expect_equal(nrow(corrected_points), nrow(gcps))
  expect_equal(as.character(unique(sf::st_geometry_type(corrected_points))), "POINT")
  original_coords <- sf::st_coordinates(gcps)
  new_coords <- sf::st_coordinates(corrected_points)
  expect_false(identical(original_coords, new_coords))
})


# --- Test 2: LINESTRING Geometry ---
test_that("apply_pai_model works correctly with LINESTRING geometry", {
  # ARRANGE: Create a simple linestring sf object.
  ls_coords <- matrix(c(0,0, 10,10, 10,0, 0,10), ncol = 2, byrow = TRUE)
  linestring_sf <- sf::st_sf(id = 1, geometry = sf::st_sfc(sf::st_linestring(ls_coords), crs = 3857))

  # ACT: Apply the model.
  corrected_ls <- expect_no_error(apply_pai_model(test_gam_model, linestring_sf))

  # ASSERT:
  expect_s3_class(corrected_ls, "sf")
  expect_equal(nrow(corrected_ls), 1)
  expect_equal(as.character(sf::st_geometry_type(corrected_ls)), "LINESTRING")
  expect_false(identical(sf::st_coordinates(linestring_sf), sf::st_coordinates(corrected_ls)))
})


# --- Test 3: POLYGON Geometry (including a hole) ---
test_that("apply_pai_model works correctly with POLYGON geometry", {
  # ARRANGE: Create a polygon with a hole.
  outer_ring <- matrix(c(0,0, 0,100, 100,100, 100,0, 0,0), ncol=2, byrow=TRUE)
  inner_hole <- matrix(c(25,25, 75,25, 75,75, 25,75, 25,25), ncol=2, byrow=TRUE)
  polygon_with_hole <- sf::st_polygon(list(outer_ring, inner_hole))
  polygon_sf <- sf::st_sf(id = 1, geometry = sf::st_sfc(polygon_with_hole, crs = 3857))

  # ACT: Apply the model.
  corrected_poly <- expect_no_error(apply_pai_model(test_gam_model, polygon_sf))

  # ASSERT:
  expect_s3_class(corrected_poly, "sf")
  expect_equal(nrow(corrected_poly), 1)
  expect_equal(as.character(sf::st_geometry_type(corrected_poly)), "POLYGON")
  # Check that the corrected geometry still has a hole (i.e., is a list of 2 matrices)
  expect_length(corrected_poly$geometry[[1]], 2)
  expect_false(identical(sf::st_coordinates(polygon_sf), sf::st_coordinates(corrected_poly)))
})


# --- Test 4: MULTIPOINT Geometry ---
test_that("apply_pai_model works correctly with MULTIPOINT geometry", {
  # ARRANGE: Create a simple multipoint sf object.
  mp_coords <- matrix(c(0,0, 10,10, 20,20), ncol = 2, byrow = TRUE)
  multipoint_sf <- sf::st_sf(id = 1, geometry = sf::st_sfc(sf::st_multipoint(mp_coords), crs = 3857))

  # ACT: Apply the model.
  corrected_mp <- expect_no_error(apply_pai_model(test_gam_model, multipoint_sf))

  # ASSERT:
  expect_s3_class(corrected_mp, "sf")
  expect_equal(nrow(corrected_mp), 1)
  expect_equal(as.character(sf::st_geometry_type(corrected_mp)), "MULTIPOINT")
  expect_false(identical(sf::st_coordinates(multipoint_sf), sf::st_coordinates(corrected_mp)))
})


# --- Test 5: MULTILINESTRING Geometry ---
test_that("apply_pai_model works correctly with MULTILINESTRING geometry", {
  # ARRANGE: Create a multilinestring sf object.
  ls1 <- matrix(c(0,0, 10,10), ncol=2, byrow=TRUE)
  ls2 <- matrix(c(20,20, 30,0), ncol=2, byrow=TRUE)
  multilinestring_sf <- sf::st_sf(id=1, geometry=sf::st_sfc(sf::st_multilinestring(list(ls1, ls2)), crs = 3857))

  # ACT: Apply the model.
  corrected_mls <- expect_no_error(apply_pai_model(test_gam_model, multilinestring_sf))

  # ASSERT:
  expect_s3_class(corrected_mls, "sf")
  expect_equal(nrow(corrected_mls), 1)
  expect_equal(as.character(sf::st_geometry_type(corrected_mls)), "MULTILINESTRING")
  expect_false(identical(sf::st_coordinates(multilinestring_sf), sf::st_coordinates(corrected_mls)))
})


# --- Test 6: MULTIPOLYGON Geometry ---
test_that("apply_pai_model works correctly with MULTIPOLYGON geometry", {
  # ARRANGE: Create a multipolygon sf object (two separate polygons).
  p1 <- sf::st_polygon(list(matrix(c(0,0, 10,0, 10,10, 0,10, 0,0), ncol=2, byrow=TRUE)))
  p2 <- sf::st_polygon(list(matrix(c(20,20, 30,20, 30,30, 20,30, 20,20), ncol=2, byrow=TRUE)))
  multipolygon_sf <- sf::st_sf(id=1, geometry=sf::st_sfc(sf::st_multipolygon(list(p1, p2)), crs = 3857))

  # ACT: Apply the model.
  corrected_mpoly <- expect_no_error(apply_pai_model(test_gam_model, multipolygon_sf))

  # ASSERT:
  expect_s3_class(corrected_mpoly, "sf")
  expect_equal(nrow(corrected_mpoly), 1)
  expect_equal(as.character(sf::st_geometry_type(corrected_mpoly)), "MULTIPOLYGON")
  expect_false(identical(sf::st_coordinates(multipolygon_sf), sf::st_coordinates(corrected_mpoly)))
})


# --- Test 7: Empty Geometry ---
test_that("apply_pai_model handles empty geometries gracefully", {
  # ARRANGE: Create an sf object with one empty geometry.
  p1 <- sf::st_polygon(list(matrix(c(0,0, 10,0, 10,10, 0,10, 0,0), ncol=2, byrow=TRUE)))
  empty_geom <- sf::st_polygon()
  sf_with_empty <- sf::st_sf(id=1:2, geometry=sf::st_sfc(p1, empty_geom, crs = 3857))

  # ACT: Apply the model.
  corrected_sf <- expect_no_error(apply_pai_model(test_gam_model, sf_with_empty))

  # ASSERT:
  expect_s3_class(corrected_sf, "sf")
  expect_equal(nrow(corrected_sf), 2)
  expect_true(sf::st_is_empty(corrected_sf$geometry[[2]]))
  expect_false(identical(sf::st_coordinates(sf_with_empty[1,]), sf::st_coordinates(corrected_sf[1,])))
})


# --- Test 8: Mixed Geometry Types (if applicable, though sf usually restricts this) ---
test_that("apply_pai_model handles mixed geometry types if they appear", {
  # ARRANGE: Create a GEOMETRYCOLLECTION (less common, but good to test).
  p1 <- sf::st_polygon(list(matrix(c(0,0, 10,0, 10,10, 0,10, 0,0), ncol=2, byrow=TRUE)))
  pt1 <- sf::st_point(c(50,50))
  # Note: sf objects usually don't mix types in one column.
  # We test this by applying the model to two separate sf objects.
  poly_sf <- sf::st_sf(id=1, geom=sf::st_sfc(p1, crs=3857))
  point_sf <- sf::st_sf(id=1, geom=sf::st_sfc(pt1, crs=3857))

  # ACT & ASSERT:
  corrected_poly <- expect_no_error(apply_pai_model(test_gam_model, poly_sf))
  expect_s3_class(corrected_poly, "sf")
  expect_equal(as.character(sf::st_geometry_type(corrected_poly)), "POLYGON")

  corrected_point <- expect_no_error(apply_pai_model(test_gam_model, point_sf))
  expect_s3_class(corrected_point, "sf")
  expect_equal(as.character(sf::st_geometry_type(corrected_point)), "POINT")
})

# --- Test 9: AOI polygon ---
test_that("apply_pai_model works correctly with an AOI polygon", {
  # ARRANGE: Create a simple line and a polygon AOI that covers part of it
  line_coords <- matrix(c(0,0, 100,0), ncol = 2, byrow = TRUE)
  line_sf <- sf::st_sf(id = 1, geometry = sf::st_sfc(sf::st_linestring(line_coords), crs = 3857))

  aoi_poly_coords <- matrix(c(-10,-10, 50,-10, 50,10, -10,10, -10,-10), ncol=2, byrow=TRUE)
  aoi_poly <- sf::st_sf(id = 1, geometry = sf::st_sfc(sf::st_polygon(list(aoi_poly_coords)), crs = 3857))

  # ACT: Apply the model with the AOI
  corrected_line <- expect_no_error(apply_pai_model(test_gam_model, line_sf, aoi = aoi_poly))

  # ASSERT
  expect_s3_class(corrected_line, "sf")
  # The output may have more features if the line is split, but the geometry should be valid
  expect_true(all(sf::st_is_valid(corrected_line)))

  # Check that the part inside the AOI was moved
  original_coords <- sf::st_coordinates(line_sf)
  corrected_coords <- sf::st_coordinates(corrected_line)

  # Find which original vertices were inside the AOI
  original_points <- sf::st_as_sf(as.data.frame(original_coords), coords = c("X", "Y"), crs = 3857)
  inside_indices <- which(sf::st_intersects(original_points, aoi_poly, sparse = FALSE))

  # The coordinates of the points inside the AOI should have changed
  expect_false(identical(original_coords[inside_indices, ], corrected_coords[inside_indices, ]))
})