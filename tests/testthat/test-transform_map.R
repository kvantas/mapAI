test_that("map_transform() returns valid sf object", {

  demo_data <- create_demo_data()
  gcp_data <- demo_data$gcp
  map_to_correct <- demo_data$map
  model_rf <- train_pai_model(gcp_data, "rf")
  corrected_map <- transform_map(pai_model = model_rf, map = map_to_correct)

  expect_s3_class(corrected_map, "sf")
  expect_equal(nrow(corrected_map), nrow(map_to_correct))
  expect_equal(st_crs(corrected_map), st_crs(map_to_correct))
  expect_false(identical(sf::st_geometry(map_to_correct), sf::st_geometry(corrected_map)))
})

test_that("map_transform() works correcty with all geometry types",{

  # Create a simple GCP dataset
  gcps <- create_dummy_gcp_data(100)

  # create a simple shift model for testing
  shift_model <- list(
    label = "Simple Shift",
    library = NULL,
    modelType = "bivariate",
    fit = function(dat, ...) {
      list(shift = TRUE)
    },
    predict = function(modelFit, newdata, ...) {
      # bivariate prediction
      n <- nrow(newdata)
      cbind(rep(10, n), rep(10, n))

    }
  )
  # fit the model
  model <- train_pai_model(gcps, shift_model)

  # Create various sf objects with different geometry types for testing
  # the gcps range is roughly 0 to 1000 in both x and y

  # --- Test 1: POINT Geometry  ---
  df <- data.frame(id = c(1,2,3),
                   x = c(100, 200, 300),
                   y= c(105, 205, 305))
  original_points <- st_as_sf(df, coords = c("x", "y"), crs = 3857)

  # Apply the model to the map_points data
  corrected_points <- expect_no_error(
    transform_map(pai_model = model, map = original_points)
  )
  # Check results
  expect_s3_class(corrected_points, "sf")
  expect_equal(nrow(corrected_points), nrow(original_points))
  expect_equal(as.character(unique(sf::st_geometry_type(corrected_points))), "POINT")
  expect_false(identical(original_points, corrected_points))

  # --- Test 2: LINESTRING Geometry ---
  ls_coords <- matrix(c(0,0, 10,10, 10,0, 0,10), ncol = 2, byrow = TRUE)
  linestring_sf <- st_sf(id = 1, geometry = st_sfc(st_linestring(ls_coords), crs = 3857))
  corrected_ls <- expect_no_error(
    transform_map(pai_model = model, map = linestring_sf))

  # check results
  expect_s3_class(corrected_ls, "sf")
  expect_equal(nrow(corrected_ls), nrow(linestring_sf))
  expect_equal(as.character(unique(sf::st_geometry_type(corrected_ls))), "LINESTRING")
  expect_false(identical(linestring_sf, corrected_ls))

  # --- Test 3: POLYGON Geometry including a hole ---
  outer_ring <- matrix(c(0,0, 0,100, 100,100, 100,0, 0,0), ncol=2, byrow=TRUE)
  inner_hole <- matrix(c(25,25, 75,25, 75,75, 25,75, 25,25), ncol=2, byrow=TRUE)
  polygon_sf <- st_polygon(list(outer_ring, inner_hole))
  polygon_sf <- st_sf(id = 1, geometry = st_sfc(polygon_sf, crs = 3857))
  corrected_poly <- expect_no_error(
    transform_map(pai_model = model, map = polygon_sf))

  # check results
  expect_s3_class(corrected_poly, "sf")
  expect_equal(nrow(corrected_poly), nrow(polygon_sf))
  expect_equal(as.character(unique(sf::st_geometry_type(corrected_poly))), "POLYGON")
  expect_false(identical(polygon_sf, corrected_poly))

  # --- Test 4: MULTIPOINT Geometry ---
  multipoint_coords <- matrix(c(0,0, 10,10, 20,20), ncol = 2, byrow = TRUE)
  multipoint_sf <- st_sf(id = 1,
                         geometry = st_sfc(st_multipoint(multipoint_coords), crs = 3857))
  corrected_multipoint <- expect_no_error(
    transform_map(pai_model = model, map = multipoint_sf)
  )
  # check results
  expect_s3_class(corrected_multipoint, "sf")
  expect_equal(nrow(corrected_multipoint), nrow(multipoint_sf))
  expect_equal(as.character(unique(sf::st_geometry_type(corrected_multipoint))),
               "MULTIPOINT")
  expect_false(identical(multipoint_sf, corrected_multipoint))

  # --- Test 5: MULTILINESTRING Geometry ---
  ls1 <- matrix(c(0,0, 10,10, 20,20), ncol = 2, byrow = TRUE)
  ls2 <- matrix(c(30,30, 40,0), ncol = 2, byrow = TRUE)
  multi_sf <- st_sf(id=1,
                    geometry=st_sfc(st_multilinestring(list(ls1, ls2)), crs = 3857))
  corrected_multi <- expect_no_error( transform_map(pai_model = model, map = multi_sf))

  # check results
  expect_s3_class(corrected_multi, "sf")
  expect_equal(nrow(corrected_multi), nrow(multi_sf))
  expect_equal(as.character(unique(st_geometry_type(corrected_multi))),
               "MULTILINESTRING")
  expect_false(identical(multi_sf, corrected_multi))

  # --- Test 6: MULTIPOLYGON Geometry ---
  p1 <- st_polygon(list(matrix(c(0,0, 10,0, 10,10, 0,10, 0,0), ncol=2, byrow=TRUE)))
  p2 <- st_polygon(list(matrix(c(20,20, 30,20, 30,30, 20,30, 20,20), ncol=2, byrow=TRUE)))
  multipolygon_sf <- st_sf(id=1, geometry=st_sfc(st_multipolygon(list(p1, p2)), crs = 3857))
  corrected_multipol <- expect_no_error( transform_map(pai_model = model, map = multipolygon_sf))

  # check results
  expect_s3_class(corrected_multipol, "sf")
  expect_equal(nrow(corrected_multipol), nrow(multipolygon_sf))
  expect_equal(as.character(unique(st_geometry_type(corrected_multipol))),
               "MULTIPOLYGON")
  expect_false(identical(multipolygon_sf, corrected_multipol))

  # --- Test 7: Empty Geometry ---
  p1 <- st_polygon(list(matrix(c(0,0, 10,0, 10,10, 0,10, 0,0), ncol=2, byrow=TRUE)))
  empty_geom <- sf::st_polygon()
  sf_with_empty <- sf::st_sf(id=1:2, geometry=sf::st_sfc(p1, empty_geom, crs = 3857))
  corrected_sf <- expect_no_error(transform_map(pai_model = model, map = sf_with_empty))

  # check results
  expect_s3_class(corrected_sf, "sf")
  expect_equal(nrow(corrected_sf), 2)
  expect_equal(as.character(sf::st_geometry_type(corrected_sf[1,])), "POLYGON")
  expect_true(sf::st_is_empty(corrected_sf$geometry[[2]]))
  expect_false(identical(sf::st_coordinates(sf_with_empty[1,]), sf::st_coordinates(corrected_sf[1,])))


  # --- Test 8: Mixed Geometry Types (if applicable, though sf usually restricts this) ---
  p1 <- sf::st_polygon(list(matrix(c(0,0, 10,0, 10,10, 0,10, 0,0), ncol=2, byrow=TRUE)))
  pt1 <- sf::st_point(c(50,50))
  # Note: sf objects usually don't mix types in one column.
  # We test this by applying the model to two separate sf objects.
  poly_sf <- sf::st_sf(id=1, geom=sf::st_sfc(p1, crs=3857))
  point_sf <- sf::st_sf(id=1, geom=sf::st_sfc(pt1, crs=3857))

  # check results
  corrected_poly <- expect_no_error(transform_map(model, poly_sf))
  expect_s3_class(corrected_poly, "sf")
  expect_equal(as.character(sf::st_geometry_type(corrected_poly)), "POLYGON")
  corrected_point <- expect_no_error(transform_map(model, point_sf))
  expect_s3_class(corrected_point, "sf")
  expect_equal(as.character(sf::st_geometry_type(corrected_point)), "POINT")


})

test_that("apply_pai_model works correctly with an AOI polygon", {

  # Create a simple GCP dataset
  gcps <- create_dummy_gcp_data(100)
  gam_model <- train_pai_model(gcps, "gam_biv")

  # Create a simple line and a polygon AOI that covers part of it
  line_coords <- matrix(c(0,0, 100,0), ncol = 2, byrow = TRUE)
  line_sf <- sf::st_sf(id = 1, geometry = sf::st_sfc(sf::st_linestring(line_coords), crs = 3857))

  aoi_poly_coords <- matrix(c(-10,-10, 50,-10, 50,10, -10,10, -10,-10), ncol=2, byrow=TRUE)
  aoi_poly <- sf::st_sf(id = 1, geometry = sf::st_sfc(sf::st_polygon(list(aoi_poly_coords)), crs = 3857))

  # Apply the model with the AOI
  corrected_line <- expect_no_error(transform_map(gam_model, line_sf, aoi = aoi_poly))

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
