test_that("all four triangulation models train and predict correctly", {
  gcp <- create_dummy_gcp_data(40)

  models <- c("tin_linear", "tin_akima", "hybrid_helmert_tin", "hybrid_affine_tin")

  for (m in models) {
    fit <- expect_no_error(suppressWarnings(train_pai_model(gcp, method = m)))
    expect_s3_class(fit, "pai_model")

    # In-sample prediction
    preds <- expect_no_error(predict(fit, newdata = gcp))
    expect_equal(nrow(preds), nrow(gcp))
    expect_false(any(is.na(preds$dx)))
    expect_false(any(is.na(preds$dy)))

    # Exact interpolation check for linear and hybrid models
    if (m %in% c("tin_linear", "hybrid_helmert_tin", "hybrid_affine_tin")) {
      expect_lt(max(abs(preds$dx - gcp$dx)), 1e-7)
      expect_lt(max(abs(preds$dy - gcp$dy)), 1e-7)
    }

    # Out-of-bounds extrapolation check (outside convex hull)
    out_pts <- data.frame(
      source_x = c(-500, 1500, 2000),
      source_y = c(-500, 1500, 2000)
    )
    out_preds <- expect_no_error(predict(fit, newdata = out_pts))
    expect_equal(nrow(out_preds), 3)
    expect_false(any(is.na(out_preds$dx)))
    expect_false(any(is.na(out_preds$dy)))
  }
})

test_that("check_tin_inversion detects valid and inverted triangles", {
  gcp <- create_dummy_gcp_data(30)

  # Normal model: should have high validity
  model <- suppressWarnings(train_pai_model(gcp, method = "tin_linear"))
  inv_res <- expect_no_error(check_tin_inversion(model))

  expect_s3_class(inv_res, "tin_inversion")
  expect_true("is_inverted" %in% names(inv_res))
  expect_true("det_J" %in% names(inv_res))
  expect_gt(nrow(inv_res), 0)

  # Test plot = TRUE
  plt <- expect_no_error(check_tin_inversion(model, plot = TRUE))
  expect_s3_class(plt, "ggplot")

  # Intentionally create inverted triangles by flipping target coordinates of two adjacent points
  gcp_flipped <- gcp
  gcp_flipped$target_x[1:2] <- gcp$target_x[2:1]
  gcp_flipped$target_y[1:2] <- gcp$target_y[2:1]

  inv_flipped <- check_tin_inversion(gcp_flipped)
  expect_gt(attr(inv_flipped, "n_inverted"), 0)
})

test_that("tin_linear and hybrid models integrate with transform_map on vector data", {
  gcp <- create_dummy_gcp_data(40)
  test_map <- sf::st_sf(
    id = 1:2,
    geometry = sf::st_sfc(
      sf::st_polygon(list(cbind(c(200, 400, 400, 200, 200), c(200, 200, 400, 400, 200)))),
      sf::st_polygon(list(cbind(c(600, 800, 800, 600, 600), c(600, 600, 800, 800, 600))))
    ),
    crs = 32632
  )

  for (m in c("tin_linear", "hybrid_helmert_tin")) {
    model <- suppressWarnings(train_pai_model(gcp, method = m))
    corr_map <- expect_no_error(transform_map(model, test_map, repair_topology = TRUE))
    expect_s3_class(corr_map, "sf")
    expect_equal(nrow(corr_map), 2)
    expect_true(all(sf::st_is_valid(corr_map)))
  }
})

test_that("triangulation models work with SpatRaster in apply_pai_raster", {
  gcp <- create_dummy_gcp_data(30)
  model <- suppressWarnings(train_pai_model(gcp, method = "tin_linear"))

  ext <- terra::ext(min(gcp$source_x), max(gcp$source_x), min(gcp$source_y), max(gcp$source_y))
  r <- terra::rast(ext, nrows = 10, ncols = 10)
  terra::values(r) <- 1:100

  r_rect <- expect_no_error(apply_pai_raster(model, r))
  expect_s4_class(r_rect, "SpatRaster")
  expect_equal(terra::ncell(r_rect), 100)
})

test_that("triangulation models work with analyze_distortion", {
  gcp <- create_dummy_gcp_data(35)
  for (m in c("tin_linear", "tin_akima", "hybrid_helmert_tin")) {
    model <- suppressWarnings(train_pai_model(gcp, method = m))
    dist <- expect_no_error(analyze_distortion(model, newdata = gcp[1:10, ]))
    expect_s3_class(dist, "distortion")
    expect_equal(nrow(dist), 10)
    expect_true(all(!is.na(dist$det_J)))
    expect_true(all(!is.na(dist$a)))
    expect_true(all(!is.na(dist$b)))
  }
})

test_that("triangulation models support print, summary, surface, and residuals methods", {
  gcp <- create_dummy_gcp_data(30)
  model <- suppressWarnings(train_pai_model(gcp, method = "tin_linear"))

  expect_output(print(model), "Piecewise Affine Delaunay Triangulation")
  res_plot <- expect_no_error(residuals(model))
  expect_s3_class(res_plot, "ggplot")

  surf_plot <- expect_no_error(surface(model, n_grid = 10))
  expect_type(surf_plot, "list")
  expect_s3_class(surf_plot$dx_plot, "ggplot")
})

test_that("triangulation models work with spatial cross-validation in assess_pai_model", {
  gcp <- create_dummy_gcp_data(40)
  cv_res <- expect_no_error(assess_pai_model(
    gcp_data = gcp,
    method = "hybrid_helmert_tin",
    validation_type = "random",
    k_folds = 3
  ))
  expect_s3_class(cv_res, "pai_assessment")
  expect_false(is.na(cv_res$summary$Mean_RMSE_2D))
})
