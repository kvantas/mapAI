# Tests for apply_pai_raster() and transform_map() with SpatRaster (in-memory)

library(terra)
library(sf)

demo_data <- create_demo_data()
test_gcps <- demo_data$gcp

test_ext <- terra::ext(
  min(test_gcps$source_x), max(test_gcps$source_x),
  min(test_gcps$source_y), max(test_gcps$source_y)
)

create_test_rast <- function(nrows = 30, ncols = 30, type = c("continuous", "categorical", "multiband")) {
  type <- match.arg(type)
  r <- terra::rast(test_ext, nrows = nrows, ncols = ncols)

  if (type == "continuous") {
    terra::values(r) <- seq_len(terra::ncell(r))
    names(r) <- "continuous_layer"
  } else if (type == "categorical") {
    set.seed(42)
    terra::values(r) <- sample(1:5, terra::ncell(r), replace = TRUE)
    names(r) <- "class_layer"
  } else if (type == "multiband") {
    r1 <- r
    r2 <- r
    r3 <- r
    terra::values(r1) <- seq_len(terra::ncell(r))
    terra::values(r2) <- seq_len(terra::ncell(r)) * 2
    terra::values(r3) <- seq_len(terra::ncell(r)) * 3
    r <- c(r1, r2, r3)
    names(r) <- c("band1", "band2", "band3")
  }
  return(r)
}

test_that("apply_pai_raster() throws errors for invalid inputs", {
  r_cont <- create_test_rast(type = "continuous")
  gam_model <- train_pai_model(test_gcps, method = "gam_biv")

  # Invalid model
  expect_error(
    apply_pai_raster(pai_model = list(), raster = r_cont),
    "`pai_model` must be an object of class 'pai_model'"
  )

  # Invalid raster
  expect_error(
    apply_pai_raster(pai_model = gam_model, raster = data.frame()),
    "`raster` must be a terra SpatRaster object or path to a valid raster file"
  )

  # Nonexistent raster file
  expect_error(
    apply_pai_raster(pai_model = gam_model, raster = "nonexistent_file.tif"),
    "Raster file not found"
  )

  # Invalid interpolation method
  expect_error(
    apply_pai_raster(pai_model = gam_model, raster = r_cont, method = "cubic_spline")
  )

  # Invalid mesh_step
  expect_error(
    apply_pai_raster(pai_model = gam_model, raster = r_cont, mesh_step = -1),
    "`mesh_step` must be a positive integer"
  )

  # Invalid aoi
  expect_error(
    apply_pai_raster(pai_model = gam_model, raster = r_cont, aoi = "invalid_aoi"),
    "`aoi` must be an sf or terra SpatVector polygon object"
  )
})

test_that("apply_pai_raster() returns a valid in-memory SpatRaster with correct properties", {
  r_cont <- create_test_rast(type = "continuous")
  gam_model <- train_pai_model(test_gcps, method = "gam_biv")

  corrected <- apply_pai_raster(gam_model, r_cont, method = "bilinear")

  expect_s4_class(corrected, "SpatRaster")
  expect_equal(terra::nrow(corrected), terra::nrow(r_cont))
  expect_equal(terra::ncol(corrected), terra::ncol(r_cont))
  expect_equal(terra::nlyr(corrected), 1)
  expect_equal(names(corrected), "continuous_layer")

  # Check that values have been warped (not identical)
  orig_vals <- terra::values(r_cont)[, 1]
  corr_vals <- terra::values(corrected)[, 1]
  expect_false(identical(orig_vals, corr_vals))
})

test_that("transform_map() dispatches seamlessly to apply_pai_raster() for SpatRaster", {
  r_cont <- create_test_rast(type = "continuous")
  gam_model <- train_pai_model(test_gcps, method = "gam_biv")

  # transform_map should work directly on SpatRaster
  corrected <- transform_map(gam_model, r_cont, method = "bilinear")

  expect_s4_class(corrected, "SpatRaster")
  expect_equal(terra::nrow(corrected), terra::nrow(r_cont))
  expect_equal(terra::ncol(corrected), terra::ncol(r_cont))
  expect_equal(terra::nlyr(corrected), 1)
})

test_that("apply_pai_raster() preserves categorical values with nearest neighbor method", {
  r_cat <- create_test_rast(type = "categorical")
  gam_model <- train_pai_model(test_gcps, method = "gam_biv")

  # Test with 'near'
  corrected_near <- apply_pai_raster(gam_model, r_cat, method = "near")
  vals_near <- stats::na.omit(terra::values(corrected_near)[, 1])
  expect_true(all(vals_near %in% 1:5))
  expect_true(all(vals_near == floor(vals_near))) # All integers

  # Test with 'simple'
  corrected_simple <- apply_pai_raster(gam_model, r_cat, method = "simple")
  vals_simple <- stats::na.omit(terra::values(corrected_simple)[, 1])
  expect_true(all(vals_simple %in% 1:5))
})

test_that("apply_pai_raster() correctly handles multi-band rasters in memory", {
  r_multi <- create_test_rast(type = "multiband")
  gam_model <- train_pai_model(test_gcps, method = "gam_biv")

  corrected <- apply_pai_raster(gam_model, r_multi, method = "bilinear")

  expect_s4_class(corrected, "SpatRaster")
  expect_equal(terra::nlyr(corrected), 3)
  expect_equal(names(corrected), c("band1", "band2", "band3"))

  # All 3 layers should be transformed
  vals <- terra::values(corrected)
  expect_equal(ncol(vals), 3)
  expect_false(identical(terra::values(r_multi)[, 1], vals[, 1]))
})

test_that("apply_pai_raster() works across all supported model types", {
  r_cont <- create_test_rast(nrows = 20, ncols = 20, type = "continuous")

  methods_to_test <- c("helmert", "lm", "tps", "gam_biv")

  for (m in methods_to_test) {
    mod <- train_pai_model(test_gcps, method = m)
    corr <- apply_pai_raster(mod, r_cont, method = "bilinear")
    expect_s4_class(corr, "SpatRaster")
    expect_equal(terra::nlyr(corr), 1)
  }
})

test_that("apply_pai_raster() mesh_step optimization works correctly in memory", {
  r_cont <- create_test_rast(nrows = 40, ncols = 40, type = "continuous")
  gam_model <- train_pai_model(test_gcps, method = "gam_biv")

  corr_exact <- apply_pai_raster(gam_model, r_cont, mesh_step = NULL)
  corr_mesh <- apply_pai_raster(gam_model, r_cont, mesh_step = 5)

  expect_s4_class(corr_mesh, "SpatRaster")
  expect_equal(dim(corr_mesh), dim(corr_exact))

  vals_exact <- terra::values(corr_exact)[, 1]
  vals_mesh <- terra::values(corr_mesh)[, 1]
  comp_idx <- !is.na(vals_exact) & !is.na(vals_mesh)

  # Mesh interpolation should closely match exact per-pixel predictions
  expect_gt(stats::cor(vals_exact[comp_idx], vals_mesh[comp_idx]), 0.999)
})

test_that("apply_pai_raster() respects custom resolution parameter", {
  r_cont <- create_test_rast(type = "continuous")
  gam_model <- train_pai_model(test_gcps, method = "gam_biv")

  orig_res <- terra::res(r_cont)
  target_res <- orig_res * 2

  corr <- apply_pai_raster(gam_model, r_cont, res = target_res)

  expect_equal(terra::res(corr)[1], target_res[1], tolerance = 1e-4)
  expect_equal(terra::res(corr)[2], target_res[2], tolerance = 1e-4)
})

test_that("apply_pai_raster() masks output when AOI is provided", {
  r_cont <- create_test_rast(type = "continuous")
  gam_model <- train_pai_model(test_gcps, method = "gam_biv")

  # Create an AOI covering the left half of the raster extent
  e <- terra::ext(r_cont)
  aoi_poly <- sf::st_polygon(list(matrix(
    c(e$xmin, e$ymin,
      (e$xmin + e$xmax) / 2, e$ymin,
      (e$xmin + e$xmax) / 2, e$ymax,
      e$xmin, e$ymax,
      e$xmin, e$ymin),
    ncol = 2, byrow = TRUE
  )))
  aoi_sf <- sf::st_sf(id = 1, geometry = sf::st_sfc(aoi_poly))

  corr_aoi <- apply_pai_raster(gam_model, r_cont, aoi = aoi_sf)

  # Cells outside the AOI (right half) should be NA
  vals <- terra::values(corr_aoi)[, 1]
  expect_true(any(is.na(vals)))
  expect_true(any(!is.na(vals)))
})

test_that("create_demo_data(raster = TRUE) produces an in-memory SpatRaster", {
  demo_with_rast <- create_demo_data(raster = TRUE)
  expect_type(demo_with_rast, "list")
  expect_named(demo_with_rast, c("gcp", "map", "raster"))
  expect_s4_class(demo_with_rast$raster, "SpatRaster")
})

test_that("analyze_distortion() works with in-memory SpatRaster input", {
  r_cont <- create_test_rast(nrows = 10, ncols = 10, type = "continuous")
  gam_model <- train_pai_model(test_gcps, method = "gam_biv")

  dist_rast <- analyze_distortion(gam_model, newdata = r_cont)

  expect_s4_class(dist_rast, "SpatRaster")
  expect_equal(terra::nlyr(dist_rast), 10)  # max_shear removed: it duplicated max_angular_distortion
  expect_true(all(c("a", "b", "area_scale", "signed_area_scale", "det_J", "is_inverted",
                    "log2_area_scale", "max_angular_distortion",
                    "airy_kavrayskiy", "theta_a") %in% names(dist_rast)))
})

test_that("apply_pai_raster() works with custom models provided as a list", {
  custom_shift <- list(
    label = "Custom Shift",
    library = NULL,
    modelType = "univariate",
    fit = function(x, y, ...) {
      mean(y)
    },
    predict = function(modelFit, newdata, ...) {
      rep(modelFit, nrow(newdata))
    }
  )

  custom_model <- train_pai_model(test_gcps, method = custom_shift)
  r_cont <- create_test_rast(nrows = 10, ncols = 10, type = "continuous")
  corrected_rast <- apply_pai_raster(custom_model, r_cont)

  expect_s4_class(corrected_rast, "SpatRaster")
  expect_equal(dim(corrected_rast), dim(r_cont))
})

test_that("apply_pai_raster() supports ext = 'auto' and custom ext", {
  r_cont <- create_test_rast(nrows = 15, ncols = 15, type = "continuous")
  gam_model <- train_pai_model(test_gcps, method = "gam_biv")

  # Auto extent
  corr_auto <- apply_pai_raster(gam_model, r_cont, ext = "auto")
  expect_s4_class(corr_auto, "SpatRaster")
  expect_false(identical(as.vector(terra::ext(corr_auto)), as.vector(terra::ext(r_cont))))

  # Custom SpatExtent. Cells are whole, so with the source resolution preserved
  # the realised extent COVERS the request, expanded by at most one cell on the
  # upper edges. It must never be smaller than requested.
  cust_ext <- terra::ext(-10, 110, -10, 110)
  corr_cust <- suppressWarnings(apply_pai_raster(gam_model, r_cont, ext = cust_ext))
  ec <- unname(as.vector(terra::ext(corr_cust)))   # xmin, xmax, ymin, ymax
  expect_equal(ec[c(1, 3)], c(-10, -10))
  expect_gte(ec[2], 110)
  expect_gte(ec[4], 110)
  expect_lt(ec[2] - 110, terra::res(r_cont)[1])
  expect_lt(ec[4] - 110, terra::res(r_cont)[2])
  expect_equal(terra::res(corr_cust), terra::res(r_cont), tolerance = 1e-9)

  # Numeric vector of length 4
  corr_vec <- suppressWarnings(apply_pai_raster(gam_model, r_cont, ext = c(-5, 105, -5, 105)))
  ev <- unname(as.vector(terra::ext(corr_vec)))
  expect_equal(ev[c(1, 3)], c(-5, -5))
  expect_gte(ev[2], 105)
  expect_gte(ev[4], 105)

  # Error handling for invalid ext, max_iter, tol
  expect_error(
    apply_pai_raster(gam_model, r_cont, ext = "invalid_option"),
    "`ext` must be 'auto'"
  )
  expect_error(
    apply_pai_raster(gam_model, r_cont, max_iter = 0),
    "`max_iter` must be a positive integer"
  )
  expect_error(
    apply_pai_raster(gam_model, r_cont, tol = -1),
    "`tol` must be a positive number"
  )
})




#### Regression tests for the corrected raster engine ####

test_that("mesh_step does not silently lose cells for any step size", {
  r_cont <- create_test_rast(nrows = 40, ncols = 40, type = "continuous")
  gam_model <- train_pai_model(test_gcps, method = "gam_biv")

  exact <- suppressWarnings(
    apply_pai_raster(gam_model, r_cont, ext = "auto", mesh_step = NULL))
  na_exact <- sum(is.na(terra::values(exact)[, 1]))

  # Steps that do not divide the raster dimensions evenly used to shift the
  # coarse extent; and even an exact divisor lost the outer half-cell ring
  # because resample() interpolates between coarse cell centres.
  for (ms in c(2, 3, 5, 7, 9, 10)) {
    mesh <- suppressWarnings(
      apply_pai_raster(gam_model, r_cont, ext = "auto", mesh_step = ms))
    na_mesh <- sum(is.na(terra::values(mesh)[, 1]))
    expect_lt(na_mesh - na_exact, 5, label = paste("mesh_step =", ms))
    expect_equal(dim(mesh), dim(exact))
  }
})

test_that("an explicit ext preserves the source resolution when res is NULL", {
  r_cont <- create_test_rast(type = "continuous")
  gam_model <- train_pai_model(test_gcps, method = "gam_biv")

  e <- terra::ext(r_cont)
  wider <- c(e$xmin - 5, e$xmax + 5, e$ymin - 5, e$ymax + 5)

  corr <- suppressWarnings(apply_pai_raster(gam_model, r_cont, ext = wider))

  # ext<- keeps nrow/ncol and rescales the cell size, so this used to change the
  # pixel size (and square off a non-square aspect ratio) without saying so.
  expect_equal(terra::res(corr), terra::res(r_cont), tolerance = 1e-9)
})

test_that("non-convergent coordinate inversion warns instead of returning quietly", {
  r_cont <- create_test_rast(nrows = 20, ncols = 20, type = "continuous")
  gam_model <- train_pai_model(test_gcps, method = "gam_biv")

  expect_warning(
    apply_pai_raster(gam_model, r_cont, ext = "auto", max_iter = 1, tol = 1e-12),
    "did not converge"
  )

  # A generous budget must NOT warn.
  expect_no_warning(
    apply_pai_raster(gam_model, r_cont, ext = "auto", max_iter = 50, tol = 1e-4)
  )
})

test_that("lambda is validated and exposed", {
  r_cont <- create_test_rast(nrows = 10, ncols = 10, type = "continuous")
  gam_model <- train_pai_model(test_gcps, method = "gam_biv")

  expect_error(
    apply_pai_raster(gam_model, r_cont, lambda = 0),
    "`lambda` must be a single number"
  )
  expect_error(
    apply_pai_raster(gam_model, r_cont, lambda = 3),
    "`lambda` must be a single number"
  )
  expect_s4_class(
    suppressWarnings(apply_pai_raster(gam_model, r_cont, lambda = 1)),
    "SpatRaster"
  )
})

test_that("ext = 'auto' bounds a locally sharp warp, not just a smooth one", {
  # A narrow bump between perimeter samples used to be clipped: the old code
  # probed only 10 points per edge and nothing in the interior.
  bump_model <- list(
    label = "Narrow Bump", modelType = "bivariate", library = NULL,
    fit = function(dat, ...) list(),
    predict = function(modelFit, newdata, ...) {
      cx <- mean(range(test_gcps$source_x))
      cy <- min(test_gcps$source_y)
      s <- 0.02 * diff(range(test_gcps$source_x))
      g <- exp(-((newdata$source_x - cx)^2 + (newdata$source_y - cy)^2) /
                 (2 * s^2))
      data.frame(dx = rep(0, nrow(newdata)),
                 dy = -30 * g)
    }
  )
  mod <- train_pai_model(test_gcps, method = bump_model)
  r_cont <- create_test_rast(nrows = 60, ncols = 60, type = "continuous")

  corr <- suppressWarnings(apply_pai_raster(mod, r_cont, ext = "auto"))

  # The warped image reaches ~30 units below the source ymin; the auto extent
  # must cover it.
  expect_lt(terra::ext(corr)$ymin, terra::ext(r_cont)$ymin - 25)
})
