# Tests for apply_pai_raster()

library(terra)
library(sf)

# Load test data and models
data(gcps)

test_ext <- terra::ext(
  min(gcps$source_x), max(gcps$source_x),
  min(gcps$source_y), max(gcps$source_y)
)

create_test_rast <- function(nrows = 30, ncols = 30, type = c("continuous", "categorical", "multiband")) {
  type <- match.arg(type)
  r <- terra::rast(test_ext, nrows = nrows, ncols = ncols, crs = sf::st_crs(gcps)$wkt)

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
  gam_model <- train_pai_model(gcps, pai_method = "gam")

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

test_that("apply_pai_raster() returns a valid SpatRaster with correct properties", {
  r_cont <- create_test_rast(type = "continuous")
  gam_model <- train_pai_model(gcps, pai_method = "gam")

  corrected <- apply_pai_raster(gam_model, r_cont, method = "bilinear")

  expect_s4_class(corrected, "SpatRaster")
  expect_equal(terra::nrow(corrected), terra::nrow(r_cont))
  expect_equal(terra::ncol(corrected), terra::ncol(r_cont))
  expect_equal(terra::nlyr(corrected), 1)
  expect_equal(names(corrected), "continuous_layer")
  expect_equal(terra::crs(corrected), terra::crs(r_cont))

  # Check that values have been warped (not identical)
  orig_vals <- terra::values(r_cont)[, 1]
  corr_vals <- terra::values(corrected)[, 1]
  expect_false(identical(orig_vals, corr_vals))
})

test_that("apply_pai_raster() preserves categorical values with nearest neighbor method", {
  r_cat <- create_test_rast(type = "categorical")
  gam_model <- train_pai_model(gcps, pai_method = "gam")

  # Test with 'near'
  corrected_near <- apply_pai_raster(gam_model, r_cat, method = "near")
  vals_near <- na.omit(terra::values(corrected_near)[, 1])
  expect_true(all(vals_near %in% 1:5))
  expect_true(all(vals_near == floor(vals_near))) # All integers

  # Test with 'simple'
  corrected_simple <- apply_pai_raster(gam_model, r_cat, method = "simple")
  vals_simple <- na.omit(terra::values(corrected_simple)[, 1])
  expect_true(all(vals_simple %in% 1:5))
})

test_that("apply_pai_raster() correctly handles multi-band rasters", {
  r_multi <- create_test_rast(type = "multiband")
  gam_model <- train_pai_model(gcps, pai_method = "gam")

  corrected <- apply_pai_raster(gam_model, r_multi, method = "bilinear")

  expect_s4_class(corrected, "SpatRaster")
  expect_equal(terra::nlyr(corrected), 3)
  expect_equal(names(corrected), c("band1", "band2", "band3"))

  # All 3 layers should be transformed
  vals <- terra::values(corrected)
  expect_equal(ncol(vals), 3)
  expect_false(identical(terra::values(r_multi)[, 1], vals[, 1]))
})

test_that("apply_pai_raster() works across different model types", {
  r_cont <- create_test_rast(nrows = 20, ncols = 20, type = "continuous")

  methods_to_test <- c("helmert", "lm", "tps", "gam", "rf", "svmRadial")

  for (m in methods_to_test) {
    mod <- train_pai_model(gcps, pai_method = m)
    corr <- apply_pai_raster(mod, r_cont, method = "bilinear")
    expect_s4_class(corr, "SpatRaster")
    expect_equal(terra::nlyr(corr), 1)
  }
})

test_that("apply_pai_raster() supports both forward and inverse trained models", {
  r_cont <- create_test_rast(nrows = 20, ncols = 20, type = "continuous")

  # Forward model with automatic inversion
  fwd_mod <- train_pai_model(gcps, pai_method = "gam", direction = "forward")
  corr_fwd <- apply_pai_raster(fwd_mod, r_cont)

  # Inverse model with direct mapping
  inv_mod <- train_pai_model(gcps, pai_method = "gam", direction = "inverse")
  corr_inv <- apply_pai_raster(inv_mod, r_cont)

  expect_s4_class(corr_fwd, "SpatRaster")
  expect_s4_class(corr_inv, "SpatRaster")

  # Both corrections should be very close to each other
  vals_fwd <- terra::values(corr_fwd)[, 1]
  vals_inv <- terra::values(corr_inv)[, 1]
  comp_idx <- !is.na(vals_fwd) & !is.na(vals_inv)
  # Correlation between the two corrections should be extremely high (> 0.99)
  expect_gt(stats::cor(vals_fwd[comp_idx], vals_inv[comp_idx]), 0.99)
})

test_that("apply_pai_raster() mesh_step optimization works correctly", {
  r_cont <- create_test_rast(nrows = 40, ncols = 40, type = "continuous")
  gam_model <- train_pai_model(gcps, pai_method = "gam")

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
  gam_model <- train_pai_model(gcps, pai_method = "gam")

  orig_res <- terra::res(r_cont)
  target_res <- orig_res * 2

  corr <- apply_pai_raster(gam_model, r_cont, res = target_res)

  expect_equal(terra::res(corr)[1], target_res[1], tolerance = 1e-4)
  expect_equal(terra::res(corr)[2], target_res[2], tolerance = 1e-4)
})

test_that("apply_pai_raster() masks output when AOI is provided", {
  r_cont <- create_test_rast(type = "continuous")
  gam_model <- train_pai_model(gcps, pai_method = "gam")

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
  aoi_sf <- sf::st_sf(id = 1, geometry = sf::st_sfc(aoi_poly, crs = sf::st_crs(gcps)))

  corr_aoi <- apply_pai_raster(gam_model, r_cont, aoi = aoi_sf)

  # Cells outside the AOI (right half) should be NA
  vals <- terra::values(corr_aoi)[, 1]
  expect_true(any(is.na(vals)))
  expect_true(any(!is.na(vals)))
})

test_that("apply_pai_raster() works with raster file paths", {
  r_cont <- create_test_rast(nrows = 20, ncols = 20, type = "continuous")
  gam_model <- train_pai_model(gcps, pai_method = "gam")

  temp_tif <- tempfile(fileext = ".tif")
  terra::writeRaster(r_cont, temp_tif, overwrite = TRUE)

  corr <- apply_pai_raster(gam_model, temp_tif)

  expect_s4_class(corr, "SpatRaster")
  expect_equal(names(corr), names(r_cont))
  unlink(temp_tif)
})
