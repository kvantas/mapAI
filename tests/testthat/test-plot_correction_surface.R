# Tests for the plot_correction_surface() function

# --- Setup: Load data and train models needed for plotting tests ---
# We use the package's internal data to make tests self-contained.
data(gcps)

# Train one of each major model type to ensure plotting works for all
model_gam <- train_pai_model(gcps, pai_method = "gam")
model_rf <- train_pai_model(gcps, pai_method = "rf")


test_that("plot_correction_surface runs successfully with valid inputs", {

  # --- Test with a GAM model ---
  plot_gam <- plot_correction_surface(pai_model = model_gam, gcp_data = gcps)

  # 1. Check that the output is a 'patchwork' object
  expect_s3_class(plot_gam, "patchwork")

  # 2. Check that the patchwork object contains ggplot objects
  expect_s3_class(plot_gam[[1]], "ggplot")
  expect_s3_class(plot_gam[[2]], "ggplot")


  # --- Test with a Random Forest model ---
  # This ensures the function works with non-GAM model structures
  plot_rf <- plot_correction_surface(pai_model = model_rf, gcp_data = gcps)
  expect_s3_class(plot_rf, "patchwork")
  expect_s3_class(plot_rf[[1]], "ggplot")


  # --- Test with a custom n_grid value ---
  # This ensures the argument is passed correctly without errors
  plot_low_res <- plot_correction_surface(
    pai_model = model_gam,
    gcp_data = gcps,
    n_grid = 10 # Using a very small grid for a fast test
  )
  expect_s3_class(plot_low_res, "patchwork")
})

test_that("plot_correction_surface handles invalid inputs gracefully", {

  # 1. Test for error when pai_model is not the correct class
  expect_error(
    plot_correction_surface(pai_model = list(), gcp_data = gcps),
    "pai_model must be a valid pai_model object."
  )

  # 2. Test for error when gcp_data is not an sf object
  # We convert the sf object to a regular data frame to trigger the error
  gcp_dataframe <- sf::st_drop_geometry(gcps)
  expect_error(
    plot_correction_surface(pai_model = model_gam, gcp_data = gcp_dataframe),
    "gcp_data must be a valid sf object."
  )
})

test_that("plot layers and aesthetics are correctly specified", {
  # This is a more advanced test to check the internals of the ggplot object
  # It ensures that key layers (raster, contour, points) are present.

  plot_obj <- plot_correction_surface(pai_model = model_gam, gcp_data = gcps)

  # Check the first plot (dx)
  p_dx <- plot_obj[[1]]

  # Check that there are 3 layers (geom_raster, geom_contour, geom_point)
  expect_equal(length(p_dx$layers), 3)

  # Check the geoms used in the layers
  expect_s3_class(p_dx$layers[[1]]$geom, "GeomRaster")
  expect_s3_class(p_dx$layers[[2]]$geom, "GeomContour")
  expect_s3_class(p_dx$layers[[3]]$geom, "GeomPoint")

  # Check a key aesthetic to ensure data is mapped correctly
  # In the first layer (geom_raster), the 'fill' aesthetic should be mapped to 'dx'
  expect_equal(rlang::quo_name(p_dx$mapping$fill), "dx")
})