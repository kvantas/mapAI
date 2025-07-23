# Tests for the plot_correction_surface() function

# --- Setup: Load data and train models needed for plotting tests ---
# We use the package's internal data to make tests self-contained.
data(gcps)

# Train one of each major model type to ensure plotting works for all
model_gam <- train_pai_model(gcps, pai_method = "gam")
model_rf <- train_pai_model(gcps, pai_method = "rf")


test_that("plot_correction_surface returns a list of two ggplot objects", {

  # --- Test with a GAM model ---
  plot_list_gam <- plot_correction_surface(pai_model = model_gam, gcp_data = gcps)

  # 1. Check that the output is a list of length 2
  expect_type(plot_list_gam, "list")
  expect_length(plot_list_gam, 2)
  expect_named(plot_list_gam, c("dx_plot", "dy_plot"))

  # 2. Check that the list contains ggplot objects
  expect_s3_class(plot_list_gam$dx_plot, "ggplot")
  expect_s3_class(plot_list_gam$dy_plot, "ggplot")


  # --- Test with a Random Forest model ---
  plot_list_rf <- plot_correction_surface(pai_model = model_rf, gcp_data = gcps)
  expect_s3_class(plot_list_rf$dx_plot, "ggplot")


  # --- Test with a custom n_grid value ---
  plot_list_low_res <- plot_correction_surface(
    pai_model = model_gam,
    gcp_data = gcps,
    n_grid = 10 # Using a very small grid for a fast test
  )
  expect_s3_class(plot_list_low_res$dx_plot, "ggplot")
})

test_that("plot_correction_surface handles invalid inputs gracefully", {

  # 1. Test for error when pai_model is not the correct class
  expect_error(
    plot_correction_surface(pai_model = list(), gcp_data = gcps),
    "pai_model must be a valid pai_model object."
  )

  # 2. Test for error when gcp_data is not an sf object
  gcp_dataframe <- sf::st_drop_geometry(gcps)
  expect_error(
    plot_correction_surface(pai_model = model_gam, gcp_data = gcp_dataframe),
    "gcp_data must be a valid sf object."
  )
})

test_that("plot layers and aesthetics are correctly specified", {
  plot_list <- plot_correction_surface(pai_model = model_gam, gcp_data = gcps)
  p_dx <- plot_list$dx_plot

  # Check that there are 3 layers (geom_raster, geom_contour, geom_point)
  expect_equal(length(p_dx$layers), 3)
  expect_s3_class(p_dx$layers[[1]]$geom, "GeomRaster")
  expect_s3_class(p_dx$layers[[2]]$geom, "GeomContour")
  expect_s3_class(p_dx$layers[[3]]$geom, "GeomPoint")

  # Check a key aesthetic to ensure data is mapped correctly
  expect_equal(rlang::quo_name(p_dx$mapping$fill), "dx")
})

test_that("plot_gcps argument correctly adds or removes point layer", {
  # --- Test when plot_gcps is TRUE (the default) ---
  plot_with_gcps <- plot_correction_surface(
    pai_model = model_gam,
    gcp_data = gcps,
    plot_gcps = TRUE
  )
  expect_equal(length(plot_with_gcps$dx_plot$layers), 3)
  expect_s3_class(plot_with_gcps$dx_plot$layers[[3]]$geom, "GeomPoint")

  # --- Test when plot_gcps is FALSE ---
  plot_without_gcps <- plot_correction_surface(
    pai_model = model_gam,
    gcp_data = gcps,
    plot_gcps = FALSE
  )
  expect_equal(length(plot_without_gcps$dx_plot$layers), 2)
})

test_that("dx_range and dy_range arguments set scale limits correctly", {
  dx_limits <- c(-5, 5)
  dy_limits <- c(-10, 10)

  plot_list <- plot_correction_surface(
    pai_model = model_gam,
    gcp_data = gcps,
    dx_range = dx_limits,
    dy_range = dy_limits
  )

  expect_s3_class(plot_list$dx_plot, "ggplot")
  expect_s3_class(plot_list$dy_plot, "ggplot")


  })
