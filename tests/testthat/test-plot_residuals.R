# This file contains tests for the plot_residuals() visualization function.

# --- Setup: Train a model once for all tests in this file ---
# This avoids retraining the model in every `test_that` block.
data(gcps)
# A Helmert model is used as it's fast and guaranteed to leave residuals on complex data.
test_helmert_model <- train_pai_model(gcps, method = "helmert")

# --- Test Scenarios ---

test_that("plot_residuals returns a valid ggplot object with correct structure", {
  # 1. ACTION: Call the function with valid inputs.
  p <- plot_residuals(test_helmert_model, gcps)

  # 2. ASSERTIONS:
  # The output must be a ggplot object.
  expect_s3_class(p, "ggplot")

  # The plot should contain exactly two layers:
  # Layer 1 for the segments (arrows)
  # Layer 2 for the points (at the start of the arrows)
  expect_length(p$layers, 2)
  expect_s3_class(p$layers[[1]]$geom, "GeomSegment")
  expect_s3_class(p$layers[[2]]$geom, "GeomPoint")
})

test_that("plot_residuals handles custom arguments correctly", {
  # 1. ARRANGE: Define custom arguments.
  custom_title <- "My Residual Plot"
  custom_subtitle <- "A test of custom labels"
  custom_arrow_color <- "firebrick"
  custom_point_color <- "orange"

  # 2. ACTION: Create a plot with the custom arguments.
  p_custom <- plot_residuals(
    test_helmert_model,
    gcps,
    title = custom_title,
    subtitle = custom_subtitle,
    arrow_color = custom_arrow_color,
    point_color = custom_point_color
  )

  # 3. ASSERTIONS:
  # Check that the plot labels match the custom inputs.
  expect_equal(p_custom$labels$title, custom_title)
  expect_equal(p_custom$labels$subtitle, custom_subtitle)

  # Check that the custom colors were set correctly.
  # These are fixed parameters, so they appear in the `aes_params` slot.
  expect_equal(p_custom$layers[[1]]$aes_params$colour, custom_arrow_color)
  expect_equal(p_custom$layers[[2]]$aes_params$colour, custom_point_color)
})

test_that("plot_residuals throws errors for invalid inputs", {
  # Error when pai_model is not a pai_model object.
  expect_error(
    plot_residuals(list(), gcps),
    "`pai_model` must be an object of class 'pai_model'."
  )

  # Error when gcp_data is not an sf object.
  expect_error(
    plot_residuals(test_helmert_model, as.data.frame(gcps)),
    "`gcp_data` must be a valid `sf` object."
  )
})
