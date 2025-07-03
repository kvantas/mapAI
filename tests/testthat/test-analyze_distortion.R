# --- Comprehensive Test Suite for analyze_distortion() ---
#
# This script uses 'testthat' and 'testthat::stub' to robustly test the
# analyze_distortion() function. The stub() approach ensures that tests
# run correctly in isolated environments like those used by devtools::check().

library(testthat)
library(sf)
library(dplyr)
library(testthat) # Explicitly load for stub

# --- Test Setup ---

# Create a consistent set of analysis points to use across all tests.
testing_points <- st_as_sf(
  data.frame(
    id = 1:9,
    x = rep(c(-10, 0, 10), 3),
    y = rep(c(-10, 0, 10), each = 3)
  ),
  coords = c("x", "y"),
  crs = 3857
)

# We only need a simple placeholder object. Its class is what matters for the
# original function's input validation.
create_placeholder_model <- function(method_name) {
  structure(list(method = method_name), class = "pai_model")
}

# Define a tolerance for floating-point comparisons
TOLERANCE <- 1e-5

# ==============================================================================
# ---- TEST SUITE for analyze_distortion() ----
# ==============================================================================

test_that("Input validation rejects incorrect object types", {
  mock_model <- create_placeholder_model("gam")

  expect_error(
    analyze_distortion(list(), testing_points),
    "`pai_model` must be an object of class 'pai_model'.",
    fixed = TRUE
  )

  expect_error(
    analyze_distortion(mock_model, as.data.frame(testing_points)),
    "`points_to_analyze` must be an sf object.",
    fixed = TRUE
  )
})

test_that("Output has the correct structure, columns, and types", {
  mock_model <- create_placeholder_model("gam")

  # Mock predict to return zero offsets
  mock_predict_for_structure <- function(object, newdata, ...) {
    data.frame(dx = rep(0, nrow(newdata)), dy = rep(0, nrow(newdata)))
  }

  stub(analyze_distortion, 'predict', mock_predict_for_structure)

  results <- suppressMessages(
    analyze_distortion(mock_model, testing_points)
  )

  expect_s3_class(results, "sf")
  expect_equal(nrow(results), nrow(testing_points))
  expected_new_cols <- c("a", "b", "area_scale", "log2_area_scale", "max_shear",
                         "max_angular_distortion", "airy_kavrayskiy", "theta_a")
  expect_true(all(expected_new_cols %in% names(results)))
})

test_that("Correctly identifies an identity transformation (no distortion)", {
  mock_model <- create_placeholder_model("identity")

  # Mock transformation: x' = u, y' = v. Offsets are zero.
  mock_predict_identity <- function(object, newdata, ...) {
    data.frame(
      target_x_offset = rep(0, nrow(newdata)),
      target_y_offset = rep(0, nrow(newdata))
    )
  }

  stub(analyze_distortion, 'predict', mock_predict_identity)

  results <- suppressMessages(analyze_distortion(mock_model, testing_points))

  expect_equal(results$a, rep(1, nrow(results)), tolerance = TOLERANCE)
  expect_equal(results$b, rep(1, nrow(results)), tolerance = TOLERANCE)
  expect_equal(results$area_scale, rep(1, nrow(results)), tolerance = TOLERANCE)
  expect_equal(results$max_shear, rep(0, nrow(results)), tolerance = TOLERANCE)
})

test_that("Correctly calculates metrics for pure area scaling", {
  mock_model <- create_placeholder_model("area_scale")

  # Mock transformation: x' = 2u, y' = 2v
  mock_predict_area <- function(object, newdata, ...) {
    data.frame(
      target_x_offset = (2 * newdata$source_x) - newdata$source_x,
      target_y_offset = (2 * newdata$source_y) - newdata$source_y
    )
  }

  stub(analyze_distortion, 'predict', mock_predict_area)

  results <- suppressMessages(analyze_distortion(mock_model, testing_points))

  # Theoretical values for x'=2u, y'=2v: a=2, b=2, area_scale=4, max_shear=0
  expect_equal(results$a, rep(2.0, nrow(results)), tolerance = TOLERANCE)
  expect_equal(results$b, rep(2.0, nrow(results)), tolerance = TOLERANCE)
  expect_equal(results$area_scale, rep(4.0, nrow(results)), tolerance = TOLERANCE)
  expect_equal(results$max_shear, rep(0.0, nrow(results)), tolerance = TOLERANCE)
})

test_that("Correctly calculates metrics for a pure shear transformation", {
  mock_model <- create_placeholder_model("shear")

  # Mock transformation: x' = u + v, y' = v
  mock_predict_shear <- function(object, newdata, ...) {
    data.frame(
      target_x_offset = (newdata$source_x + newdata$source_y) - newdata$source_x,
      target_y_offset = newdata$source_y - newdata$source_y
    )
  }

  stub(analyze_distortion, 'predict', mock_predict_shear)

  results <- suppressMessages(analyze_distortion(mock_model, testing_points))

  # Theoretical values for x'=u+v, y'=v
  expected_a <- (1 + sqrt(5)) / 2
  expected_b <- 1 / expected_a
  expected_area_scale <- 1.0
  max_shear_rad <- asin((expected_a - expected_b) / (expected_a + expected_b))
  expected_max_shear_deg <- max_shear_rad * 180 / pi

  expect_equal(results$a, rep(expected_a, nrow(results)), tolerance = TOLERANCE)
  expect_equal(results$b, rep(expected_b, nrow(results)), tolerance = TOLERANCE)
  expect_equal(results$area_scale, rep(expected_area_scale, nrow(results)), tolerance = TOLERANCE)
  expect_equal(results$max_shear, rep(expected_max_shear_deg, nrow(results)), tolerance = TOLERANCE)
})

test_that("`reference_scale` argument correctly normalizes log2_area_scale", {
  mock_model <- create_placeholder_model("lm")

  # Mock transformation: x' = 1.2u, y' = 0.8v
  mock_predict_ref_scale <- function(object, newdata, ...) {
    data.frame(
      target_x_offset = (newdata$source_x * 1.2) - newdata$source_x,
      target_y_offset = (newdata$source_y * 0.8) - newdata$source_y
    )
  }

  stub(analyze_distortion, 'predict', mock_predict_ref_scale)

  # With default reference_scale = 1
  res1 <- suppressMessages(
    analyze_distortion(mock_model, testing_points, reference_scale = 1)
  )
  # area_scale = 1.2 * 0.8 = 0.96. log2(0.96 / 1^2)
  expect_equal(unique(round(res1$log2_area_scale, 6)),
               log2(0.96),
               tolerance = TOLERANCE)

  # With reference_scale = 2
  res2 <- suppressMessages(
    analyze_distortion(mock_model, testing_points, reference_scale = 2)
  )
  # log2(0.96 / 2^2)
  expect_equal(unique(round(res2$log2_area_scale, 6)),
               log2(0.96 / 4),
               tolerance = TOLERANCE)
})
