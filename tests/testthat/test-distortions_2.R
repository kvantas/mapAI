library(testthat)
library(sf)
library(dplyr)

# --- Mock Model Setup ---
# To test the function, we need to create mock "pai_model" objects that
# apply transformations with known distortion properties. We define a new
# class "mock_model" that inherits from "pai_model" and create a custom
# predict method for it.

#' Custom predict method for our mock models.
#'
#' This function applies a specific transformation based on the 'type'
#' attribute of the mock_model object.
#'
#' @param object A list object of class c("mock_model", "pai_model").
#' @param newdata A data frame with 'source_x' and 'source_y' columns.
#' @return A data frame with transformed coordinates.
predict.mock_model <- function(object, newdata) {
  u <- newdata$source_x
  v <- newdata$source_y

  # The pai_model expects the *residuals* or *offsets*, not the final
  # coordinates. So we calculate the final coordinates (x', y') and then
  # subtract the original coordinates (u, v) to get the offsets.
  if (object$type == "area_scale") {
    # Transformation: x' = 2u, y' = 2v
    # This doubles the scale in both directions, quadrupling the area.
    target_x <- 2 * u
    target_y <- 2 * v
  } else if (object$type == "shear") {
    # Transformation: x' = u + v, y' = v
    # This is a simple shear transformation that preserves area.
    target_x <- u + v
    target_y <- v
  } else if (object$type == "identity") {
    # Transformation: x' = u, y' = v
    # No distortion. This is a control case.
    target_x <- u
    target_y <- v
  } else {
    stop("Unknown mock model type.")
  }

  # Return the offsets (residuals)
  data.frame(
    target_x_offset = target_x - u,
    target_y_offset = target_y - v
  )
}


# --- Test Suite Definition ---

# Create a simple set of points for analysis
# A 3x3 grid centered at the origin
analysis_points <- st_make_grid(
  st_sfc(st_point(c(0, 0))),
  n = c(3, 3),
  cellsize = c(10, 10)
) %>%
  st_centroid() %>%
  st_sf()

# Define a tolerance for floating-point comparisons
TOLERANCE <- 1e-7

# Test 1: Identity Transformation (No Distortion)
test_that("Correctly identifies an identity transformation with no distortion", {
  # 1. Setup: Create a mock model for identity transformation
  identity_model <- list(method = "mock_identity", type = "identity")
  class(identity_model) <- c("mock_model", "pai_model")

  # 2. Execution: Run the analysis
  results <- analyze_distortion(identity_model, analysis_points)

  # 3. Expectations: All distortion metrics should indicate no distortion
  # The results should be constant across all points.
  expect_equal(results$a, rep(1, nrow(results)), tolerance = TOLERANCE)
  expect_equal(results$b, rep(1, nrow(results)), tolerance = TOLERANCE)
  expect_equal(results$area_scale, rep(1, nrow(results)), tolerance = TOLERANCE)
  expect_equal(results$max_shear, rep(0, nrow(results)), tolerance = TOLERANCE)
  expect_equal(results$log2_area_scale, rep(0, nrow(results)), tolerance = TOLERANCE)
})


# Test 2: Pure Area Scaling
test_that("Correctly calculates metrics for pure area scaling", {
  # 1. Setup: Create a mock model that scales x and y by 2
  area_model <- list(method = "mock_area_scale", type = "area_scale")
  class(area_model) <- c("mock_model", "pai_model")

  # Theoretical values for x'=2u, y'=2v:
  # Jacobian = [[2, 0], [0, 2]]
  # a = 2, b = 2
  # area_scale = a * b = 4
  # max_shear = 0 (because a = b, it's a conformal transformation)
  expected_a <- 2.0
  expected_b <- 2.0
  expected_area_scale <- 4.0
  expected_max_shear <- 0.0

  # 2. Execution: Run the analysis
  results <- analyze_distortion(area_model, analysis_points)

  # 3. Expectations
  expect_equal(results$a, rep(expected_a, nrow(results)), tolerance = TOLERANCE)
  expect_equal(results$b, rep(expected_b, nrow(results)), tolerance = TOLERANCE)
  expect_equal(results$area_scale, rep(expected_area_scale, nrow(results)), tolerance = TOLERANCE)
  expect_equal(results$max_shear, rep(expected_max_shear, nrow(results)), tolerance = TOLERANCE)
})


# Test 3: Pure Shear Transformation
test_that("Correctly calculates metrics for a pure shear transformation", {
  # 1. Setup: Create a mock model for shear: x' = u + v, y' = v
  shear_model <- list(method = "mock_shear", type = "shear")
  class(shear_model) <- c("mock_model", "pai_model")

  # Theoretical values for x'=u+v, y'=v:
  # Jacobian = [[1, 1], [0, 1]]
  # E = 1, G = 2, F = 1
  # a = (0.5 * (3 + sqrt(5)))
  # b = sqrt(0.5 * (3 - sqrt(5))) -> 1/phi
  # area_scale = a * b = 1
  # max_shear_rad = asin((a-b)/(a+b))
  phi <- (1 + sqrt(5)) / 2
  expected_a <- (phi)
  expected_b <- (1/phi)
  expected_area_scale <- 1.0

  # Calculate expected max shear in degrees
  max_shear_rad <- asin((expected_a - expected_b) / (expected_a + expected_b))
  expected_max_shear_deg <- max_shear_rad * 180 / pi

  # 2. Execution: Run the analysis
  results <- analyze_distortion(shear_model, analysis_points)

  # 3. Expectations
  expect_equal(results$a, rep(expected_a, nrow(results)), tolerance = TOLERANCE)
  expect_equal(results$b, rep(expected_b, nrow(results)), tolerance = TOLERANCE)
  expect_equal(results$area_scale, rep(expected_area_scale, nrow(results)), tolerance = TOLERANCE)
  expect_equal(results$max_shear, rep(expected_max_shear_deg, nrow(results)), tolerance = TOLERANCE)
})
