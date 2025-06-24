# Create a consistent set of analysis points to use across all tests.
testing_points <- st_as_sf(
  data.frame(
    id = 1:10,
    x = seq(1000, 10000, length.out = 10),
    y = seq(2000, 20000, length.out = 10)
  ),
  coords = c("x", "y"),
  crs = 3857
)

# We only need a simple placeholder object. Its class is what matters for the
# original function's input validation. Its internal structure is irrelevant
# because the `predict` call will be completely replaced by the mock.
create_placeholder_model <- function(method_name) {
  structure(list(method = method_name), class = "pai_model")
}

# ==============================================================================
# ---- TEST SUITE for analyze_distortion() ----
# ==============================================================================

test_that("Input validation rejects incorrect object types", {
  # This test doesn't call predict, so no mocking is needed.
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

  # Define the mock behavior for this test
  # CORRECTED: The mock function must return a data.frame with the same
  # number of rows as its input 'newdata'.
  mock_predict_for_structure <- function(object, newdata, ...) {
    data.frame(dx = rep(0, nrow(newdata)), dy = rep(0, nrow(newdata)))
  }

  # Use stub() to set up the mock for the next call to analyze_distortion
  stub(analyze_distortion, 'predict', mock_predict_for_structure)

  # Now execute the original function; the 'predict' call inside it is mocked.
  results <- suppressMessages(
    analyze_distortion(mock_model, testing_points)
  )

  expect_s3_class(results, "sf")
  expect_equal(nrow(results), nrow(testing_points))
  expected_new_cols <- c("a", "b", "area_scale", "log2_area_scale", "max_shear",
                         "max_angular_distortion", "airy_kavrayskiy", "theta_a")
  expect_true(all(expected_new_cols %in% names(results)))
})

test_that("Logic for 'lm' yields constant distortion", {
  mock_model <- create_placeholder_model("lm")

  # This function works correctly because vectorized operations on columns
  # already produce an output with the correct number of rows.
  mock_predict_lm <- function(object, newdata, ...) {
    data.frame(
      dx = (newdata$source_x * 1.2) - newdata$source_x,
      dy = (newdata$source_y * 0.8) - newdata$source_y
    )
  }

  stub(analyze_distortion, 'predict', mock_predict_lm)

  results <- suppressMessages(
    analyze_distortion(mock_model, testing_points)
  )

  expect_length(unique(round(results$a, 8)), 1)
  expect_length(unique(round(results$b, 8)), 1)
})

test_that("Logic for 'rf' models results in no local distortion", {
  mock_model <- create_placeholder_model("rf")

  # CORRECTED: Ensure the mock returns a value for every row of input data.
  mock_predict_rf <- function(object, newdata, ...) {
    data.frame(dx = rep(0, nrow(newdata)), dy = rep(0, nrow(newdata)))
  }

  stub(analyze_distortion, 'predict', mock_predict_rf)

  results <- suppressMessages(
    analyze_distortion(mock_model, testing_points)
  )

  expect_equal(results$a, rep(1, nrow(results)))
  expect_equal(results$area_scale, rep(1, nrow(results)))
})

test_that("Logic for 'gam' models results in variable distortion", {
  mock_model <- create_placeholder_model("gam")

  # This function works correctly because sin() and cos() are vectorized
  # and will produce an output with the correct number of rows.
  mock_predict_gam <- function(object, newdata, ...) {
    data.frame(
      dx = sin(newdata$source_x / 1e5) * 10,
      dy = cos(newdata$source_y / 1e5) * 10
    )
  }

  stub(analyze_distortion, 'predict', mock_predict_gam)

  results <- suppressMessages(
    analyze_distortion(mock_model, testing_points)
  )

  expect_gt(length(unique(round(results$a, 8))), 1)
  expect_gt(length(unique(round(results$b, 8))), 1)
})

test_that("`reference_scale` argument correctly normalizes log2_area_scale", {
  mock_model <- create_placeholder_model("lm")

  mock_predict_lm <- function(object, newdata, ...) {
    data.frame(
      dx = (newdata$source_x * 1.2) - newdata$source_x,
      dy = (newdata$source_y * 0.8) - newdata$source_y
    )
  }

  stub(analyze_distortion, 'predict', mock_predict_lm)

  res1 <- suppressMessages(
    analyze_distortion(mock_model, testing_points, reference_scale = 1)
  )
  expect_equal(unique(res1$log2_area_scale), log2(0.96))

  # The stub persists within the test_that block, so we can call again
  res2 <- suppressMessages(
    analyze_distortion(mock_model, testing_points, reference_scale = 2)
  )
  expect_equal(unique(res2$log2_area_scale), log2(0.96 / 4))
})
