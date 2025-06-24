# Tests for the model assessment function: assess_pai_model()

# --- Setup for all tests in this file ---
gcp_data <- read_gcps(gcp_path = DEMO_FILES$gcp_path, crs = 3857)

test_that("assess_pai_model() returns a correctly structured data frame", {
  methods_to_test <- c("rf", "lm", "gam")
  validation_types <- c("random", "spatial")

  for (method in methods_to_test) {
    for (v_type in validation_types) {
      assessment <- assess_pai_model(
        gcp_data,
        method = method,
        validation_type = v_type,
        k_folds = 3
      )
      expect_s3_class(assessment, "data.frame")
      expect_equal(nrow(assessment), 1)
      expect_named(assessment, c("Method", "ValidationType", "Mean_RMSE_2D", "SD_RMSE_2D"))
    }
  }
})

test_that("assessment is reproducible when a seed is set", {
  assessment1 <- assess_pai_model(gcp_data, method = "rf", seed = 123)
  assessment2 <- assess_pai_model(gcp_data, method = "rf", seed = 123)
  expect_equal(assessment1, assessment2)
})

test_that("assess_pai_model() handles invalid inputs gracefully", {
  expect_error(assess_pai_model(gcp_data, method = "svm"))

  # THE FIX for the error: Check for the correct error message.
  small_gcp_data <- gcp_data[1:3, ]
  expect_error(
    assess_pai_model(small_gcp_data, method = "rf", k_folds = 5),
    "The number of complete GCPs is less than k_folds. Please use a smaller k_folds value.",
    fixed = TRUE
  )
})

test_that("sanitize data works",{

  gcd_data_na <- gcp_data
  gcd_data_na$source_x[1] <- NA

  expect_warning(
    assessment <- assess_pai_model(gcd_data_na, method = "rf", seed = 123)
    )

})

