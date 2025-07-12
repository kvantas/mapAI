# Tests for the model assessment function: assess_pai_model()

test_that("assess_pai_model()  handels arguments corectly", {
  withr::with_tempdir({
    demo_files <- create_demo_data(output_dir = ".")
    gcp_data <- read_gcps(gcp_path = demo_files$gcp_path, crs = 3857)

    expect_error( assess_pai_model(list(), method = "rf"))
    expect_error( assess_pai_model(gcp_data, method = "rf", validation_type = "xxx"))
    expect_no_error(assess_pai_model(gcp_data = gcp_data, validation_type = "probability", k_folds = NULL, method = "rf"))
    expect_no_error(assess_pai_model(gcp_data = gcp_data, validation_type = "stratified", k_folds = NULL, method = "rf"))

  })
})


test_that("assess_pai_model() returns a correctly structured data frame", {
  withr::with_tempdir({
    demo_files <- create_demo_data(output_dir = ".")
    gcp_data <- read_gcps(gcp_path = demo_files$gcp_path, crs = 3857)

    methods_to_test <- c("rf", "lm", "gam", "helmert", "tps")
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

    # Test for probability sampling
    assessment1_prob <- assess_pai_model(gcp_data, method = "rf", validation_type = "probability", seed = 789)
    assessment2_prob <- assess_pai_model(gcp_data, method = "rf", validation_type = "probability", seed = 789)
    expect_equal(assessment1_prob, assessment2_prob)
  })
})

test_that("assessment is reproducible when a seed is set", {
  withr::with_tempdir({
    demo_files <- create_demo_data(output_dir = ".")
    gcp_data <- read_gcps(gcp_path = demo_files$gcp_path, crs = 3857)

    assessment1 <- assess_pai_model(gcp_data, method = "rf", seed = 123)
    assessment2 <- assess_pai_model(gcp_data, method = "rf", seed = 123)
    expect_equal(assessment1, assessment2)
  })
})

test_that("assess_pai_model() handles invalid inputs gracefully", {
  withr::with_tempdir({
    demo_files <- create_demo_data(output_dir = ".")
    gcp_data <- read_gcps(gcp_path = demo_files$gcp_path, crs = 3857)

    expect_error(assess_pai_model(gcp_data, method = "svm"))

    # THE FIX for the error: Check for the correct error message.
    small_gcp_data <- gcp_data[1:3, ]
    expect_error(
      assess_pai_model(small_gcp_data, method = "rf", k_folds = 5),
      "The number of complete GCPs is less than k_folds.",
      fixed = TRUE
    )
  })
})

test_that("sanitize data works",{
  withr::with_tempdir({
    demo_files <- create_demo_data(output_dir = ".")
    gcp_data <- read_gcps(gcp_path = demo_files$gcp_path, crs = 3857)

    gcd_data_na <- gcp_data
    gcd_data_na$source_x[1] <- NA

    expect_warning(
      assessment <- assess_pai_model(gcd_data_na, method = "rf", seed = 123)
      )
  })
})
