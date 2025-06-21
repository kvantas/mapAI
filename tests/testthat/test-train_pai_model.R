# Test cases for train_pai_model
gcp_data <- read_gcps(gcp_path = DEMO_FILES$gcp_path, crs = 3857)

# TEST 0: helmert method
test_that("train_pai_model works with method = 'helmert'", {
  model_h <- train_pai_model(gcp_data, method = "helmert")

  expect_s3_class(model_h, "pai_model")
  expect_equal(model_h$method, "helmert")
  expect_equal(length(model_h$model$coefficients), 2)
  expect_equal(length(model_h$model$centroids), 4)
})

# Test 1: lm method
test_that("train_pai_model works with method = 'lm'", {
  model_lm <- train_pai_model(gcp_data, method = "lm")

  expect_s3_class(model_lm, "pai_model")
  expect_equal(model_lm$method, "lm")
  expect_s3_class(model_lm$model$model_dx, "lm")
  expect_s3_class(model_lm$model$model_dy, "lm")
})

# Test 2: rf method
test_that("train_pai_model works with method = 'rf'", {
  model_rf <- train_pai_model(gcp_data, method = "rf")

  expect_s3_class(model_rf, "pai_model")
  expect_equal(model_rf$method, "rf")
  expect_s3_class(model_rf$model$model_dx, "ranger")
  expect_s3_class(model_rf$model$model_dy, "ranger")
})

# Test 3: gam method
test_that("train_pai_model works with method = 'gam'", {
  model_gam <- train_pai_model(gcp_data, method = "gam")

  expect_s3_class(model_gam, "pai_model")
  expect_equal(model_gam$method, "gam")
  expect_s3_class(model_gam$model, "gam")
})

# Test 4: seed parameter for reproducibility (using rf method)
test_that("train_pai_model produces reproducible results with seed (rf method)", {

  model1 <- train_pai_model(gcp_data, method = "rf", seed = 42)
  model2 <- train_pai_model(gcp_data, method = "rf", seed = 42)
  model3 <- train_pai_model(gcp_data, method = "rf", seed = 100)

  # For rf, compare prediction error or other reproducible metrics
  # Note: ranger models might not have a direct 'coef' equivalent for comparison.
  # Comparing prediction error is a good proxy for reproducibility.
  expect_equal(model1$model$model_dx$prediction.error, model2$model$model_dx$prediction.error)
  expect_equal(model1$model$model_dy$prediction.error, model2$model$model_dy$prediction.error)

  # Ensure different seed produces different results (highly probable)
  expect_false(isTRUE(all.equal(model1$model$model_dx$prediction.error,
                                model3$model$model_dx$prediction.error)))
})

# Test 5: Handling of invalid method return error
test_that("train_pai_model returns NULL models for invalid method", {
  expect_error( train_pai_model(gcp_data, method = "invalid_method"))
})

# Test 6: Passing additional arguments via ... for lm
test_that("train_pai_model passes additional arguments to lm via ...", {
  # Pass 'weights' argument to lm
  weights_data <- runif(nrow(gcp_data), 0.1, 1)
  model_lm_weighted <- train_pai_model(gcp_data, method = "lm", weights = weights_data)

  expect_s3_class(model_lm_weighted, "pai_model")
  expect_equal(model_lm_weighted$method, "lm")
  expect_s3_class(model_lm_weighted$model$model_dx, "lm")
  expect_s3_class(model_lm_weighted$model$model_dy, "lm")
  expect_true("weights" %in% names(model_lm_weighted$model$model_dx$call))
  expect_true("weights" %in% names(model_lm_weighted$model$model_dy$call))
})

# Test 7: Passing additional arguments via ... for rf
test_that("train_pai_model passes additional arguments to rf via ...", {
  # Pass 'min.node.size' argument to ranger
  model_rf_min_node <- train_pai_model(gcp_data, method = "rf", min.node.size = 2)

  expect_s3_class(model_rf_min_node, "pai_model")
  expect_equal(model_rf_min_node$method, "rf")
  expect_s3_class(model_rf_min_node$model$model_dx, "ranger")
  expect_s3_class(model_rf_min_node$model$model_dy, "ranger")
  expect_equal(model_rf_min_node$model$model_dx$min.node.size, 2)
  expect_equal(model_rf_min_node$model$model_dx$min.node.size, 2)
})

# Test 8: Passing additional arguments via ... for gam
test_that("train_pai_model passes additional arguments to gam via ...", {

  # let's test a top-level argument that gam accepts, e.g., 'optimizer'
  model_gam_gamma <- train_pai_model(gcp_data, method = "gam", gamma = 3)

  expect_s3_class(model_gam_gamma, "pai_model")
  expect_equal(model_gam_gamma$method, "gam")
  expect_s3_class(model_gam_gamma$model, "gam")

  # Check if the gamma argument was passed
  expect_true("gamma" %in% names(model_gam_gamma$model$call))
})
