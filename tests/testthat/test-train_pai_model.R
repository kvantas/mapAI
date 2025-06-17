# Test cases for train_pai_model

# Test 1: lm method
test_that("train_pai_model works with method = 'lm'", {
  gcp_data <- create_dummy_gcp_data()
  model_lm <- train_pai_model(gcp_data, method = "lm")

  expect_s3_class(model_lm, "pai_model")
  expect_equal(model_lm$method, "lm")
  expect_s3_class(model_lm$model$model_dx, "lm")
  expect_s3_class(model_lm$model$model_dy, "lm")
})

# Test 2: rf method
test_that("train_pai_model works with method = 'rf'", {
  gcp_data <- create_dummy_gcp_data()
  model_rf <- train_pai_model(gcp_data, method = "rf")

  expect_s3_class(model_rf, "pai_model")
  expect_equal(model_rf$method, "rf")
  expect_s3_class(model_rf$model$model_dx, "ranger")
  expect_s3_class(model_rf$model$model_dy, "ranger")
})

# Test 3: gam method
test_that("train_pai_model works with method = 'gam'", {
  gcp_data <- create_dummy_gcp_data()
  model_gam <- train_pai_model(gcp_data, method = "gam")

  expect_s3_class(model_gam, "pai_model")
  expect_equal(model_gam$method, "gam")
  expect_s3_class(model_gam$model, "gam")
})

# Test 4: seed parameter for reproducibility (using rf method)
test_that("train_pai_model produces reproducible results with seed (rf method)", {
  gcp_data <- create_dummy_gcp_data()

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

# Test 5: Handling of invalid method (returns NULL models)
test_that("train_pai_model returns NULL models for invalid method", {
  gcp_data <- create_dummy_gcp_data()
  model_invalid <- train_pai_model(gcp_data, method = "invalid_method")

  expect_s3_class(model_invalid, "pai_model")
  expect_equal(model_invalid$method, "invalid_method")
  expect_null(model_invalid$model$model_dx)
  expect_null(model_invalid$model$model_dy)
})

# Test 6: Passing additional arguments via ... for lm
test_that("train_pai_model passes additional arguments to lm via ...", {
  gcp_data <- create_dummy_gcp_data()
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
  gcp_data <- create_dummy_gcp_data()
  # Pass 'min.node.size' argument to ranger
  model_rf_min_node <- train_pai_model(gcp_data, method = "rf", min.node.size = 10)

  expect_s3_class(model_rf_min_node, "pai_model")
  expect_equal(model_rf_min_node$method, "rf")
  expect_s3_class(model_rf_min_node$model$model_dx, "ranger")
  expect_s3_class(model_rf_min_node$model$model_dy, "ranger")
  expect_equal(model_rf_min_node$model$model_dx$call$min.node.size, 10)
  expect_equal(model_rf_min_node$model$model_dy$call$min.node.size, 10)
})

# Test 8: Passing additional arguments via ... for gam
test_that("train_pai_model passes additional arguments to gam via ...", {
  gcp_data <- create_dummy_gcp_data()

  # let's test a top-level argument that gam accepts, e.g., 'optimizer'
  model_gam_optimizer <- train_pai_model(gcp_data, method = "gam", gamma = 3)

  expect_s3_class(model_gam_optimizer, "pai_model")
  expect_equal(model_gam_optimizer$method, "gam")
  expect_s3_class(model_gam_optimizer$model, "gam")

  # Check if the gamma argument was passed
  expect_true("gamma" %in% names(model_gam_optimizer$model$call))
})
