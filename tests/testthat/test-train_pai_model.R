library(testthat)
library(sf)
library(dplyr)
library(mgcv)
library(tidymodels)
library(rsample)
library(yardstick)

# Helper function to create dummy gcp_data
create_dummy_gcp_data <- function(n = 200) {
  set.seed(123) # for reproducibility of dummy data
  data.frame(
    source_x = runif(n, 0, 100),
    source_y = runif(n, 0, 100),
    dx = rnorm(n, 0, 0.1),
    dy = rnorm(n, 0, 0.1)
  ) %>%
    sf::st_as_sf(coords = c("source_x", "source_y"), crs = 4326, remove = FALSE)
}

test_that("train_pai_model returns a pai_model object for 'gam' method", {
  gcp_data <- create_dummy_gcp_data()
  model_gam <- train_pai_model(gcp_data, method = "gam")

  expect_s3_class(model_gam, "pai_model")
  expect_true("model" %in% names(model_gam))
  expect_s3_class(model_gam$model, "gam")
  expect_equal(model_gam$method, "gam")
})

test_that("train_pai_model returns a pai_model object for 'lm' method", {
  gcp_data <- create_dummy_gcp_data()
  model_lm <- train_pai_model(gcp_data, method = "lm")

  expect_s3_class(model_lm, "pai_model")
  expect_true("model" %in% names(model_lm))
  expect_true("model_dx" %in% names(model_lm$model))
  expect_true("model_dy" %in% names(model_lm$model))
  expect_s3_class(model_lm$model$model_dx, "workflow")
  expect_s3_class(model_lm$model$model_dy, "workflow")
  expect_equal(model_lm$method, "lm")
})

test_that("train_pai_model returns a pai_model object for 'rf' method", {
  gcp_data <- create_dummy_gcp_data()
  model_rf <- train_pai_model(gcp_data, method = "rf")

  expect_s3_class(model_rf, "pai_model")
  expect_true("model" %in% names(model_rf))
  expect_true("model_dx" %in% names(model_rf$model))
  expect_true("model_dy" %in% names(model_rf$model))
  expect_s3_class(model_rf$model$model_dx, "workflow")
  expect_s3_class(model_rf$model$model_dy, "workflow")
  expect_equal(model_rf$method, "rf")
})

test_that("train_pai_model returns a pai_model object for 'svm_radial' method", {
  gcp_data <- create_dummy_gcp_data()
  model_svm_radial <- train_pai_model(gcp_data, method = "svm_radial")

  expect_s3_class(model_svm_radial, "pai_model")
  expect_true("model" %in% names(model_svm_radial))
  expect_true("model_dx" %in% names(model_svm_radial$model))
  expect_true("model_dy" %in% names(model_svm_radial$model))
  expect_s3_class(model_svm_radial$model$model_dx, "workflow")
  expect_s3_class(model_svm_radial$model$model_dy, "workflow")
  expect_equal(model_svm_radial$method, "svm_radial")
})

test_that("train_pai_model returns a pai_model object for 'svm_linear' method", {
  gcp_data <- create_dummy_gcp_data()
  model_svm_linear <- train_pai_model(gcp_data, method = "svm_linear")

  expect_s3_class(model_svm_linear, "pai_model")
  expect_true("model" %in% names(model_svm_linear))
  expect_true("model_dx" %in% names(model_svm_linear$model))
  expect_true("model_dy" %in% names(model_svm_linear$model))
  expect_s3_class(model_svm_linear$model$model_dx, "workflow")
  expect_s3_class(model_svm_linear$model$model_dy, "workflow")
  expect_equal(model_svm_linear$method, "svm_linear")
})

test_that("train_pai_model throws error for invalid method", {
  gcp_data <- create_dummy_gcp_data()
  expect_error(train_pai_model(gcp_data, method = "invalid_method"),
               "Invalid method. Choose from 'lm', 'gam', 'rf', 'svm_radial', 'svm_linear'.")
})

test_that("train_pai_model produces reproducible results with same seed", {
  gcp_data <- create_dummy_gcp_data()
  model1 <- train_pai_model(gcp_data, method = "lm", seed = 42)
  model2 <- train_pai_model(gcp_data, method = "lm", seed = 42)

  # For 'lm', the models should be identical
  expect_equal(model1, model2)

  model3 <- train_pai_model(gcp_data, method = "rf", seed = 100)
  model4 <- train_pai_model(gcp_data, method = "rf", seed = 100)

  # For 'rf', the models should be identical due to seed
  expect_equal(model3, model4)
})

test_that("train_pai_model produces different results with different seeds (for stochastic methods)", {
  gcp_data <- create_dummy_gcp_data()
  model1 <- train_pai_model(gcp_data, method = "rf", seed = 1)
  model2 <- train_pai_model(gcp_data, method = "rf", seed = 2)

  # For 'rf', models should be different with different seeds
  expect_false(identical(model1, model2))
})
