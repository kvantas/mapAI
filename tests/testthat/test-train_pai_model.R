library(testthat)
library(mapAI)
library(sf)

# Create dummy gcp_data for testing
set.seed(123)
dummy_gcp_data <- data.frame(
  source_x = runif(100, 0, 100),
  source_y = runif(100, 0, 100),
  target_x = runif(100, 0, 100),
  target_y = runif(100, 0, 100),
  dx = rnorm(100, 0, 1),
  dy = rnorm(100, 0, 1)
)

dummy_gcp_data <- sf::st_as_sf(dummy_gcp_data, coords = c("source_x", "source_y"), crs = 4326)


test_that("train_pai_model works with lm method", {
  model <- train_pai_model(dummy_gcp_data, method = "lm")
  expect_s3_class(model, "mapAI_model")
  expect_equal(model$method, "lm")
  expect_true("model_dx" %in% names(model$model))
  expect_true("model_dy" %in% names(model$model))
})

test_that("train_pai_model works with gam method", {
  model <- train_pai_model(dummy_gcp_data, method = "gam")
  expect_s3_class(model, "mapAI_model")
  expect_equal(model$method, "gam")
})

test_that("train_pai_model works with rf method", {
  model <- train_pai_model(dummy_gcp_data, method = "rf")
  expect_s3_class(model, "mapAI_model")
  expect_equal(model$method, "rf")
})

test_that("train_pai_model works with svm_radial method", {
  model <- train_pai_model(dummy_gcp_data, method = "svm_radial")
  expect_s3_class(model, "mapAI_model")
  expect_equal(model$method, "svm_radial")
})

test_that("train_pai_model works with svmlinear method", {
  model <- train_pai_model(dummy_gcp_data, method = "svmlinear")
  expect_s3_class(model, "mapAI_model")
  expect_equal(model$method, "svmlinear")
})

test_that("train_pai_model throws error for invalid method", {
  expect_error(train_pai_model(dummy_gcp_data, method = "invalid_method"),
               "Invalid method. Choose from 'lm', 'gam', 'rf', 'svm_radial', 'svmlinear'.")
})

test_that("train_pai_model throws error for nnet method", {
  expect_error(train_pai_model(dummy_gcp_data, method = "nnet"),
               "Invalid method. Choose from 'lm', 'gam', 'rf', 'svm_radial', 'svmlinear'.")
})

test_that("train_pai_model throws error for knn method", {
  expect_error(train_pai_model(dummy_gcp_data, method = "knn"),
               "Invalid method. Choose from 'lm', 'gam', 'rf', 'svm_radial', 'svmlinear'.")
})
