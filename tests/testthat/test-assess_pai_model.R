test_that("output is the correct class and structure", {

  test_gcp <- create_dummy_gcp_data(50)

  simple_lm_method <- list(
    label = "SimpleLM",
    modelType = "univariate",
    library = NULL,
    fit = function(x, y, ...) stats::lm(y ~ ., data.frame(y, x)),
    predict = function(m, n, ...) stats::predict(m, n)
  )


  res <- cv_pai_model(gcp_data = test_gcp,
                      pai_method =  simple_lm_method,
                      validation_type =  "random",
                      k_folds = 5)

  expect_s3_class(res, "pai_assessment")
  expect_named(res, c("summary", "predictions", "details"))
  expect_s3_class(res$summary, "data.frame")
  expect_s3_class(res$predictions, "data.frame")
})

test_that("random k-fold CV works as expected", {

  test_gcp <- create_dummy_gcp_data(50)
  k <- 5

  res <- cv_pai_model(test_gcp, "lm", validation_type = "random", k_folds = k)

  expect_equal(res$summary$ValidationType, "random")

  expect_equal(res$details$k_folds, k)
  expect_true(is.na(res$details$train_split_ratio))
  expect_true(is.na(res$details$n_strata))

  expect_equal(nrow(res$predictions), nrow(test_gcp)) # All points predicted once

  expect_false(is.na(res$summary$SD_RMSE_2D)) # SD should be present
  expect_length(unique(res$predictions$fold), k)

})

test_that("spatial k-fold CV works as expected", {
  k <- 4
  test_gcp <- create_dummy_gcp_data(50)
  res <- cv_pai_model(test_gcp, "lm", validation_type = "spatial", k_folds = k)

  expect_equal(res$summary$ValidationType, "spatial")
  expect_equal(res$details$k_folds, k)
  expect_true(is.na(res$details$train_split_ratio))
  expect_true(is.na(res$details$n_strata))

  expect_equal(nrow(res$predictions), nrow(test_gcp))
  expect_false(is.na(res$summary$SD_RMSE_2D))
  expect_length(unique(res$predictions$fold), k)

})

test_that("probability (single split) works as expected", {
  test_gcp <- create_dummy_gcp_data(50)
  res <- cv_pai_model(test_gcp, "lm",
                      validation_type = "probability",
                      train_split_ratio = 0.75)

  expect_equal(res$summary$ValidationType, "probability")

  expect_true(is.na(res$details$k_folds))
  expect_equal(res$details$train_split_ratio, 0.75)
  expect_true(is.na(res$details$n_strata))

  # Number of predictions should match test set size
  expect_equal(nrow(res$predictions), nrow(test_gcp) - floor(0.75 * nrow(test_gcp)))
  expect_true(is.na(res$summary$SD_RMSE_2D)) # No SD for single split
})

test_that("stratified works as expected", {
  test_gcp <- create_dummy_gcp_data(60)
  n_strata <- 4
  k_folds <- 5
  res <- cv_pai_model(test_gcp, "lm",
                      validation_type = "stratified",
                      n_strata = n_strata,
                      k_folds = k_folds)

  expect_equal(res$summary$ValidationType, "stratified")

  expect_true(is.na(res$details$train_split_ratio))

  expect_equal(res$details$n_strata, n_strata)
  expect_equal(res$details$k_folds, k_folds)
  expect_equal(nrow(res$predictions), nrow(test_gcp))
  expect_length(unique(res$predictions$fold), k_folds)

  expect_false(is.na(res$summary$SD_RMSE_2D))
})

test_that("falling back to simple random sampling...", {
  test_gcp <- create_dummy_gcp_data(10)
  test_gcp$dx <- 1
  test_gcp$dy <- 1

  expect_error(
    res <- cv_pai_model(test_gcp, "lm",
                        validation_type = "stratified",
                        train_split_ratio = 0.25),
    regexp = "Could not create sufficient strata from 'dx' and 'dy'"
  )

})

test_that("print method for CV results is correct", {

  test_gcp <- create_dummy_gcp_data(60)
  res <- cv_pai_model(test_gcp, "lm", validation_type = "random", k_folds = 5)

  output <- capture.output(print(res))

  expect_true(any(grepl("--- PAI Model Assessment Results ---", output)))
  expect_true(any(grepl("Validation Type:    random", output)))
  expect_true(any(grepl("Folds:              5", output)))
  expect_true(any(grepl("Model CV 2D RMSE:", output)))
  expect_true(any(grepl("Std Dev of RMSE:", output))) # Check for SD line
})

test_that("print method for single-split results is correct", {
  test_gcp <- create_dummy_gcp_data(60)
  res <- cv_pai_model(test_gcp, "lm", validation_type = "probability", train_split_ratio = 0.8)

  output <- capture.output(print(res))

  expect_true(any(grepl("Validation Type:    probability", output)))
  expect_true(any(grepl("Train/Test Split:   80% / 20%", output)))
  expect_false(any(grepl("Std Dev of RMSE:", output))) # Should NOT be a line for SD
})

test_that("print method for stratified results is correct", {
  test_gcp <- create_dummy_gcp_data(60)
  res <- cv_pai_model(test_gcp, "lm", validation_type = "stratified", n_strata = 3)

  output <- capture.output(print(res))

  expect_true(any(grepl("Validation Type:    stratified", output)))
  expect_true(any(grepl("Strata:", output)))
  expect_true(any(grepl("Std Dev of RMSE:", output)))
})
