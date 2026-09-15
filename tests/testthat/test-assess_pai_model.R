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

test_that("spatial_block and spatial_buffered CV work as expected", {
  test_gcp <- create_dummy_gcp_data(60)

  # Spatial block CV
  res_block <- cv_pai_model(test_gcp, method = "lm", validation_type = "spatial_block", k_folds = 4)
  expect_equal(res_block$summary$ValidationType, "spatial_block")
  expect_equal(res_block$details$k_folds, 4)
  expect_false(is.na(res_block$summary$Mean_RMSE_2D))
  expect_false(is.na(res_block$summary$SD_RMSE_2D))

  # Spatial buffered CV
  res_buf <- assess_pai_model(test_gcp, method = "lm", validation_type = "spatial_buffered", k_folds = 4, buffer_dist = 50)
  expect_equal(res_buf$summary$ValidationType, "spatial_buffered")
  expect_equal(res_buf$details$k_folds, 4)
  expect_equal(res_buf$details$buffer_dist, 50)
  expect_false(is.na(res_buf$summary$Mean_RMSE_2D))

  # Check print for spatial_buffered
  output <- capture.output(print(res_buf))
  expect_true(any(grepl("Buffer Distance:", output)))
})

test_that("integrated cross-validation in train_pai_model works seamlessly", {
  test_gcp <- create_dummy_gcp_data(50)

  # With cv = TRUE (defaults to spatial_block)
  m1 <- train_pai_model(test_gcp, method = "lm", cv = TRUE)
  expect_s3_class(m1, "pai_model")
  expect_false(is.null(m1$cv))
  expect_s3_class(m1$cv, "pai_assessment")
  expect_equal(m1$cv$summary$ValidationType, "spatial_block")
  expect_false(is.null(m1$cv_rmse_2d))

  # With custom cv list using buffered CV
  m2 <- train_pai_model(
    test_gcp,
    method = "lm",
    cv = list(validation_type = "spatial_buffered", k_folds = 3, buffer_dist = 40)
  )
  expect_equal(m2$cv$summary$ValidationType, "spatial_buffered")
  expect_equal(m2$cv$details$k_folds, 3)

  # Check print method displays CV summary
  out_m <- capture.output(print(m2))
  expect_true(any(grepl("Cross-Validation Assessment", out_m)))
})



#### Regression tests for the corrected cross-validation estimator ####

test_that("Mean_RMSE_2D pools residuals rather than averaging per-fold RMSEs", {
  gcp <- create_dummy_gcp_data(120)

  for (vt in c("random", "spatial", "spatial_block", "stratified")) {
    a <- suppressWarnings(suppressMessages(
      assess_pai_model(gcp, method = "lm", validation_type = vt,
                       k_folds = 4, seed = 7)
    ))
    p <- a$predictions
    pooled <- sqrt(mean((p$true_dx - p$pred_dx)^2 + (p$true_dy - p$pred_dy)^2))
    expect_equal(a$summary$Mean_RMSE_2D, pooled, tolerance = 1e-12,
                 info = vt)

    # The fold-mean is retained as a diagnostic but must not be the headline.
    expect_true(!is.null(a$details$fold_rmse))
    expect_equal(unname(a$summary$SD_RMSE_2D), sd(a$details$fold_rmse),
                 tolerance = 1e-12)
  }
})

test_that("spatial_block block size is independent of k_folds", {
  gcp <- create_dummy_gcp_data(300)

  # Raising k must not, on its own, change the block geometry; and a smaller
  # block_size must produce more, smaller blocks.
  s_small <- mapAI:::assign_spatial_blocks(gcp, k = 5, block_size = 100)
  s_large <- mapAI:::assign_spatial_blocks(gcp, k = 5, block_size = 400)
  expect_gt(length(unique(s_small$fold_ids)), 0)
  expect_equal(s_small$k, 5)
  expect_equal(s_large$k, 5)

  # With the default sizing, folds should be far more balanced than the old
  # "one tile per fold" behaviour produced.
  a <- suppressWarnings(suppressMessages(
    assess_pai_model(gcp, method = "lm", validation_type = "spatial_block",
                     k_folds = 5, seed = 42)
  ))
  sizes <- a$details$n_test_per_fold
  expect_equal(length(sizes), 5)
  expect_lt(max(sizes) / min(sizes), 3)
})

test_that("too few occupied blocks reduces k instead of creating empty folds", {
  gcp <- create_dummy_gcp_data(40)
  # Coordinates span ~1000 units; a 400-unit block gives a 3x3 grid, so far
  # fewer occupied blocks than the 30 folds requested.
  expect_warning(
    res <- mapAI:::assign_spatial_blocks(gcp, k = 30, block_size = 400),
    "fewer than k_folds"
  )
  expect_lt(res$k, 30)
  # Every realised fold must be non-empty.
  expect_true(all(tabulate(res$fold_ids, nbins = res$k) > 0))
})

test_that("an over-wide buffer is refused rather than silently re-admitting points", {
  gcp <- create_dummy_gcp_data(80)
  span <- max(diff(range(gcp$source_x)), diff(range(gcp$source_y)))

  # A buffer of this size prunes essentially the whole training set. Previously
  # this silently restored the 3 farthest excluded points and carried on.
  expect_error(
    suppressWarnings(suppressMessages(
      assess_pai_model(gcp, method = "lm", validation_type = "spatial_buffered",
                       k_folds = 5, buffer_dist = span * 0.9, seed = 1)
    )),
    "buffer pruned"
  )
})

test_that("non-finite predictions are reported, not silently dropped", {
  gcp <- create_dummy_gcp_data(60)

  na_model <- list(
    label = "NA Emitter", modelType = "bivariate", library = NULL,
    fit = function(dat, ...) list(),
    predict = function(modelFit, newdata, ...) {
      n <- nrow(newdata)
      dx <- rep(0, n); dy <- rep(0, n)
      dx[1] <- NA_real_
      data.frame(dx = dx, dy = dy)
    }
  )

  expect_warning(
    suppressMessages(
      assess_pai_model(gcp, method = na_model, validation_type = "random",
                       k_folds = 4, seed = 5)
    ),
    "not finite"
  )
})

test_that("assess_pai_model restores the caller's RNG state", {
  gcp <- create_dummy_gcp_data(60)

  set.seed(999)
  before <- .Random.seed
  suppressWarnings(suppressMessages(
    assess_pai_model(gcp, method = "lm", validation_type = "random",
                     k_folds = 3, seed = 42)
  ))
  expect_identical(.Random.seed, before)
})

test_that("a seed supplied inside the cv list is honoured", {
  gcp <- create_dummy_gcp_data(80)

  m1 <- suppressWarnings(suppressMessages(train_pai_model(
    gcp, method = "lm",
    cv = list(validation_type = "random", k_folds = 4, seed = 11))))
  m2 <- suppressWarnings(suppressMessages(train_pai_model(
    gcp, method = "lm",
    cv = list(validation_type = "random", k_folds = 4, seed = 11))))
  m3 <- suppressWarnings(suppressMessages(train_pai_model(
    gcp, method = "lm",
    cv = list(validation_type = "random", k_folds = 4, seed = 22))))

  expect_equal(m1$cv_rmse_2d, m2$cv_rmse_2d)
  expect_false(isTRUE(all.equal(m1$cv_rmse_2d, m3$cv_rmse_2d)))
})

test_that("unrecognised cv list elements are flagged", {
  gcp <- create_dummy_gcp_data(60)
  expect_warning(
    suppressMessages(train_pai_model(
      gcp, method = "lm",
      cv = list(validation_type = "random", k_folds = 3, nfolds = 10))),
    "unrecognised"
  )
})
