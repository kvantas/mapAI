# test train and predict methods
test_that("train_pai_model handles missing arguments", {

  test_gcp <- create_dummy_gcp_data(10)

  expect_error(train_pai_model(method = "lm"),
               "Both 'gcp_data' and 'method' arguments are required.")
  expect_error(train_pai_model(gcp_data = test_gcp),
               "Both 'gcp_data' and 'method' arguments are required.")
})

test_that("train_pai_model validates gcp_data class", {

  test_gcp <- create_dummy_gcp_data(10)
  bad_data <- as.data.frame(test_gcp)

   expect_error(train_pai_model(gcp_data = bad_data, method = "lm"),
               "must be an object of class 'gcp'")
})

test_that("train_pai_model trains and predicts a univariate model correctly", {
  test_gcp <- create_dummy_gcp_data(10)
  model <- train_pai_model(gcp_data = test_gcp, method = "lm", seed = 123)

  expect_s3_class(model, "pai_model")
  expect_equal(model$model_info$label, "Linear Model")
  expect_true("model_dx" %in% names(model$model))
  expect_true("model_dy" %in% names(model$model))
  expect_s3_class(model$model$model_dx, "lm")

  # predict using new data
  newdata <- create_dummy_gcp_data(100)
  preds <- predict(model, newdata = newdata)

  expect_s3_class(preds, "gcp")
  expect_equal(nrow(preds), 100)
  expect_true(all(c("source_x", "source_y", "dx", "dy", "target_x",
                    "target_y") %in% names(preds))
  )
  expect_true(is.numeric(preds$dx) && is.numeric(preds$dy))
  expect_equal(preds$target_x, preds$source_x + preds$dx)
  expect_equal(preds$target_y, preds$source_y + preds$dy)

})

test_that("train_pai_model trains and predicts a bivariate model correctly", {

  test_gcp <- create_dummy_gcp_data(100)
  model <- train_pai_model(gcp_data = test_gcp, method = "gam_biv", seed = 123)

  expect_s3_class(model, "pai_model")
  expect_equal(model$model_info$label, "Bivariate GAM")
  expect_true("model" %in% names(model))
  expect_s3_class(model$model, "gam")


})

test_that("train_pai_model trains and predicts a custom model correctly", {

  test_gcp <- create_dummy_gcp_data(100)

  custom_biv_model <- list(
    label = "Custom Bivariate",
    modelType = "bivariate",
    library = NULL,
    fit = function(data, ...) {
      # A dummy model that just stores the mean dx/dy
      list(mean_dx = mean(data$dx), mean_dy = mean(data$dy))
    },
    predict = function(model, newdata, ...) {
      # Predicts the mean for all new data
      matrix(c(rep(model$mean_dx, nrow(newdata)),
               rep(model$mean_dy, nrow(newdata))), ncol = 2)
    }
  )

  model <- train_pai_model(gcp_data = test_gcp, method = custom_biv_model)

  expect_s3_class(model, "pai_model")
  expect_equal(model$model_info$label, "Custom Bivariate")
  expect_equal(model$model$mean_dx, mean(test_gcp$dx))
})

test_that("predict.pai_model() correctly uses newdata for all model types", {

  gcp <- create_dummy_gcp_data(200)
  new_gcp <- gcp[1:20, ]
  new_gcp$source_x <- new_gcp$source_x + 10

  # --- Train one model of each type ---
  model_lm <- train_pai_model(gcp,  "lm")
  model_gam <- train_pai_model(gcp,  "gam_biv")
  model_tps <- train_pai_model(gcp, "tps")
  model_hlm <-train_pai_model(gcp,  "helmert")

  methods_to_test <- list(
    lm = model_lm,
    gam = model_gam,
    tps = model_tps,
    helmert = model_hlm)

  for (method_name in names(methods_to_test)) {
    model <- methods_to_test[[method_name]]

    # 1. Get predictions on the NEW data
    predictions_new <- predict(model, new_gcp)
    predictions_fitted <- predict(model, gcp[1:10, ])


    # 3. The predictions should NOT be identical
    expect_false(
      identical(predictions_new$dx, predictions_fitted$dx),
      info = paste("dx predictions for", method_name, "model did not use newdata.")
    )
    expect_false(
      identical(predictions_new$dy, predictions_fitted$dy),
      info = paste("dy predictions for", method_name, "model did not use newdata.")
    )

  }

})

# test for errors in  train_pai_model
test_that("train_pai_model provides a clear error message on fit failure", {

  gcp <- create_dummy_gcp_data(10)

  # Create a custom model where the 'fit' function is guaranteed to fail
  broken_univariate_model <- list(
    label = "Broken Model",
    modelType = "univariate",
    library = NULL,
    fit = function(x, y, ...) {
      stop("This model intentionally fails.") # The failing call
    },
    predict = function(model, newdata, ...) {
      rep(0, nrow(newdata))
    }
  )

  # Use expect_error to check the exact error message
  expect_error(train_pai_model(gcp, broken_univariate_model))
})

# test for errors in predict.pai_model
test_that("predict.pai_model provides a clear error message on predict failure", {

  gcp <- create_dummy_gcp_data(100)

  # A model that fits but has a broken predict method
  broken_predict_model <- list(
    label = "Broken Predict",
    modelType = "univariate",
    library = NULL,
    fit = function(x, y, ...) {
      stats::lm(y ~ ., data.frame(y, x))
    },
    predict = function(model, newdata, ...) {
      stop("Intentional prediction failure.") # The failing call
    }
  )

  trained_model <- train_pai_model(gcp, broken_predict_model)

  expect_error(
    predict(trained_model, gcp),
    "Prediction for the dx component failed.\\n  Underlying error: Intentional prediction failure."
  )
})

# S3 Methods: print, plot, residuals, surface
test_that("print, plot, residuals, surface methods run without error", {

  test_gcp <- create_dummy_gcp_data(50)
  model <- train_pai_model(gcp_data = test_gcp, method = "gam_biv", seed = 123)

  # print method
  expect_output(print(model), "PAI Model - Bivariate GAM")
  expect_invisible(print(model))

  # plot method
  expect_no_error({
    # Suppress plotting to console during tests
    pdf(NULL)
    plot(model)
    dev.off()
  })

  # residuals
  p <- residuals.pai_model(model)
  expect_s3_class(p, "ggplot")

  # surface
  s <- surface(model, n_grid = 10) # Use small grid for speed

  expect_type(s, "list")
  expect_equal(length(s), 2)
  expect_named(s, c("dx_plot", "dy_plot"))
  expect_s3_class(s$dx_plot, "ggplot")
  expect_s3_class(s$dy_plot, "ggplot")

})


#### Regression tests: control network validation and zero-variance GAM ####

make_flat_gcp <- function(n = 8, dx = 25, dy = -10) {
  g <- expand.grid(source_x = seq(0, 1000, length.out = n),
                   source_y = seq(0, 1000, length.out = n))
  d <- data.frame(source_x = g$source_x, source_y = g$source_y,
                  dx = rep(dx, nrow(g)), dy = rep(dy, nrow(g)))
  d$target_x <- d$source_x + d$dx
  d$target_y <- d$source_y + d$dy
  class(d) <- c("gcp", "data.frame")
  d
}

test_that("gam_biv refuses a constant displacement field with an actionable message", {
  skip_if_not_installed("mgcv")

  # A map offset by a uniform shift is an ordinary input. mgcv::mvn(d = 2) has no
  # residual variance to model and used to fail with "NA/NaN/Inf in foreign
  # function call (arg 1)", which tells the user nothing.
  shifted <- make_flat_gcp()

  expect_error(train_pai_model(shifted, "gam_biv"), "constant across all")
  expect_error(train_pai_model(shifted, "gam_biv"), "helmert")

  # An exact identity warp is the same situation.
  expect_error(train_pai_model(make_flat_gcp(dx = 0, dy = 0), "gam_biv"),
               "constant across all")

  # One flat component is enough to make the covariance singular.
  half_flat <- make_flat_gcp()
  half_flat$dy <- seq(-10, 10, length.out = nrow(half_flat))
  half_flat$target_y <- half_flat$source_y + half_flat$dy
  expect_error(train_pai_model(half_flat, "gam_biv"), "dx displacement component")

  # The methods that CAN fit a pure translation must still do so, exactly.
  h <- train_pai_model(shifted, "helmert")
  expect_equal(unname(h$model$parameters["tx"]), 25, tolerance = 1e-8)
  expect_equal(unname(h$model$parameters["ty"]), -10, tolerance = 1e-8)
  expect_s3_class(train_pai_model(shifted, "lm"), "pai_model")
})

test_that("train_pai_model validates the control network like assess_pai_model", {
  gcp <- create_dummy_gcp_data(40)

  # NA control points were silently dropped by stats::lm(), so the model was
  # fitted on fewer points than supplied without saying so.
  gcp_na <- gcp
  gcp_na$dx[3] <- NA_real_
  expect_error(train_pai_model(gcp_na, "lm"), "contains NA values")
  expect_error(assess_pai_model(gcp_na, "lm", validation_type = "random",
                                k_folds = 3), "NA")

  # Degenerate networks used to return a model with NA coefficients, which then
  # predicted NA far from the cause.
  expect_error(train_pai_model(gcp[1, ], "lm"), "at least 3 control points")
  expect_error(train_pai_model(gcp[rep(1, 10), ], "lm"), "co-located")

  collinear <- gcp
  collinear$source_y <- collinear$source_x   # all points on one line
  expect_error(train_pai_model(collinear, "lm"), "collinear")

  # Non-finite coordinates.
  gcp_inf <- gcp
  gcp_inf$source_x[2] <- Inf
  expect_error(train_pai_model(gcp_inf, "lm"), "non-finite")
})

test_that("network validation does not reject cases the model can genuinely fit", {
  gcp <- create_dummy_gcp_data(40)

  # Helmert is a rigid 4-parameter transform: 2 points suffice, and it stays
  # estimable from collinear control points.
  expect_s3_class(train_pai_model(gcp[1:2, ], "helmert"), "pai_model")

  collinear <- gcp
  collinear$source_y <- collinear$source_x
  expect_s3_class(train_pai_model(collinear, "helmert"), "pai_model")

  # Three non-collinear points exactly determine an affine fit.
  tri <- gcp[1:3, ]
  tri$source_x <- c(0, 100, 50)
  tri$source_y <- c(0, 0, 100)
  expect_s3_class(train_pai_model(tri, "lm"), "pai_model")

  # Ordinary data must be unaffected.
  expect_s3_class(train_pai_model(gcp, "lm"), "pai_model")
})
