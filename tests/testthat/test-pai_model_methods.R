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
               "gcp_data must be an object of class 'gcp'.")
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
  model_rf <- train_pai_model(gcp, "rf")
  model_lm <- train_pai_model(gcp,  "lm")
  model_gam <- train_pai_model(gcp,  "gam_biv")
  model_tps <- train_pai_model(gcp, "tps")
  model_hlm <-train_pai_model(gcp,  "helmert")

  methods_to_test <- list(
    rf = model_rf,
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
  expect_error(
    train_pai_model(gcp, broken_univariate_model),
    "Failed to train the model for the dx component.\\n  Underlying error: This model intentionally fails."
  )
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
  expect_no_failure({
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
