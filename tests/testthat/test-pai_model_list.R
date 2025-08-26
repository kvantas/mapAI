test_that("pai_model_list contains all expected models", {
  # Load internal data
  pai_model_list <- mapAI:::pai_model_list

  # Check if all expected models are present
  expected_models <- c("gam_biv", "helmert", "lm", "rf", "tps")
  expect_true(all(expected_models %in% names(pai_model_list)))

  # Check structure of each model
  for (model_name in names(pai_model_list)) {
    model <- pai_model_list[[model_name]]
    expect_type(model, "list")
    expect_true(all(c("label", "library", "modelType", "fit", "predict") %in% names(model)))
    expect_type(model$fit, "closure")
    expect_type(model$predict, "closure")
  }
})

test_that("all models can fit and predict with simulated data", {
  # Load internal data
  pai_model_list <- mapAI:::pai_model_list

  # Create simple test data
  test_data <- create_dummy_gcp_data(100)

  # Test each model
  for (model_name in names(pai_model_list)) {
    model <- pai_model_list[[model_name]]

    # Skip if required library is not available
    if (!is.null(model$library)) {
      if (!requireNamespace(model$library, quietly = TRUE)) {
        print(paste("Skipping", model_name, "model: required package", model$library, "not installed."))
        next
      }
    }

    test_that(sprintf("%s model can fit and predict", model_name), {
      # Fit model based on type
      if (model$modelType == "univariate") {
        # For univariate models, fit x and y separately
        expect_error({
          model_x <- model$fit(
            x = test_data[c("source_x", "source_y")],
            y = test_data$target_x
          )
          model_y <- model$fit(
            x = test_data[c("source_x", "source_y")],
            y = test_data$target_y
          )
        }, NA)

        # Test predictions
        expect_error({
          pred_x <- model$predict(model_x, test_data[c("source_x", "source_y")])
          pred_y <- model$predict(model_y, test_data[c("source_x", "source_y")])
        }, NA)

        # Check prediction dimensions
        expect_equal(length(pred_x), nrow(test_data))
        expect_equal(length(pred_y), nrow(test_data))

      } else if (model$modelType == "bivariate") {
        # For bivariate models, fit once for both coordinates
        expect_error({
          model_fit <- model$fit(test_data)
        }, NA)

        # Test predictions
        expect_error({
          preds <- model$predict(model_fit, test_data)
        }, NA)

        # Check prediction dimensions
        expect_equal(nrow(preds), nrow(test_data))
        expect_equal(ncol(preds), 2)
      }
    })
  }
})

