# Tests for the S3 predict method: predict.pai_model()

# --- Setup for all tests in this file ---
gcp_data <- read_gcps(gcp_path = DEMO_FILES$gcp_path, crs = 3857)

# Create a 'newdata' set that is different from the training data
new_gcp_data <- gcp_data[1:10, ]
new_gcp_data$source_x <- new_gcp_data$source_x + 10 # Alter coordinates

# --- Train one model of each type ---
model_rf <- train_pai_model(gcp_data, method = "rf")
model_lm <- train_pai_model(gcp_data, method = "lm")
model_gam <- train_pai_model(gcp_data, method = "gam")
model_tps <- train_pai_model(gcp_data, method = "tps")


test_that("predict.pai_model() returns a correctly structured data frame", {
  predictions <- predict(model_rf, newdata = sf::st_drop_geometry(new_gcp_data))

  expect_s3_class(predictions, "data.frame")
  expect_named(predictions, c("dx", "dy"))
  expect_equal(nrow(predictions), nrow(new_gcp_data))
})

test_that("predict.pai_model() correctly uses newdata for all model types", {
  # This is the most critical test. It ensures that the function is not just
  # returning the fitted values from the training data.

  methods_to_test <- list(rf = model_rf, lm = model_lm, gam = model_gam, tps = model_tps)

  for (method_name in names(methods_to_test)) {
    model <- methods_to_test[[method_name]]

    # 1. Get predictions on the NEW data
    predictions_new <- predict(model, newdata = sf::st_drop_geometry(new_gcp_data))

    # 2. Get predictions on the ORIGINAL training data (by not supplying newdata)
    # We must explicitly subset the original data to match the size for a fair comparison.
    predictions_fitted <- predict(model, newdata = sf::st_drop_geometry(gcp_data[1:10, ]))

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

test_that("predict.pai_model() handles NA values gracefully", {
  # It should return NAs for rows with NA predictors
  bad_data <- new_gcp_data
  bad_data$source_x[1] <- NA

  predictions_na <- predict(model_rf, newdata = sf::st_drop_geometry(bad_data))

  # The row corresponding to the NA input should have an NA output
  expect_true(is.na(predictions_na$dx[1]))
  expect_true(is.na(predictions_na$dy[1]))
})
