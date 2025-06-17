gcp_data <- read_gcps(gcp_path = DEMO_FILES$gcp_path, crs = 3857)

test_that("train_pai_model() creates valid models", {
  for (method in c("rf", "lm", "gam")) {
    model <- train_pai_model(gcp_data, method = method)
    expect_s3_class(model, "pai_model")
    expect_named(model, c("model", "method"))
    expect_equal(model$method, method)
  }
})

test_that("train_pai_model() internal model classes are correct", {
  model_rf <- train_pai_model(gcp_data, method = "rf")
  expect_s3_class(model_rf$model$model_dx, "ranger")

  model_lm <- train_pai_model(gcp_data, method = "lm")
  expect_s3_class(model_lm$model$model_dx, "lm")

  model_gam <- train_pai_model(gcp_data, method = "gam")
  expect_s3_class(model_gam$model, "gam")
})
