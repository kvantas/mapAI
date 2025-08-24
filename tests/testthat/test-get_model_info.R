test_that("get_model_info works with built-in models", {
  model_info <- get_model_info("lm")
  expect_type(model_info, "list")
  expect_equal(model_info$label, "Linear Model")
  expect_equal(model_info$modelType, "univariate")
  expect_true(is.function(model_info$fit))
  expect_true(is.function(model_info$predict))
})

test_that("get_model_info works with custom model list", {

  custom_model <- list(
    label = "Custom Model",
    modelType = "univariate",
    fit = function(x, y) {
      lm(y ~ x)
    },
    predict = function(model, newdata) {
      predict(model, newdata)
    }
  )

  model_info <- get_model_info(custom_model)
  expect_type(model_info, "list")
  expect_equal(model_info$label, "Custom Model")
  expect_equal(model_info$modelType, "univariate")
  expect_true(is.function(model_info$fit))
  expect_true(is.function(model_info$predict))
})

test_that("get_model_info errors with invalid built-in model", {
  expect_error(get_model_info("nonexistent_model"),
               "Built-in model ' nonexistent_model ' not found. Available")
})

test_that("get_model_info errors with invalid custom model list", {
  invalid_model <- list(
    label = "Invalid Model",
    fit = function(x, y) { lm(y ~ x) }
    # Missing modelType and predict
  )
  expect_error(get_model_info(invalid_model),
               "Custom model list is missing required elements.")

  invalid_model2 <- list(
    label = "Invalid Model",
    modelType = "invalid_type",
    fit = function(x, y) { lm(y ~ x) },
    predict = function(model, newdata) { predict(model, newdata) }
  )
  expect_error(get_model_info(invalid_model2),
               "The 'modelType' element must be 'univariate' or 'bivariate'.")

  invalid_model3 <- list(
    label = "Invalid Model",
    modelType = "univariate",
    fit = "not_a_function",
    predict = function(model, newdata) { predict(model, newdata) }
  )
  expect_error(get_model_info(invalid_model3),
               "The 'fit' and 'predict' elements must be functions.")
})

test_that("get_model_info errors with invalid method type", {
  expect_error(get_model_info(123),
               "`method` must be a character string or a list.")
  expect_error(get_model_info(list("just", "a", "vector")),
               "Custom model list is missing required elements.")
})

