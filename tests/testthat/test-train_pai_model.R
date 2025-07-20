test_that("train_pai_model() creates valid models", {
  withr::with_tempdir({
    demo_files <- create_demo_data(output_dir = ".")
    gcp_data <- read_gcps(gcp_path = demo_files$gcp_path)
    for (pai_method in c("rf", "lm", "gam", "helmert", "tps")) {
      model <- train_pai_model(gcp_data, pai_method = pai_method)
      expect_s3_class(model, "pai_model")
      expect_named(model, c("model", "method"))
      expect_equal(model$method, pai_method)
    }
  })
})

test_that("train_pai_model() internal model classes are correct", {
  withr::with_tempdir({
    demo_files <- create_demo_data(output_dir = ".")
    gcp_data <- read_gcps(gcp_path = demo_files$gcp_path)

    model_rf <- train_pai_model(gcp_data, pai_method = "rf")
    expect_s3_class(model_rf$model$model_dx, "ranger")

    model_lm <- train_pai_model(gcp_data, pai_method = "lm")
    expect_s3_class(model_lm$model$model_dx, "lm")

    model_gam <- train_pai_model(gcp_data, pai_method = "gam")
    expect_s3_class(model_gam$model, "gam")

    model_tps <- train_pai_model(gcp_data, pai_method = "tps")
    expect_equal(class(model_tps$model$model_dx), c("Krig", "Tps"))
  })
})

# TEST 0: helmert pai_method
test_that("train_pai_model works with pai_method = 'helmert'", {
  withr::with_tempdir({
    demo_files <- create_demo_data(output_dir = ".")
    gcp_data <- read_gcps(gcp_path = demo_files$gcp_path)
    model_h <- train_pai_model(gcp_data, pai_method = "helmert")

    expect_s3_class(model_h, "pai_model")
    expect_equal(model_h$method, "helmert")
    expect_equal(length(model_h$model$coefficients), 2)
    expect_equal(length(model_h$model$centroids), 4)
  })
})

# Test 1: lm pai_method
test_that("train_pai_model works with pai_method = 'lm'", {
  withr::with_tempdir({
    demo_files <- create_demo_data(output_dir = ".")
    gcp_data <- read_gcps(gcp_path = demo_files$gcp_path)
    model_lm <- train_pai_model(gcp_data, pai_method = "lm")

    expect_s3_class(model_lm, "pai_model")
    expect_equal(model_lm$method, "lm")
    expect_s3_class(model_lm$model$model_dx, "lm")
    expect_s3_class(model_lm$model$model_dy, "lm")
  })
})

# Test 2: rf pai_method
test_that("train_pai_model works with pai_method = 'rf'", {
  withr::with_tempdir({
    demo_files <- create_demo_data(output_dir = ".")
    gcp_data <- read_gcps(gcp_path = demo_files$gcp_path)
    model_rf <- train_pai_model(gcp_data, pai_method = "rf")

    expect_s3_class(model_rf, "pai_model")
    expect_equal(model_rf$method, "rf")
    expect_s3_class(model_rf$model$model_dx, "ranger")
    expect_s3_class(model_rf$model$model_dy, "ranger")
  })
})

# Test 3: gam pai_method
test_that("train_pai_model works with pai_method = 'gam'", {
  withr::with_tempdir({
    demo_files <- create_demo_data(output_dir = ".")
    gcp_data <- read_gcps(gcp_path = demo_files$gcp_path)
    model_gam <- train_pai_model(gcp_data, pai_method = "gam")

    expect_s3_class(model_gam, "pai_model")
    expect_equal(model_gam$method, "gam")
    expect_s3_class(model_gam$model, "gam")
  })
})

# Test 3.1: tps pai_method
test_that("train_pai_model works with pai_method = 'tps'", {
  withr::with_tempdir({
    demo_files <- create_demo_data(output_dir = ".")
    gcp_data <- read_gcps(gcp_path = demo_files$gcp_path)
    model_gam <- train_pai_model(gcp_data, pai_method = "tps")

    expect_s3_class(model_gam, "pai_model")
    expect_equal(model_gam$method, "tps")
  })
})

# Test 4: seed parameter for reproducibility
test_that("train_pai_model produces reproducible results with seed (rf pai_method)", {
  withr::with_tempdir({
    demo_files <- create_demo_data(output_dir = ".")
    gcp_data <- read_gcps(gcp_path = demo_files$gcp_path)

    model1 <- train_pai_model(gcp_data, pai_method = "rf", seed = 42)
    model2 <- train_pai_model(gcp_data, pai_method = "rf", seed = 42)
    model3 <- train_pai_model(gcp_data, pai_method = "rf", seed = 100)

    # For rf, compare prediction error or other reproducible metrics
    # Note: ranger models might not have a direct 'coef' equivalent for comparison.
    # Comparing prediction error is a good proxy for reproducibility.
    expect_equal(model1$model$model_dx$prediction.error, model2$model$model_dx$prediction.error)
    expect_equal(model1$model$model_dy$prediction.error, model2$model$model_dy$prediction.error)

    # Ensure different seed produces different results (highly probable)
    expect_false(isTRUE(all.equal(model1$model$model_dx$prediction.error,
                                  model3$model$model_dx$prediction.error)))
  })
})

# Test 5: Handling of invalid pai_method return error
test_that("train_pai_model returns NULL models for invalid pai_method", {
  withr::with_tempdir({
    demo_files <- create_demo_data(output_dir = ".")
    gcp_data <- read_gcps(gcp_path = demo_files$gcp_path)
    expect_error( train_pai_model(gcp_data, pai_method = "invalid_method"))
  })
})

# Test 6: Passing additional arguments via ... for lm
test_that("train_pai_model passes additional arguments to lm via ...", {
  withr::with_tempdir({
    demo_files <- create_demo_data(output_dir = ".")
    gcp_data <- read_gcps(gcp_path = demo_files$gcp_path)
    # Pass 'weights' argument to lm
    weights_data <- runif(nrow(gcp_data), 0.1, 1)
    model_lm_weighted <- train_pai_model(gcp_data, pai_method = "lm", weights = weights_data)

    expect_s3_class(model_lm_weighted, "pai_model")
    expect_equal(model_lm_weighted$method, "lm")
    expect_s3_class(model_lm_weighted$model$model_dx, "lm")
    expect_s3_class(model_lm_weighted$model$model_dy, "lm")
    expect_true("weights" %in% names(model_lm_weighted$model$model_dx$call))
    expect_true("weights" %in% names(model_lm_weighted$model$model_dy$call))
  })
})

# Test 7: Passing additional arguments via ... for rf
test_that("train_pai_model passes additional arguments to rf via ...", {
  withr::with_tempdir({
    demo_files <- create_demo_data(output_dir = ".")
    gcp_data <- read_gcps(gcp_path = demo_files$gcp_path)
    # Pass 'min.node.size' argument to ranger
    model_rf_min_node <- train_pai_model(gcp_data, pai_method = "rf", min.node.size = 2)

    expect_s3_class(model_rf_min_node, "pai_model")
    expect_equal(model_rf_min_node$method, "rf")
    expect_s3_class(model_rf_min_node$model$model_dx, "ranger")
    expect_s3_class(model_rf_min_node$model$model_dy, "ranger")
    expect_equal(model_rf_min_node$model$model_dx$min.node.size, 2)
    expect_equal(model_rf_min_node$model$model_dx$min.node.size, 2)
  })
})

# Test 8: Passing additional arguments via ... for gam
test_that("train_pai_model passes additional arguments to gam via ...", {
  withr::with_tempdir({
    demo_files <- create_demo_data(output_dir = ".")
    gcp_data <- read_gcps(gcp_path = demo_files$gcp_path)

    # let's test a top-level argument that gam accepts, e.g., gamma
    model_gam_gamma <- train_pai_model(gcp_data, pai_method = "gam", gamma = 3)

    expect_s3_class(model_gam_gamma, "pai_model")
    expect_equal(model_gam_gamma$method, "gam")
    expect_s3_class(model_gam_gamma$model, "gam")

    # Check if the gamma argument was passed
    expect_true("gamma" %in% names(model_gam_gamma$model$call))
  })
})

# Test 8.1: Passing additional arguments via ... for tps
test_that("train_pai_model passes additional arguments to tps via ...", {
  withr::with_tempdir({
    demo_files <- create_demo_data(output_dir = ".")
    gcp_data <- read_gcps(gcp_path = demo_files$gcp_path)

    # let's test a top-level argument that tps accepts, e.g., pai_method
    model_tps <- train_pai_model(gcp_data, pai_method = "tps", GCV = FALSE)

    expect_equal(model_tps$method, "tps")

    # Check if the GCV argument was passed
    expect_true("GCV" %in% names(model_tps$model$model_dx$call))
  })
})


# Test 9: Handling < 60 points with nonlinear methods
test_that("return error with less that 60 points", {
  withr::with_tempdir({
    demo_files <- create_demo_data(output_dir = ".")
    gcp_data <- read_gcps(gcp_path = demo_files$gcp_path)
    expect_error(train_pai_model(gcp_data[1:50, ], pai_method = "gam" ))
  })
})

# test 10: Return error with wrong input data
test_that("gcp_data must be an sf object",{
  expect_error(
    train_pai_model(gcp_data = list(), pai_method = "rf")
  )
})
