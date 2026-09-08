# Tests for new smooth non-linear ML methods: gp, gamboost, torch

test_that("train_pai_model and predict work with Gaussian Process ('gp')", {
  withr::with_tempdir({
    demo_files <- create_demo_data(output_dir = ".")
    gcp_data <- read_gcps(gcp_path = demo_files$gcp_path)

    # Train GP model
    gp_model <- train_pai_model(gcp_data, pai_method = "gp")
    expect_s3_class(gp_model, "pai_model")
    expect_equal(gp_model$method, "gp")
    expect_s3_class(gp_model$model$model_dx, "spatialProcess")
    expect_s3_class(gp_model$model$model_dy, "spatialProcess")

    # Predict with new data
    new_data <- sf::st_drop_geometry(gcp_data[1:10, ])
    new_data$source_x <- new_data$source_x + 5
    preds <- predict(gp_model, newdata = new_data)
    expect_s3_class(preds, "data.frame")
    expect_named(preds, c("dx", "dy"))
    expect_equal(nrow(preds), 10)
    expect_true(all(!is.na(preds$dx)))
    expect_true(all(!is.na(preds$dy)))

    # NA handling in assess_pai_model
    gcp_na <- gcp_data
    gcp_na$dx[1] <- NA
    expect_warning(
      suppressMessages(assess_pai_model(gcp_na, pai_method = "gp", validation_type = "random", k_folds = 2)),
      "row(s) with missing values were removed before assessment",
      fixed = TRUE
    )
  })
})

test_that("analyze_distortion works smoothly with 'gp' model", {
  withr::with_tempdir({
    demo_files <- create_demo_data(output_dir = ".")
    gcp_data <- read_gcps(gcp_path = demo_files$gcp_path)
    gp_model <- train_pai_model(gcp_data, pai_method = "gp")

    pts <- sf::st_as_sf(
      data.frame(
        id = 1:4,
        x = c(500050, 500100, 500050, 500100),
        y = c(4200050, 4200050, 4200100, 4200100)
      ),
      coords = c("x", "y"),
      crs = sf::st_crs(gcp_data)
    )

    # analyze_distortion should run without warning about non-differentiability
    dist_res <- suppressMessages(analyze_distortion(gp_model, pts))
    expect_s3_class(dist_res, "sf")
    expect_true("area_scale" %in% names(dist_res))
    expect_true("max_shear" %in% names(dist_res))
    expect_true(all(is.finite(dist_res$area_scale)))
  })
})

test_that("assess_pai_model works with 'gp'", {
  withr::with_tempdir({
    demo_files <- create_demo_data(output_dir = ".")
    gcp_data <- read_gcps(gcp_path = demo_files$gcp_path)

    # Random 2-fold CV
    res_rand <- suppressMessages(
      assess_pai_model(gcp_data, pai_method = "gp", validation_type = "random", k_folds = 2, seed = 42)
    )
    expect_s3_class(res_rand, "data.frame")
    expect_equal(res_rand$Method, "gp")
    expect_true(is.numeric(res_rand$Mean_RMSE_2D))
    expect_true(res_rand$Mean_RMSE_2D > 0)
  })
})

test_that("train_pai_model and predict work with Boosted Splines ('gamboost')", {
  skip_if_not_installed("mboost")

  withr::with_tempdir({
    demo_files <- create_demo_data(output_dir = ".")
    gcp_data <- read_gcps(gcp_path = demo_files$gcp_path)

    gb_model <- train_pai_model(gcp_data, pai_method = "gamboost", mstop = 50)
    expect_s3_class(gb_model, "pai_model")
    expect_equal(gb_model$method, "gamboost")
    expect_s3_class(gb_model$model$model_dx, "gamboost")

    new_data <- sf::st_drop_geometry(gcp_data[1:10, ])
    preds <- predict(gb_model, newdata = new_data)
    expect_s3_class(preds, "data.frame")
    expect_named(preds, c("dx", "dy"))
    expect_equal(nrow(preds), 10)
    expect_true(all(!is.na(preds$dx)))

    # assess_pai_model
    res_gb <- suppressMessages(
      assess_pai_model(gcp_data, pai_method = "gamboost", validation_type = "random", k_folds = 2, mstop = 30)
    )
    expect_s3_class(res_gb, "data.frame")
    expect_equal(res_gb$Method, "gamboost")
  })
})

test_that("train_pai_model and predict work with Neural Network ('torch')", {
  skip_if_not_installed("torch")

  withr::with_tempdir({
    demo_files <- create_demo_data(output_dir = ".")
    gcp_data <- read_gcps(gcp_path = demo_files$gcp_path)

    torch_model <- train_pai_model(gcp_data, pai_method = "torch", epochs = 20, hidden_units = c(16, 16))
    expect_s3_class(torch_model, "pai_model")
    expect_equal(torch_model$method, "torch")
    expect_s3_class(torch_model$model$net, "nn_module")

    new_data <- sf::st_drop_geometry(gcp_data[1:10, ])
    preds <- predict(torch_model, newdata = new_data)
    expect_s3_class(preds, "data.frame")
    expect_named(preds, c("dx", "dy"))
    expect_equal(nrow(preds), 10)
    expect_true(all(!is.na(preds$dx)))

    # assess_pai_model
    res_nn <- suppressMessages(
      assess_pai_model(gcp_data, pai_method = "torch", validation_type = "random", k_folds = 2, epochs = 10)
    )
    expect_s3_class(res_nn, "data.frame")
    expect_equal(res_nn$Method, "torch")
  })
})

test_that("missing suggested packages produce informative errors", {
  withr::with_tempdir({
    demo_files <- create_demo_data(output_dir = ".")
    gcp_data <- read_gcps(gcp_path = demo_files$gcp_path)

    if (!requireNamespace("mboost", quietly = TRUE)) {
      expect_error(
        train_pai_model(gcp_data, pai_method = "gamboost"),
        "The 'mboost' package is required",
        fixed = TRUE
      )
    }

    if (!requireNamespace("torch", quietly = TRUE)) {
      expect_error(
        train_pai_model(gcp_data, pai_method = "torch"),
        "The 'torch' package is required",
        fixed = TRUE
      )
    }
  })
})
