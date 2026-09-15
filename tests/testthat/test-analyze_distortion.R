#### analyze_distortion function unit tests ####

test_that("input validation works correctly", {

  gcp <- create_dummy_gcp_data(50)
  model <- train_pai_model(gcp, "lm")

  expect_error(analyze_distortion(pai_model = "not a model"),
               "`pai_model` must be an object of class 'pai_model'.")

  expect_error(analyze_distortion(pai_model = model, newdata = 1:3),
               "`newdata` must contain 'source_x' and 'source_y' columns.")

  expect_error(analyze_distortion(pai_model = model, reference_scale = 0),
               "`reference_scale` must be a single positive numeric value.")
})

test_that("output has the correct structure and class", {

  gcp <- create_dummy_gcp_data(50)
  model <- train_pai_model(gcp, "lm")

  res <- analyze_distortion(model, gcp)

  expect_s3_class(res, "distortion")
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), nrow(gcp))

  expected_cols <- c("source_x", "source_y", "a", "b", "area_scale",
                     "theta_a")
  expect_true(all(expected_cols %in% names(res)))
  expect_true(all(is.numeric(res$a)))
})

test_that("function defaults to model's gcp data when input is NULL", {

  gcp <- create_dummy_gcp_data(50)
  model <- train_pai_model(gcp, "lm")

  res_default <- analyze_distortion(model)
  expect_equal(nrow(res_default), nrow(model$gcp))
})

test_that("numerical derivatives match analytical derivatives", {
  # This is the most critical test. It ensures the numerical method is accurate.

  # Known Transformation:
  #   target_x = 2*sx + 0.5*sy + 10
  #   target_y = 0.1*sx + 1.5*sy + 20
  #
  # Analytical Derivatives:
  #   dfx_dx = 2.0, dfy_dx = 0.1
  #   dfx_dy = 0.5, dfy_dy = 1.5

  affine_model <- list(
    label = "Known Affine",
    modelType = "bivariate",
    library = NULL,

    # The 'fit' function is trivial as the transformation is fixed.
    # It just needs to return a model object for the predict function to use.
    fit = function(gcp_data, ...) {
      return(list(transform_type = "fixed_affine"))
    },

    # The 'predict' function applies the known analytical transformation.
    # It must return a 2-column matrix of displacements (dx, dy).
    predict = function(model, newdata, ...) {
      # Calculate the final target coordinates based on the known formula
      pred_target_x <- 2 * newdata$source_x + 0.5 * newdata$source_y + 10
      pred_target_y <- 0.1 * newdata$source_x + 1.5 * newdata$source_y + 20

      # Calculate the displacements (the difference from the source)
      dx <- pred_target_x - newdata$source_x
      dy <- pred_target_y - newdata$source_y

      return(cbind(dx, dy))
    }
  )
  model <- train_pai_model(create_dummy_gcp_data(10), method = affine_model)
  test_points <- create_dummy_gcp_data(10)
  res <- analyze_distortion(model, test_points)

  # Calculate the analytical solution for Tissot's indicatrix parameters
  dfx_dx <- 2.0
  dfy_dx <- 0.1
  dfx_dy <- 0.5
  dfy_dy <- 1.5

  E <- dfx_dx^2 + dfy_dx^2
  G <- dfx_dy^2 + dfy_dy^2
  F_metric <- dfx_dx * dfx_dy + dfy_dx * dfy_dy
  sqrt_term <- sqrt((E - G)^2 + 4 * F_metric^2)

  a_analytical <- sqrt(0.5 * (E + G + sqrt_term))
  b_analytical <- sqrt(0.5 * (E + G - sqrt_term))
  area_scale_analytical <- a_analytical * b_analytical

  # Compare the function's numerical results to the analytical solution.
  # We use a tolerance for floating-point inaccuracies.
  n <- nrow(res)
  expect_equal(res$a, rep(a_analytical, n), tolerance = 1e-6)
  expect_equal(res$b, rep(b_analytical, n), tolerance = 1e-6)
  expect_equal(res$area_scale, rep(area_scale_analytical, n), tolerance = 1e-6)
})

test_that("Correctly identifies an identity transformation (no distortion)", {

  # An identity transformation should yield a=1, b=1, area_scale=1, and zero
  # angular distortion everywhere.
  identity_model <- list(
    label = "Identity Transformation",
    modelType = "bivariate",
    library = NULL,

    fit = function(gcp_data, ...) {
      return(list(transform_type = "identity"))
    },

    predict = function(model, newdata, ...) {
      n <- nrow(newdata)
      # Identity transformation: target coordinates are the same as source

      # Calculate the displacements (the difference from the source)
      dx <- rep(0, n)
      dy <- rep(0, n)

      return(cbind(dx, dy))
    }
  )

  model <- train_pai_model(create_dummy_gcp_data(10), method = identity_model)
  res <-analyze_distortion(model, create_dummy_gcp_data(10))

  TOLERANCE  <- 10^-6
  n <- nrow(res)

  expect_equal(res$a, rep(1, n), tolerance = TOLERANCE)
  expect_equal(res$b, rep(1, n), tolerance = TOLERANCE)
  expect_equal(res$area_scale, rep(1, n), tolerance = TOLERANCE)
})

#### distortion object methods unit tests ####

# --- Tests for print.distortion ---
test_that("print.distortion prints key information", {

  model <- train_pai_model(create_dummy_gcp_data(20), method = "lm")
  res <-analyze_distortion(model, create_dummy_gcp_data(20))

  expect_output(print(res), "Distortion Analysis Results")
  expect_output(print(res), "Number of Points Analyzed: 20")
  expect_output(print(res), "Metrics Included:")
  expect_invisible(print(res))

})

# --- Tests for summary.distortion ---
test_that("summary.distortion handles invalid input", {

  model <- train_pai_model(create_dummy_gcp_data(20), method = "lm")
  res <-analyze_distortion(model, create_dummy_gcp_data(20))

  expect_equal(colnames(summary(res)), c("Mean", "Median", "SD", "Min", "Max" ))

  expect_equal(rownames(summary(res)), c("a", "b", "area_scale", "signed_area_scale",
                                         "det_J", "is_inverted",
                                         "log2_area_scale",
                                         "max_angular_distortion",
                                         "airy_kavrayskiy", "theta_a"))
})

test_that("analyze_distortion() detects topological fold-overs and negative Jacobian determinants", {
  gcp <- create_dummy_gcp_data(10)
  # Model with reflection: target_x = -source_x (det_J = -1)
  refl_model <- list(
    label = "Reflection Model",
    modelType = "bivariate",
    library = NULL,
    fit = function(...) list(),
    predict = function(modelFit, newdata, ...) {
      cbind(-2 * newdata$source_x, rep(0, nrow(newdata)))
    }
  )
  mod <- train_pai_model(gcp, refl_model)

  expect_warning(
    res <- analyze_distortion(mod, gcp),
    "Topological fold-over detected"
  )

  expect_true(all(res$det_J < 0))
  expect_true(all(res$is_inverted))
  expect_equal(res$signed_area_scale, res$det_J)
  expect_equal(res$area_scale, abs(res$det_J), tolerance = 1e-5)
})

# --- Tests for plot.distortion ---
test_that("plot.distortion handles invalid inputs", {

  model <- train_pai_model(create_dummy_gcp_data(20), method = "lm")
  res <-analyze_distortion(model, create_dummy_gcp_data(20))

  expect_error(plot.distortion("not a distortion object"))
  expect_error(plot(res, metric = "invalid_metric"), "must be one of")

  p <- plot(res, metric = "a", add_points = FALSE)
  expect_s3_class(p, "ggplot")

  p <- plot(res, metric = "a",
            diverging = TRUE,
            add_points = TRUE)
  expect_s3_class(p, "ggplot")

})


# --- Tests for indicatrices  ---

test_that("input validation works correctly with new scale_factor logic", {

  model <- train_pai_model(create_dummy_gcp_data(20), method = "lm")
  test_dist <-analyze_distortion(model, create_dummy_gcp_data(20))

  # Test that NULL is now a valid value for scale_factor
  expect_no_error(indicatrices(test_dist, scale_factor = NULL))

  # Test for invalid scale_factor
  expect_error(
    indicatrices(test_dist, scale_factor = -1),
    "`scale_factor` must be NULL or a single positive numeric value."
  )
})

test_that("automatic scaling calculates a factor and issues a message", {

  model <- train_pai_model(create_dummy_gcp_data(20), method = "lm")
  test_dist <-analyze_distortion(model, create_dummy_gcp_data(20))

  # Check that a message is printed when scale_factor is NULL
  expect_message(
    indicatrices(test_dist, scale_factor = NULL),
    "`scale_factor` is NULL. Automatically chosen value"
  )

  # Check that it runs without error
  p <- suppressMessages(indicatrices(test_dist))
  expect_s3_class(p, "ggplot")

  # Check that the resulting scale factor is a reasonable positive number
  p_build <- ggplot_build(p)
  built_data <- p_build$data[[1]]
  # The ellipse axes 'a' and 'b' should be much larger than the raw data 'a' and 'b'
  expect_gt(mean(built_data$a), mean(test_dist$a))
})

test_that("manual scale_factor override works as expected", {

  model <- train_pai_model(create_dummy_gcp_data(20), method = "lm")
  test_dist <-analyze_distortion(model, create_dummy_gcp_data(20))


  scale <- 1000

  # When we provide a value, there should be NO message
  expect_no_message(
    indicatrices(test_dist, scale_factor = scale),
  )

  p <- indicatrices(test_dist, scale_factor = scale)
  p_build <- ggplot_build(p)
  built_data <- p_build$data[[1]]

  # Check if the manual scale was applied correctly
  expect_equal(built_data$a[1], test_dist$a[1] * scale, tolerance = 0.1)
  expect_equal(built_data$b[1], test_dist$b[1] * scale, tolerance = 0.1)
})

# Keep the other tests from the previous version as they are still valid
test_that("output is a ggplot object with a GeomEllipse layer", {

  model <- train_pai_model(create_dummy_gcp_data(20), method = "lm")
  test_dist <-analyze_distortion(model, create_dummy_gcp_data(20))

  p <- suppressMessages(indicatrices(test_dist))
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$layers[[1]]$geom, "GeomShape")
})

test_that("static aesthetics (colors) are mapped correctly", {

  model <- train_pai_model(create_dummy_gcp_data(20), method = "lm")
  test_dist <-analyze_distortion(model, create_dummy_gcp_data(20))

  fill <- "#FF5733"
  border <- "darkgrey"

  p <- suppressMessages(indicatrices(
    test_dist, fill_color = fill, border_color = border)
    )

  p_build <- ggplot_build(p)
  built_data <- p_build$data[[1]]

  expect_equal(unique(built_data$fill), fill)
  expect_equal(unique(built_data$colour), border)
})


#### Regression tests for corrected distortion mathematics ####

test_that("theta_a matches the major-axis orientation from the SVD of J", {
  # theta_a must equal the angle of the first LEFT singular vector of J, i.e.
  # the major-axis orientation of Tissot's indicatrix in the target plane.
  # Angles are defined modulo 180 degrees.
  norm180 <- function(d) ((d + 90) %% 180) - 90

  make_affine_model <- function(J) {
    list(
      label = "Fixed Affine", modelType = "bivariate", library = NULL,
      fit = function(gcp_data, ...) list(),
      predict = function(model, newdata, ...) {
        tx <- J[1, 1] * newdata$source_x + J[1, 2] * newdata$source_y
        ty <- J[2, 1] * newdata$source_x + J[2, 2] * newdata$source_y
        cbind(tx - newdata$source_x, ty - newdata$source_y)
      }
    )
  }

  set.seed(42)
  for (i in 1:15) {
    J <- matrix(rnorm(4, sd = 2), 2, 2)
    if (abs(det(J)) < 1e-3) next

    mod <- suppressWarnings(
      train_pai_model(create_dummy_gcp_data(8), method = make_affine_model(J))
    )
    res <- suppressWarnings(
      suppressMessages(analyze_distortion(mod, create_dummy_gcp_data(8)))
    )

    s <- svd(J)
    truth <- atan2(s$u[2, 1], s$u[1, 1]) * 180 / pi

    expect_equal(norm180(res$theta_a[1]), norm180(truth), tolerance = 1e-5)
    expect_equal(res$a[1], s$d[1], tolerance = 1e-5)
    expect_equal(res$b[1], s$d[2], tolerance = 1e-5)
  }
})

test_that("max_angular_distortion is reported in degrees", {
  # A pure scaling by (2, 1) gives a = 2, b = 1, so 2*Omega = 2*asin(1/3).
  scale_model <- list(
    label = "Anisotropic Scale", modelType = "bivariate", library = NULL,
    fit = function(gcp_data, ...) list(),
    predict = function(model, newdata, ...) {
      cbind(newdata$source_x, rep(0, nrow(newdata)))
    }
  )
  mod <- train_pai_model(create_dummy_gcp_data(10), method = scale_model)
  res <- suppressMessages(analyze_distortion(mod, create_dummy_gcp_data(10)))

  expect_equal(res$a[1], 2, tolerance = 1e-5)
  expect_equal(res$b[1], 1, tolerance = 1e-5)
  expect_equal(res$max_angular_distortion[1],
               2 * asin(1 / 3) * 180 / pi, tolerance = 1e-5)
  # Guard against a silent reversion to radians.
  expect_gt(res$max_angular_distortion[1], 10)
})

test_that("airy_kavrayskiy responds to reference_scale and takes the outer root", {
  model <- train_pai_model(create_dummy_gcp_data(30), method = "lm")
  gcp <- create_dummy_gcp_data(30)

  d1 <- suppressMessages(analyze_distortion(model, gcp, reference_scale = 1))
  d2 <- suppressMessages(analyze_distortion(model, gcp, reference_scale = 2))

  expect_false(isTRUE(all.equal(d1$airy_kavrayskiy, d2$airy_kavrayskiy)))

  expected <- sqrt(0.5 * (log(d1$a / 2)^2 + log(d1$b / 2)^2))
  expect_equal(d2$airy_kavrayskiy, expected, tolerance = 1e-9)

  # log2_area_scale must shift by exactly -2 when the linear reference doubles.
  expect_equal(d2$log2_area_scale, d1$log2_area_scale - 2, tolerance = 1e-9)
})

test_that("a degenerate newdata does not silently produce NaN metrics", {
  model <- train_pai_model(create_dummy_gcp_data(40), method = "lm")

  one_point <- data.frame(source_x = 500, source_y = 500)
  res <- suppressMessages(analyze_distortion(model, one_point))
  expect_false(any(is.nan(c(res$a, res$b, res$det_J, res$theta_a))))
  expect_true(all(is.finite(c(res$a, res$b, res$det_J))))

  # A collinear set is degenerate in one axis only, which was also fatal before.
  collinear <- data.frame(source_x = rep(500, 5), source_y = seq(0, 400, 100))
  res2 <- suppressMessages(analyze_distortion(model, collinear))
  expect_true(all(is.finite(res2$a)))
})

test_that("raster distortion metrics stay aligned when the input has NA cells", {
  skip_if_not_installed("terra")

  gcp <- create_dummy_gcp_data(60)
  model <- train_pai_model(gcp, method = "lm")

  r <- terra::rast(
    terra::ext(min(gcp$source_x), max(gcp$source_x),
               min(gcp$source_y), max(gcp$source_y)),
    nrows = 10, ncols = 10
  )
  terra::values(r) <- seq_len(terra::ncell(r))
  na_idx <- 1:40
  terra::values(r)[na_idx] <- NA

  out <- suppressMessages(analyze_distortion(model, r))

  expect_equal(terra::ncell(out), terra::ncell(r))
  # The input NA mask must be carried through, not filled with recycled values.
  expect_equal(sum(is.na(terra::values(out)[, "a"])), length(na_idx))

  # Values on the surviving cells must match a direct point-wise evaluation.
  xy <- terra::crds(r, na.rm = FALSE)
  pts <- data.frame(source_x = xy[, 1], source_y = xy[, 2])
  direct <- suppressMessages(analyze_distortion(model, pts))
  keep <- !is.na(terra::values(r)[, 1])
  expect_equal(terra::values(out)[keep, "a"], direct$a[keep],
               tolerance = 1e-8, ignore_attr = TRUE)
})
