#' @title Built-in PAI Transformation Model Registry
#' @description Definitions of the transformation models shipped with `mapAI`.
#'
#' @details
#' Each entry of `pai_model_list` is a list describing one transformation model:
#' \describe{
#'   \item{`label`}{Human-readable model name used in printed output.}
#'   \item{`library`}{Name of the package required to fit the model, or `NULL`
#'     for models that depend only on base R and `mapAI` itself. Checked with
#'     `requireNamespace()` by `train_pai_model()`.}
#'   \item{`modelType`}{Either `"univariate"` (the displacement components `dx`
#'     and `dy` are fitted by two independent calls) or `"bivariate"` (a single
#'     call receives the whole GCP data frame and returns both components).}
#'   \item{`fit`}{For univariate models, `function(x, y, ...)` where `x` is a data
#'     frame of predictors and `y` the response vector. For bivariate models,
#'     `function(dat, ...)` where `dat` is the full GCP data frame, carrying
#'     `source_x`, `source_y`, `target_x`, `target_y`, `dx` and `dy`.}
#'   \item{`predict`}{`function(modelFit, newdata, ...)`. Univariate models return
#'     a numeric vector; bivariate models return a two-column object whose first
#'     column is `dx` and second `dy`.}
#' }
#'
#' This registry lives in package source rather than in `R/sysdata.rda` so that
#' the fitting and prediction mathematics is reviewable in a diff. The same
#' contract governs user-supplied custom models passed to `train_pai_model()` via
#' `method = list(...)`; see the `custom_model_training` vignette.
#'
#' @keywords internal
#' @name pai_model_list
NULL


# ---- Univariate models -------------------------------------------------------

#' @noRd
lm_model <- list(
  label = "Linear Model",
  library = NULL,
  modelType = "univariate",
  fit = function(x, y, ...) {
    dat <- cbind(y, x)
    names(dat)[1] <- "outcome"
    stats::lm(outcome ~ ., data = dat, ...)
  },
  predict = function(modelFit, newdata, ...) {
    stats::predict(modelFit, newdata = newdata, ...)
  }
)

#' @noRd
tps_model <- list(
  label = "TPS",
  library = "fields",
  modelType = "univariate",
  fit = function(x, y, ...) {
    fields::Tps(x = as.matrix(x), Y = y, ...)
  },
  predict = function(modelFit, newdata, ...) {
    predict(modelFit, x = as.matrix(newdata), ...)
  }
)

#' @noRd
tin_akima_model <- list(
  label = "Akima C1 Triangulated Spline",
  library = "interp",
  modelType = "univariate",
  fit = function(x, y, ...) {
    dat <- data.frame(source_x = x$source_x, source_y = x$source_y, val = y)
    fallback_fit <- stats::lm(val ~ source_x + source_y, data = dat)
    fit_obj <- list(
      dat = dat,
      fallback_fit = fallback_fit
    )
    class(fit_obj) <- "tin_akima_fit"
    return(fit_obj)
  },
  predict = function(modelFit, newdata, ...) {
    ak_res <- interp::interp(
      x = modelFit$dat$source_x,
      y = modelFit$dat$source_y,
      z = modelFit$dat$val,
      xo = newdata$source_x,
      yo = newdata$source_y,
      output = "points",
      method = "akima"
    )
    preds <- ak_res$z
    na_idx <- is.na(preds)
    if (any(na_idx)) {
      preds[na_idx] <- stats::predict(modelFit$fallback_fit,
                                      newdata = newdata[na_idx, , drop = FALSE])
    }
    return(preds)
  }
)


# ---- Bivariate models --------------------------------------------------------

#' @noRd
gam_biv_model <- list(
  label = "Bivariate GAM",
  library = "mgcv",
  modelType = "bivariate",
  fit = function(dat, k = NULL, k_max = 29, ...) {
    n_unique <- nrow(unique(dat[, c("source_x", "source_y")]))
    if (is.null(k)) {
      k_adaptive <- max(3, min(k_max, floor(n_unique * 0.6)))
    } else {
      k_adaptive <- min(k, n_unique - 1)
    }
    formula_list <- list(
      stats::as.formula(sprintf("dx ~ s(source_x, source_y, k = %d)", k_adaptive)),
      stats::as.formula(sprintf("dy ~ s(source_x, source_y, k = %d)", k_adaptive))
    )
    # family = mvn(d = 2) fits dx and dy jointly with a full 2x2 residual
    # covariance. mvn is a general family, so mgcv selects smoothing parameters
    # by REML regardless of any `method` argument.
    mgcv::gam(formula_list, data = dat, family = mgcv::mvn(d = 2), ...)
  },
  predict = function(modelFit, newdata, ...) {
    stats::predict(modelFit, newdata = newdata, ...)
  }
)

#' @noRd
helmert_model <- list(
  label = "Helmert Model",
  library = NULL,
  modelType = "bivariate",
  fit = function(dat, ...) {
    helmert(dat$source_x, dat$source_y,
            dat$target_x, dat$target_y, ...)
  },
  predict = function(modelFit, newdata, ...) {
    preds <- stats::predict(modelFit, newdata = newdata, ...)
    pred_dx <- preds[, 1] - newdata$source_x
    pred_dy <- preds[, 2] - newdata$source_y

    return(data.frame(dx = pred_dx, dy = pred_dy, row.names = row.names(newdata)))
  }
)

#' @noRd
tin_linear_model <- list(
  label = "Piecewise Affine Delaunay Triangulation",
  library = "interp",
  modelType = "bivariate",
  fit = function(dat, fallback = c("helmert", "affine"), ...) {
    fallback <- match.arg(fallback)
    tri_mesh <- interp::tri.mesh(x = dat$source_x, y = dat$source_y)

    # Pre-compute fallback model for extrapolation outside convex hull
    fallback_model <- if (fallback == "helmert") {
      helmert(dat$source_x, dat$source_y, dat$target_x, dat$target_y)
    } else {
      list(
        lm_x = stats::lm(dx ~ source_x + source_y, data = dat),
        lm_y = stats::lm(dy ~ source_x + source_y, data = dat)
      )
    }

    # Inversion analysis on triangles. det_J is the ratio of signed areas, so it
    # is independent of the triangulation's vertex winding convention.
    tris <- interp::triangles(tri_mesh)
    v1 <- tris[, 1]; v2 <- tris[, 2]; v3 <- tris[, 3]

    src_area <- 0.5 * ((dat$source_x[v2] - dat$source_x[v1]) * (dat$source_y[v3] - dat$source_y[v1]) -
                       (dat$source_x[v3] - dat$source_x[v1]) * (dat$source_y[v2] - dat$source_y[v1]))
    tgt_area <- 0.5 * ((dat$target_x[v2] - dat$target_x[v1]) * (dat$target_y[v3] - dat$target_y[v1]) -
                       (dat$target_x[v3] - dat$target_x[v1]) * (dat$target_y[v2] - dat$target_y[v1]))

    det_J <- tgt_area / src_area
    n_inverted <- sum(det_J <= 0, na.rm = TRUE)
    if (n_inverted > 0) {
      warning(
        sprintf(
          "TIN warning: %d triangle(s) have non-positive Jacobian determinants (det(J) <= 0) and will produce topological fold-overs.",
          n_inverted
        ),
        call. = FALSE
      )
    }

    fit_obj <- list(
      mesh = tri_mesh,
      gcp = dat,
      fallback = fallback,
      fallback_model = fallback_model,
      triangles = tris,
      det_J = det_J,
      is_inverted = (det_J <= 0)
    )
    class(fit_obj) <- "tin_linear_fit"
    return(fit_obj)
  },
  predict = function(modelFit, newdata, ...) {
    n_pts <- nrow(newdata)
    tf <- interp::tri.find(modelFit$mesh, newdata$source_x, newdata$source_y)
    inside <- (tf$i1 > 0)

    pred_dx <- rep(NA_real_, n_pts)
    pred_dy <- rep(NA_real_, n_pts)

    if (any(inside)) {
      i1 <- tf$i1[inside]; i2 <- tf$i2[inside]; i3 <- tf$i3[inside]
      bc <- tf$bc[inside, , drop = FALSE]
      pred_dx[inside] <- bc[, 1] * modelFit$gcp$dx[i1] +
                         bc[, 2] * modelFit$gcp$dx[i2] +
                         bc[, 3] * modelFit$gcp$dx[i3]
      pred_dy[inside] <- bc[, 1] * modelFit$gcp$dy[i1] +
                         bc[, 2] * modelFit$gcp$dy[i2] +
                         bc[, 3] * modelFit$gcp$dy[i3]
    }

    if (any(!inside)) {
      out_df <- newdata[!inside, , drop = FALSE]
      if (modelFit$fallback == "helmert") {
        h_pred <- stats::predict(modelFit$fallback_model, newdata = out_df)
        pred_dx[!inside] <- h_pred[, 1] - out_df$source_x
        pred_dy[!inside] <- h_pred[, 2] - out_df$source_y
      } else {
        pred_dx[!inside] <- stats::predict(modelFit$fallback_model$lm_x, newdata = out_df)
        pred_dy[!inside] <- stats::predict(modelFit$fallback_model$lm_y, newdata = out_df)
      }
    }

    return(data.frame(dx = pred_dx, dy = pred_dy, row.names = row.names(newdata)))
  }
)

#' @noRd
hybrid_helmert_tin_model <- list(
  label = "Hybrid Helmert-TIN Model",
  library = "interp",
  modelType = "bivariate",
  fit = function(dat, ...) {
    # 1. Global conformal Helmert base
    h_fit <- helmert(dat$source_x, dat$source_y, dat$target_x, dat$target_y, ...)
    h_pred <- stats::predict(h_fit, newdata = dat)

    # 2. Local residual displacements
    res_x <- dat$target_x - h_pred[, 1]
    res_y <- dat$target_y - h_pred[, 2]

    # 3. Delaunay mesh on residuals
    tri_mesh <- interp::tri.mesh(x = dat$source_x, y = dat$source_y)

    fit_obj <- list(
      h_fit = h_fit,
      mesh = tri_mesh,
      res_x = res_x,
      res_y = res_y,
      gcp = dat
    )
    class(fit_obj) <- "hybrid_helmert_tin_fit"
    return(fit_obj)
  },
  predict = function(modelFit, newdata, ...) {
    n_pts <- nrow(newdata)
    h_pred <- stats::predict(modelFit$h_fit, newdata = newdata, ...)

    tf <- interp::tri.find(modelFit$mesh, newdata$source_x, newdata$source_y)
    inside <- (tf$i1 > 0)

    rx <- numeric(n_pts)
    ry <- numeric(n_pts)

    if (any(inside)) {
      i1 <- tf$i1[inside]; i2 <- tf$i2[inside]; i3 <- tf$i3[inside]
      bc <- tf$bc[inside, , drop = FALSE]
      rx[inside] <- bc[, 1] * modelFit$res_x[i1] +
                    bc[, 2] * modelFit$res_x[i2] +
                    bc[, 3] * modelFit$res_x[i3]
      ry[inside] <- bc[, 1] * modelFit$res_y[i1] +
                    bc[, 2] * modelFit$res_y[i2] +
                    bc[, 3] * modelFit$res_y[i3]
    }
    # Outside convex hull: rx and ry remain 0 (pure Helmert extrapolation)

    pred_tgt_x <- h_pred[, 1] + rx
    pred_tgt_y <- h_pred[, 2] + ry

    pred_dx <- pred_tgt_x - newdata$source_x
    pred_dy <- pred_tgt_y - newdata$source_y

    return(data.frame(dx = pred_dx, dy = pred_dy, row.names = row.names(newdata)))
  }
)

#' @noRd
hybrid_affine_tin_model <- list(
  label = "Hybrid Affine-TIN Model",
  library = "interp",
  modelType = "bivariate",
  fit = function(dat, ...) {
    # 1. Global affine base
    lm_x <- stats::lm(dx ~ source_x + source_y, data = dat)
    lm_y <- stats::lm(dy ~ source_x + source_y, data = dat)

    aff_dx <- stats::predict(lm_x, newdata = dat)
    aff_dy <- stats::predict(lm_y, newdata = dat)

    res_dx <- dat$dx - aff_dx
    res_dy <- dat$dy - aff_dy

    tri_mesh <- interp::tri.mesh(x = dat$source_x, y = dat$source_y)

    fit_obj <- list(
      lm_x = lm_x,
      lm_y = lm_y,
      mesh = tri_mesh,
      res_dx = res_dx,
      res_dy = res_dy,
      gcp = dat
    )
    class(fit_obj) <- "hybrid_affine_tin_fit"
    return(fit_obj)
  },
  predict = function(modelFit, newdata, ...) {
    n_pts <- nrow(newdata)
    base_dx <- stats::predict(modelFit$lm_x, newdata = newdata)
    base_dy <- stats::predict(modelFit$lm_y, newdata = newdata)

    tf <- interp::tri.find(modelFit$mesh, newdata$source_x, newdata$source_y)
    inside <- (tf$i1 > 0)

    rx <- numeric(n_pts)
    ry <- numeric(n_pts)

    if (any(inside)) {
      i1 <- tf$i1[inside]; i2 <- tf$i2[inside]; i3 <- tf$i3[inside]
      bc <- tf$bc[inside, , drop = FALSE]
      rx[inside] <- bc[, 1] * modelFit$res_dx[i1] +
                    bc[, 2] * modelFit$res_dx[i2] +
                    bc[, 3] * modelFit$res_dx[i3]
      ry[inside] <- bc[, 1] * modelFit$res_dy[i1] +
                    bc[, 2] * modelFit$res_dy[i2] +
                    bc[, 3] * modelFit$res_dy[i3]
    }
    # Outside convex hull: rx and ry remain 0 (pure affine extrapolation)

    pred_dx <- base_dx + rx
    pred_dy <- base_dy + ry

    return(data.frame(dx = pred_dx, dy = pred_dy, row.names = row.names(newdata)))
  }
)


# ---- Registry ----------------------------------------------------------------

#' @rdname pai_model_list
#' @keywords internal
pai_model_list <- list(
  gam_biv = gam_biv_model,
  helmert = helmert_model,
  hybrid_affine_tin = hybrid_affine_tin_model,
  hybrid_helmert_tin = hybrid_helmert_tin_model,
  lm = lm_model,
  tin_akima = tin_akima_model,
  tin_linear = tin_linear_model,
  tps = tps_model
)
