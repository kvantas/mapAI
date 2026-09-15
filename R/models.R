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

#' **Performance note**: `interp::interp()` is called at predict time, not fit time.
#' The Delaunay triangulation and Akima C1 spline coefficients are rebuilt from
#' scratch on every `predict()` call. This gives O(M·n log n) prediction complexity
#' vs O(M log n) for `tin_linear`. For n > 200 GCPs, this becomes a significant
#' bottleneck. Fit is O(n) — just stores data and fits a linear fallback.
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
    # mgcv::mvn(d = 2) estimates a full 2x2 residual covariance. If either
    # displacement component is constant, that matrix is singular and mgcv fails
    # deep in compiled code with "NA/NaN/Inf in foreign function call (arg 1)",
    # which tells the user nothing. A constant field is an ordinary input -- a
    # map needing no correction, or only a rigid shift -- so catch it here and
    # name a method that can fit it.
    rel_sd <- function(z) stats::sd(z) / max(1, mean(abs(z)))
    flat <- c(dx = rel_sd(dat$dx), dy = rel_sd(dat$dy)) <= 1e-10

    if (any(flat)) {
      which_flat <- paste(names(flat)[flat], collapse = " and ")
      stop(sprintf(
        paste0("The %s displacement component(s) are constant across all ",
               "control points, so the bivariate GAM has no residual variance ",
               "to model. This is what a map needing no correction, or only a ",
               "uniform shift, looks like. Use method = \"helmert\" (which ",
               "recovers a pure translation exactly) or method = \"lm\"."),
        which_flat), call. = FALSE)
    }

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
    #
    # The check above catches a displacement field that is exactly constant. A
    # field with variance small enough to make the residual covariance
    # numerically singular fails too, but at a threshold that depends on mgcv's
    # internals rather than on anything we can compute here, so translate that
    # failure rather than trying to predict it.
    tryCatch(
      mgcv::gam(formula_list, data = dat, family = mgcv::mvn(d = 2), ...),
      error = function(e) {
        msg <- conditionMessage(e)
        if (grepl("foreign function call|NA/NaN/Inf", msg)) {
          stop(sprintf(
            paste0("mgcv could not estimate the residual covariance of the ",
                   "bivariate GAM (relative SD of dx = %.2e, dy = %.2e). This ",
                   "happens when the displacement field carries almost no ",
                   "variance, as for a map needing no correction or only a ",
                   "near-uniform shift. Use method = \"helmert\" or ",
                   "method = \"lm\". Original message: %s"),
            rel_sd(dat$dx), rel_sd(dat$dy), msg), call. = FALSE)
        }
        stop(e)
      }
    )
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

#' **Mathematical equivalence**: Inside the convex hull, this model produces results
#' identical to `tin_linear` (with `fallback = "helmert"`) to machine precision (~1e-14).
#' This is because barycentric weights satisfy Σλ_i = 1 and Σλ_i·p_i = p, so for any
#' affine base h the trend cancels exactly in the residual interpolation. The Helmert
#' base is affine, so the two-stage decomposition adds no accuracy inside the hull.
#' The model differs from `tin_linear` **only in extrapolation** outside the convex
#' hull: residuals default to zero, giving pure Helmert extrapolation, whereas
#' `tin_linear` uses its fallback model directly.
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

#' **Mathematical equivalence**: Same identity as `hybrid_helmert_tin` — identical
#' to `tin_linear` (with `fallback = "affine"`) inside the hull.
#' Outside the hull: residuals default to zero, giving pure affine extrapolation.
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


# ---- Moving Least Squares 2D Models (Schaefer et al., 2006) ------------------

#' 2D Moving Least Squares Deformation Solver
#' @noRd
predict_mls_2d <- function(P, Q, newdata, mode = c("similarity", "rigid", "affine"), alpha = 1.0) {
  mode <- match.arg(mode)
  M <- nrow(newdata)
  out_dx <- numeric(M)
  out_dy <- numeric(M)

  vx <- newdata$source_x
  vy <- newdata$source_y
  px <- P[, 1]
  py <- P[, 2]
  qx <- Q[, 1]
  qy <- Q[, 2]

  for (k in seq_len(M)) {
    vk_x <- vx[k]
    vk_y <- vy[k]

    diff_x <- px - vk_x
    diff_y <- py - vk_y
    d2 <- diff_x^2 + diff_y^2

    min_idx <- which.min(d2)
    if (d2[min_idx] < 1e-12) {
      out_dx[k] <- qx[min_idx] - vk_x
      out_dy[k] <- qy[min_idx] - vk_y
      next
    }

    w <- 1.0 / (d2^alpha)
    w_sum <- sum(w)
    if (w_sum < 1e-15) {
      out_dx[k] <- 0
      out_dy[k] <- 0
      next
    }

    p_star_x <- sum(w * px) / w_sum
    p_star_y <- sum(w * py) / w_sum
    q_star_x <- sum(w * qx) / w_sum
    q_star_y <- sum(w * qy) / w_sum

    p_hat_x <- px - p_star_x
    p_hat_y <- py - p_star_y
    q_hat_x <- qx - q_star_x
    q_hat_y <- qy - q_star_y
    v_hat_x <- vk_x - p_star_x
    v_hat_y <- vk_y - p_star_y

    if (mode == "affine") {
      p11 <- sum(w * p_hat_x * p_hat_x)
      p12 <- sum(w * p_hat_x * p_hat_y)
      p22 <- sum(w * p_hat_y * p_hat_y)
      det_p <- p11 * p22 - p12 * p12
      if (abs(det_p) < 1e-12) {
        out_dx[k] <- q_star_x - p_star_x
        out_dy[k] <- q_star_y - p_star_y
        next
      }
      inv11 <-  p22 / det_p
      inv12 <- -p12 / det_p
      inv22 <-  p11 / det_p

      q11 <- sum(w * p_hat_x * q_hat_x)
      q12 <- sum(w * p_hat_x * q_hat_y)
      q21 <- sum(w * p_hat_y * q_hat_x)
      q22 <- sum(w * p_hat_y * q_hat_y)

      m11 <- inv11 * q11 + inv12 * q21
      m12 <- inv11 * q12 + inv12 * q22
      m21 <- inv12 * q11 + inv22 * q21
      m22 <- inv12 * q12 + inv22 * q22

      pred_x <- v_hat_x * m11 + v_hat_y * m21 + q_star_x
      pred_y <- v_hat_x * m12 + v_hat_y * m22 + q_star_y

    } else {
      # Similarity and Rigid modes
      mu <- sum(w * (p_hat_x^2 + p_hat_y^2))
      if (abs(mu) < 1e-14) {
        out_dx[k] <- q_star_x - p_star_x
        out_dy[k] <- q_star_y - p_star_y
        next
      }

      dot1 <- v_hat_x * p_hat_x + v_hat_y * p_hat_y
      dot2 <- -v_hat_x * p_hat_y + v_hat_y * p_hat_x

      fx <- sum(w * (dot1 * q_hat_x - dot2 * q_hat_y)) / mu
      fy <- sum(w * (dot2 * q_hat_x + dot1 * q_hat_y)) / mu

      if (mode == "similarity") {
        pred_x <- fx + q_star_x
        pred_y <- fy + q_star_y
      } else { # rigid
        norm_f <- sqrt(fx^2 + fy^2)
        norm_v <- sqrt(v_hat_x^2 + v_hat_y^2)
        if (norm_f < 1e-12 || norm_v < 1e-12) {
          pred_x <- fx + q_star_x
          pred_y <- fy + q_star_y
        } else {
          pred_x <- norm_v * (fx / norm_f) + q_star_x
          pred_y <- norm_v * (fy / norm_f) + q_star_y
        }
      }
    }

    out_dx[k] <- pred_x - vk_x
    out_dy[k] <- pred_y - vk_y
  }

  return(data.frame(dx = out_dx, dy = out_dy, row.names = row.names(newdata)))
}

#' @noRd
mls_similarity_model <- list(
  label = "Moving Least Squares (Similarity)",
  library = NULL,
  modelType = "bivariate",
  fit = function(dat, alpha = 1.0, ...) {
    list(
      P = as.matrix(dat[, c("source_x", "source_y")]),
      Q = as.matrix(dat[, c("target_x", "target_y")]),
      alpha = alpha
    )
  },
  predict = function(modelFit, newdata, ...) {
    predict_mls_2d(modelFit$P, modelFit$Q, newdata, mode = "similarity", alpha = modelFit$alpha)
  }
)

#' @noRd
mls_rigid_model <- list(
  label = "Moving Least Squares (Rigid)",
  library = NULL,
  modelType = "bivariate",
  fit = function(dat, alpha = 1.0, ...) {
    list(
      P = as.matrix(dat[, c("source_x", "source_y")]),
      Q = as.matrix(dat[, c("target_x", "target_y")]),
      alpha = alpha
    )
  },
  predict = function(modelFit, newdata, ...) {
    predict_mls_2d(modelFit$P, modelFit$Q, newdata, mode = "rigid", alpha = modelFit$alpha)
  }
)

#' @noRd
mls_affine_model <- list(
  label = "Moving Least Squares (Affine)",
  library = NULL,
  modelType = "bivariate",
  fit = function(dat, alpha = 1.0, ...) {
    list(
      P = as.matrix(dat[, c("source_x", "source_y")]),
      Q = as.matrix(dat[, c("target_x", "target_y")]),
      alpha = alpha
    )
  },
  predict = function(modelFit, newdata, ...) {
    predict_mls_2d(modelFit$P, modelFit$Q, newdata, mode = "affine", alpha = modelFit$alpha)
  }
)


# ---- Radial Basis Function (RBF) Models (Hardy, 1971) ------------------------

#' @noRd
rbf_multiquadric_model <- list(
  label = "Radial Basis Function (Multiquadric)",
  library = NULL,
  modelType = "bivariate",
  fit = function(dat, epsilon = NULL, regularization = 0.0, ...) {
    P <- as.matrix(dat[, c("source_x", "source_y")])
    Q <- as.matrix(dat[, c("target_x", "target_y")])
    N <- nrow(P)

    # Automatic epsilon scale: inverse mean nearest-neighbor distance
    D <- compute_pairwise_distances(P, P)
    if (is.null(epsilon)) {
      diag_D <- D
      diag(diag_D) <- Inf
      mean_nn <- mean(apply(diag_D, 1, min))
      epsilon <- if (mean_nn > 1e-12) 1.0 / mean_nn else 1.0
    }

    Phi <- sqrt((epsilon * D)^2 + 1)
    if (regularization > 0) {
      Phi <- Phi + regularization * diag(N)
    }

    P_aug <- cbind(1, P)
    L <- rbind(
      cbind(Phi, P_aug),
      cbind(t(P_aug), matrix(0, nrow = 3, ncol = 3))
    )
    Y <- rbind(Q, matrix(0, nrow = 3, ncol = 2))

    sol <- tryCatch(
      solve(L, Y),
      error = function(e) qr.solve(L, Y)
    )

    fit_obj <- list(
      W = sol[1:N, , drop = FALSE],
      A = sol[(N + 1):(N + 3), , drop = FALSE],
      source_pts = P,
      epsilon = epsilon,
      regularization = regularization
    )
    class(fit_obj) <- "rbf_fit"
    return(fit_obj)
  },
  predict = function(modelFit, newdata, ...) {
    M_pts <- as.matrix(newdata[, c("source_x", "source_y")])
    D_eval <- compute_pairwise_distances(M_pts, modelFit$source_pts)
    Phi_eval <- sqrt((modelFit$epsilon * D_eval)^2 + 1)
    P_eval <- cbind(1, M_pts)

    pred_tgt <- Phi_eval %*% modelFit$W + P_eval %*% modelFit$A
    pred_dx <- pred_tgt[, 1] - newdata$source_x
    pred_dy <- pred_tgt[, 2] - newdata$source_y

    return(data.frame(dx = pred_dx, dy = pred_dy, row.names = row.names(newdata)))
  }
)


# ---- Global Projective / Homography Model (DLT) ------------------------------

#' @noRd
projective_model <- list(
  label = "Global Projective (Homography)",
  library = NULL,
  modelType = "bivariate",
  fit = function(dat, ...) {
    P <- as.matrix(dat[, c("source_x", "source_y")])
    Q <- as.matrix(dat[, c("target_x", "target_y")])
    N <- nrow(P)

    A_mat <- matrix(0, nrow = 2 * N, ncol = 9)
    for (i in seq_len(N)) {
      x <- P[i, 1]; y <- P[i, 2]
      u <- Q[i, 1]; v <- Q[i, 2]
      A_mat[2 * i - 1, ] <- c(-x, -y, -1,  0,  0,  0,  x * u, y * u, u)
      A_mat[2 * i, ]     <- c( 0,  0,  0, -x, -y, -1,  x * v, y * v, v)
    }

    ev <- eigen(crossprod(A_mat), symmetric = TRUE)
    h <- ev$vectors[, 9]
    if (abs(h[9]) > 1e-12) {
      h <- h / h[9]
    }
    H <- matrix(h, nrow = 3, byrow = TRUE)

    fit_obj <- list(H = H)
    class(fit_obj) <- "projective_fit"
    return(fit_obj)
  },
  predict = function(modelFit, newdata, ...) {
    P <- as.matrix(newdata[, c("source_x", "source_y")])
    homog <- cbind(P, 1)
    pred_homog <- tcrossprod(homog, modelFit$H)
    w <- pred_homog[, 3]
    w[abs(w) < 1e-12] <- 1e-12

    pred_tgt_x <- pred_homog[, 1] / w
    pred_tgt_y <- pred_homog[, 2] / w

    pred_dx <- pred_tgt_x - newdata$source_x
    pred_dy <- pred_tgt_y - newdata$source_y

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
  mls_affine = mls_affine_model,
  mls_rigid = mls_rigid_model,
  mls_similarity = mls_similarity_model,
  projective = projective_model,
  rbf_multiquadric = rbf_multiquadric_model,
  tin_akima = tin_akima_model,
  tin_linear = tin_linear_model,
  tps = tps_model
)
