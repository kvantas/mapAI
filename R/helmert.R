#' Helmert 2D Transformation Solver
#'
#' @description Calculates the parameters of a 2D similarity (Helmert)
#'   transformation using either Ordinary Least Squares (OLS) or Total Least
#'   Squares (TLS), or scaled orthogonal Procrustes. Calculates geodetic physical
#'   parameters (scale factor, rotation angle, translations), standard errors,
#'   and residual variance.
#'
#' @details The 2D conformal similarity (Helmert) transformation relates
#'   source coordinates \eqn{(u, v)} to target coordinates \eqn{(x, y)} through:
#'   \deqn{\begin{pmatrix} x \\ y \end{pmatrix} = \begin{pmatrix} t_x \\ t_y \end{pmatrix} + \begin{pmatrix} a & -b \\ b & a \end{pmatrix} \begin{pmatrix} u \\ v \end{pmatrix}}
#'   where the physical parameters are:
#'   \itemize{
#'     \item Scale factor: \eqn{s = \sqrt{a^2 + b^2}}
#'     \item Rotation angle: \eqn{\theta = \operatorname{atan2}(b, a)}
#'     \item Translation vector: \eqn{(t_x, t_y)}
#'   }
#'
#'   Three estimators are available:
#'   \itemize{
#'     \item `"ols"` (default): Ordinary Least Squares. Errors are assumed to lie
#'       only in the target coordinates. Closed-form solution.
#'     \item `"tls"`: Total Least Squares. The 2n-by-2 design matrix \eqn{\mathbf{A}}
#'       and the stacked target vector \eqn{\mathbf{d}} are combined into the
#'       augmented matrix
#'       \deqn{\mathbf{M} = \begin{pmatrix} \mathbf{u} & -\mathbf{v} & \mathbf{x} \\ \mathbf{v} & \mathbf{u} & \mathbf{y}\end{pmatrix}}
#'       (centred coordinates), and the estimate is read from the right singular
#'       vector \eqn{\mathbf{v}_3} belonging to the smallest singular value:
#'       \deqn{(a, b)^T = -\frac{1}{v_{33}}(v_{31}, v_{32})^T}
#'       Because the perturbation is applied to the whole of \eqn{\mathbf{M}},
#'       error in the source coordinates is accounted for, which reduces the
#'       attenuation (regression-dilution) bias that OLS incurs when the source
#'       coordinates are themselves measured with error.
#'     \item `"procrustes"`: scaled orthogonal Procrustes (Umeyama) via SVD of the
#'       cross-product matrix. \strong{For this 4-parameter conformal model it
#'       minimises the same objective over the same parameter set as OLS and so
#'       returns the same estimate to machine precision}; it is provided for
#'       completeness and for comparison, not as an errors-in-variables method.
#'   }
#'
#'   Standard errors, `sigma0` and `df` are derived under the OLS error model.
#'   For `"tls"` they should be read as approximate.
#'
#' @references
#' \itemize{
#'   \item Wolf, P. R., & Ghilani, C. D. (2006). \emph{Adjustment Computations: Spatial Data Analysis} (4th ed.). John Wiley & Sons.
#'   \item Vantas, K. (2025). \emph{mapAI: Positional Accuracy Improvement for Geospatial Vector and Raster Data}. R package version 1.0.0. \doi{10.5281/zenodo.15767080}
#' }
#'
#' @param source_x Numeric vector of approximate ('from') x coordinates.
#' @param source_y Numeric vector of approximate ('from') y coordinates.
#' @param target_x Numeric vector of actual ('to') x coordinates.
#' @param target_y Numeric vector of actual ('to') y coordinates.
#' @param method Estimation method: `"ols"` (Ordinary Least Squares, default),
#'   `"tls"` (Total Least Squares, errors-in-variables), or `"procrustes"`
#'   (scaled orthogonal Procrustes, which coincides with `"ols"` for this model).
#'
#' @return An object of class `helmert` containing:
#'   \item{coefficients}{Calculated coefficients \eqn{a} and \eqn{b}.}
#'   \item{centroids}{Centroids of source and target coordinates.}
#'   \item{parameters}{Named vector of physical parameters: `scale`, `theta_deg`,
#'     `theta_rad`, `tx`, `ty`.}
#'   \item{se}{Standard errors of `scale`, `theta_deg`, `tx`, and `ty`.}
#'   \item{sigma0}{Reference standard deviation (standard error of unit weight).}
#'   \item{df}{Degrees of freedom (\eqn{2n - 4}).}
#'   \item{residuals}{Data frame of coordinate residuals \eqn{(r_x, r_y)}.}
#'   \item{method}{Estimation method used (`"ols"`, `"tls"`, or `"procrustes"`).}
#'
#' @export
#' @examples
#' # Sample homologous control points
#' source_coords <- data.frame(source_x = c(10, 20, 30, 15),
#'                             source_y = c(15, 25, 10, 5))
#' target_coords <- data.frame(target_x = c(110.5, 119.8, 131.2, 114.0),
#'                             target_y = c(114.5, 125.2, 109.8, 104.9))
#'
#' # Calculate Helmert transformation via OLS
#' helmert_model <- helmert(
#'   source_x = source_coords$source_x,
#'   source_y = source_coords$source_y,
#'   target_x = target_coords$target_x,
#'   target_y = target_coords$target_y,
#'   method = "ols"
#' )
#' print(helmert_model)
helmert <- function(source_x, source_y, target_x, target_y,
                    method = c("ols", "tls", "procrustes")) {

  # ---  Input Validation ---
  input_validation(source_x, source_y, target_x, target_y)
  method <- match.arg(method)

  n <- length(source_x)

  # --- Core Helmert Calculation ---

  # Calculate centroids of both source and target points
  u_mean <- mean(source_x)
  v_mean <- mean(source_y)
  x_mean <- mean(target_x)
  y_mean <- mean(target_y)

  # Calculate centered coordinates for both systems
  u_i <- source_x - u_mean
  v_i <- source_y - v_mean
  x_i <- target_x - x_mean
  y_i <- target_y - y_mean

  # Robust, scale-invariant co-location validation
  denominator <- sum(u_i^2 + v_i^2)
  source_scale <- max(1, mean(source_x^2 + source_y^2))
  rel_tolerance <- max(1e-9, .Machine$double.eps * source_scale)

  if (denominator < rel_tolerance) {
    stop("Cannot solve Helmert transformation: source points are co-located.",
         call. = FALSE)
  }

  target_denom <- sum(x_i^2 + y_i^2)
  target_scale <- max(1, mean(target_x^2 + target_y^2))
  if (target_denom < max(1e-9, .Machine$double.eps * target_scale)) {
    stop("Cannot solve Helmert transformation: target points are co-located.",
         call. = FALSE)
  }

  if (method == "ols") {
    # Standard OLS least-squares solution for similarity transformation
    a <- (sum(u_i * x_i) + sum(v_i * y_i)) / denominator
    b <- (sum(u_i * y_i) - sum(v_i * x_i)) / denominator

  } else if (method == "procrustes") {
    # Scaled orthogonal Procrustes (Umeyama). NOTE: for the 4-parameter
    # conformal model this minimises the same objective over the same parameter
    # set as OLS and therefore returns the same estimate to machine precision.
    # It is retained because it generalises, not because it differs here.
    X <- cbind(u_i, v_i)
    Y <- cbind(x_i, y_i)
    M <- crossprod(X, Y)
    svd_m <- svd(M)
    R <- tcrossprod(svd_m$v, svd_m$u)

    # Reflection check (ensure proper rotation). When the unconstrained solution
    # is a reflection, the SO(2)-constrained maximiser flips the last singular
    # vector, and the optimal scale becomes (d1 - d2)/D rather than (d1 + d2)/D.
    reflected <- det(R) < 0
    if (reflected) {
      v_mod <- svd_m$v
      v_mod[, 2] <- -v_mod[, 2]
      R <- tcrossprod(v_mod, svd_m$u)
    }

    scale_pro <- if (reflected) {
      (svd_m$d[1] - svd_m$d[2]) / denominator
    } else {
      (svd_m$d[1] + svd_m$d[2]) / denominator
    }
    a <- scale_pro * R[1, 1]
    b <- scale_pro * R[2, 1]

  } else {
    # Classical Total Least Squares (errors-in-variables).
    #
    # Stacking the centred conformal model gives the 2n x 2 design matrix
    #   A = [ u  -v ;  v   u ]   and observation vector  d = [ x ; y ],
    # so that A %*% c(a, b) = d. TLS minimises the squared Frobenius norm of the
    # perturbation to the FULL augmented matrix [A | d], i.e. it attributes error
    # to the source coordinates as well as the target ones. The solution is the
    # right singular vector of [A | d] belonging to the smallest singular value:
    #   (a, b)^T = -(1 / v_33) * (v_31, v_32)^T
    aug <- rbind(
      cbind(u_i, -v_i, x_i),
      cbind(v_i,  u_i, y_i)
    )
    svd_aug <- svd(aug)
    v3 <- svd_aug$v[, 3]

    if (abs(v3[3]) < .Machine$double.eps^0.5) {
      stop("Cannot solve Helmert transformation by TLS: the augmented system is ",
           "degenerate (non-generic TLS problem). Use method = \"ols\".",
           call. = FALSE)
    }

    a <- -v3[1] / v3[3]
    b <- -v3[2] / v3[3]
  }

  # Physical geodetic parameters
  scale_factor <- sqrt(a^2 + b^2)
  theta_rad <- atan2(b, a)
  theta_deg <- theta_rad * 180 / pi
  tx <- x_mean - (a * u_mean - b * v_mean)
  ty <- y_mean - (b * u_mean + a * v_mean)

  parameters <- c(
    scale = scale_factor,
    theta_deg = theta_deg,
    theta_rad = theta_rad,
    tx = tx,
    ty = ty
  )

  # Residuals and Statistical Inference
  pred_x <- tx + a * source_x - b * source_y
  pred_y <- ty + b * source_x + a * source_y
  res_x <- target_x - pred_x
  res_y <- target_y - pred_y

  df <- 2 * n - 4
  sigma0_sq <- if (df > 0) sum(res_x^2 + res_y^2) / df else NA_real_
  sigma0 <- sqrt(sigma0_sq)

  # Parameter standard errors via Gauss-Markov dispersion matrix
  if (!is.na(sigma0_sq) && sigma0_sq > 0) {
    se_a <- sqrt(sigma0_sq / denominator)
    se_b <- se_a
    se_scale <- se_a
    se_theta_rad <- se_a / scale_factor
    se_theta_deg <- se_theta_rad * 180 / pi
    se_t <- sqrt(sigma0_sq * (1 / n + (u_mean^2 + v_mean^2) / denominator))
    se_params <- c(
      scale = se_scale,
      theta_deg = se_theta_deg,
      theta_rad = se_theta_rad,
      tx = se_t,
      ty = se_t
    )
  } else {
    se_params <- c(scale = NA_real_, theta_deg = NA_real_, theta_rad = NA_real_,
                   tx = NA_real_, ty = NA_real_)
  }

  # --- Create Model Object ---
  model <- structure(
    list(
      coefficients = c(a = a, b = b),
      centroids = c(u_mean = u_mean, v_mean = v_mean,
                    x_mean = x_mean, y_mean = y_mean),
      parameters = parameters,
      se = se_params,
      sigma0 = sigma0,
      df = df,
      residuals = data.frame(rx = res_x, ry = res_y),
      method = method
    ),
    class = "helmert"
  )

  return(model)
}


#' Print a Helmert Model Object
#'
#' @description S3 print method for objects of class `helmert`.
#' @param x An object of class `helmert`.
#' @param ... Additional arguments (not used).
#' @export
print.helmert <- function(x, ...) {
  method_str <- switch(if (is.null(x$method)) "ols" else x$method,
                       tls = "TLS (errors-in-variables)",
                       procrustes = "Scaled orthogonal Procrustes",
                       "OLS")
  cat(sprintf("--- Helmert Transformation Model (%s) ---\n\n", method_str))
  cat("Helmert Transformation Parameters:\n")
  print(round(x$coefficients, 6))
  print(round(x$centroids, 6))

  if (!is.null(x$parameters)) {
    cat("\nPhysical Geodetic Parameters:\n")
    cat(sprintf("  Scale Factor (s):       %.6f\n", x$parameters["scale"]))
    cat(sprintf("  Rotation Angle:         %.4f deg (%.6f rad)\n",
                x$parameters["theta_deg"], x$parameters["theta_rad"]))
    cat(sprintf("  Translation X (tx):     %.4f\n", x$parameters["tx"]))
    cat(sprintf("  Translation Y (ty):     %.4f\n", x$parameters["ty"]))
  }

  if (!is.null(x$sigma0) && !is.na(x$sigma0)) {
    cat(sprintf("\nStatistical Diagnostics (df = %d):\n", x$df))
    cat(sprintf("  Unit Variance Sigma0:   %.6f\n", x$sigma0))
    if (!is.null(x$se) && !is.na(x$se["scale"])) {
      cat(sprintf("  SE(scale):              %.6f\n", x$se["scale"]))
      cat(sprintf("  SE(theta):              %.4f deg\n", x$se["theta_deg"]))
      cat(sprintf("  SE(tx):                 %.4f\n", x$se["tx"]))
      cat(sprintf("  SE(ty):                 %.4f\n", x$se["ty"]))
    }
  }

  invisible(x)
}

#' Predict Helmert 2D Transformation
#'
#' @description Applies a trained Helmert 2D transformation to new source
#'   coordinates to predict their position in the target coordinate system.
#'
#' @param object An object of class `helmert`, as created by the `helmert()`
#'   function.
#' @param newdata A data frame containing the new source coordinates. It must
#'   have columns with the same names as the original source data, typically
#'   `source_x` and `source_y`.
#' @param ... Additional arguments (not used).
#'
#' @return A data frame with the predicted `target_x` and `target_y`
#'   coordinates.
#' @export
#'
#' @examples
#' # Create some sample data
#' source_coords <- data.frame(
#'   source_x = c(10, 20, 30, 15),
#'   source_y = c(15, 25, 10, 5)
#' )
#'
#' target_coords <- data.frame(
#'   target_x = c(110.5, 119.8, 131.2, 114.0),
#'   target_y = c(114.5, 125.2, 109.8, 104.9)
#' )
#'
#' # 1. Train the Helmert model
#' helmert_model <- helmert(
#'   source_x = source_coords$source_x,
#'   source_y = source_coords$source_y,
#'   target_x = target_coords$target_x,
#'   target_y = target_coords$target_y
#' )
#'
#' # 2. Define new points to transform
#' new_points <- data.frame(
#'   source_x = c(25, 5),
#'   source_y = c(18, 8)
#' )
#'
#' # 3. Predict the target coordinates for the new points
#' predicted_points <- predict(helmert_model, new_points)
#'
#' print(predicted_points)
predict.helmert <- function(object, newdata, ...) {

  # --- Input Validation ---
  if (!inherits(object, "helmert")) {
    stop("The 'object' provided must be of class 'helmert'.", call. = FALSE)
  }
  required_cols <- c("source_x", "source_y")
  if (!is.data.frame(newdata) || !all(required_cols %in% names(newdata))) {
    stop(
      "'newdata' must be a data frame with columns 'source_x' and 'source_y'.",
      call. = FALSE)
  }
  if (any(!is.finite(newdata$source_x)) || any(!is.finite(newdata$source_y))) {
    stop(
      "All 'source_x' and 'source_y' values in 'newdata' must be finite.",
         call. = FALSE)
  }

  # --- Extract Parameters ---

  # Extract coefficients (a, b) and centroids from the model object
  a <- object$coefficients["a"]
  b <- object$coefficients["b"]
  u_mean <- object$centroids["u_mean"]
  v_mean <- object$centroids["v_mean"]
  x_mean <- object$centroids["x_mean"]
  y_mean <- object$centroids["y_mean"]

  # Extract new source coordinates from the newdata data frame
  u_new <- newdata$source_x
  v_new <- newdata$source_y

  # --- Apply Transformation ---
  # The transformation is applied relative to the centroids for numerical
  # stability.
  pred_x <- x_mean + a * (u_new - u_mean) - b * (v_new - v_mean)
  pred_y <- y_mean + b * (u_new - u_mean) + a * (v_new - v_mean)

  # --- Return Results ---
  return(data.frame(target_x = pred_x, target_y = pred_y))
}
