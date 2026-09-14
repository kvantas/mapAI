#' Summary Method for tin_linear_fit Objects
#' @param object An object of class `tin_linear_fit`.
#' @param ... Additional arguments (not used).
#' @export
summary.tin_linear_fit <- function(object, ...) {
  n_pts <- nrow(object$gcp)
  n_tri <- nrow(object$triangles)
  n_inv <- sum(object$is_inverted, na.rm = TRUE)
  det_range <- range(object$det_J, na.rm = TRUE)

  res <- list(
    n_points = n_pts,
    n_triangles = n_tri,
    n_inverted = n_inv,
    det_J_range = det_range,
    fallback = object$fallback
  )
  class(res) <- "summary.tin_linear_fit"
  res
}

#' Print Method for summary.tin_linear_fit Objects
#' @param x An object of class `summary.tin_linear_fit`.
#' @param ... Additional arguments (not used).
#' @export
print.summary.tin_linear_fit <- function(x, ...) {
  cat("Piecewise Affine Delaunay Triangulation Fit\n")
  cat("  Control Points (GCPs):", x$n_points, "\n")
  cat("  Triangular Facets:    ", x$n_triangles, "\n")
  cat("  Inverted Facets (det(J) <= 0):", x$n_inverted, "\n")
  cat(sprintf("  Jacobian det(J) range: [%.4f, %.4f]\n", x$det_J_range[1], x$det_J_range[2]))
  cat("  Extrapolation Fallback:", x$fallback, "\n")
  invisible(x)
}

#' Print Method for tin_linear_fit Objects
#' @param x An object of class `tin_linear_fit`.
#' @param ... Additional arguments (not used).
#' @export
print.tin_linear_fit <- function(x, ...) {
  print(summary(x, ...))
  invisible(x)
}

#' Summary Method for tin_akima_fit Objects
#' @param object An object of class `tin_akima_fit`.
#' @param ... Additional arguments (not used).
#' @export
summary.tin_akima_fit <- function(object, ...) {
  res <- list(
    n_points = nrow(object$dat),
    fallback_summary = summary(object$fallback_fit)
  )
  class(res) <- "summary.tin_akima_fit"
  res
}

#' Print Method for summary.tin_akima_fit Objects
#' @param x An object of class `summary.tin_akima_fit`.
#' @param ... Additional arguments (not used).
#' @export
print.summary.tin_akima_fit <- function(x, ...) {
  cat("Akima C1 Triangulated Spline Fit\n")
  cat("  Control Points (GCPs):", x$n_points, "\n")
  cat("  Extrapolation Fallback: Linear Model\n")
  invisible(x)
}

#' Print Method for tin_akima_fit Objects
#' @param x An object of class `tin_akima_fit`.
#' @param ... Additional arguments (not used).
#' @export
print.tin_akima_fit <- function(x, ...) {
  print(summary(x, ...))
  invisible(x)
}

#' Summary Method for hybrid_helmert_tin_fit Objects
#' @param object An object of class `hybrid_helmert_tin_fit`.
#' @param ... Additional arguments (not used).
#' @export
summary.hybrid_helmert_tin_fit <- function(object, ...) {
  tris <- interp::triangles(object$mesh)
  res <- list(
    h_fit = object$h_fit,
    n_points = nrow(object$gcp),
    n_triangles = nrow(tris),
    max_res_x = max(abs(object$res_x)),
    max_res_y = max(abs(object$res_y))
  )
  class(res) <- "summary.hybrid_helmert_tin_fit"
  res
}

#' Print Method for summary.hybrid_helmert_tin_fit Objects
#' @param x An object of class `summary.hybrid_helmert_tin_fit`.
#' @param ... Additional arguments (not used).
#' @export
print.summary.hybrid_helmert_tin_fit <- function(x, ...) {
  cat("Hybrid Helmert-TIN Fit\n")
  cat("  Base Model: 4-Parameter Conformal Similarity (Helmert)\n")
  print(x$h_fit)
  cat("  Local Mesh: Delaunay Triangulation on Residuals\n")
  cat("    Control Points:   ", x$n_points, "\n")
  cat("    Triangular Facets:", x$n_triangles, "\n")
  cat(sprintf("    Max Residual dx:  %.6f\n", x$max_res_x))
  cat(sprintf("    Max Residual dy:  %.6f\n", x$max_res_y))
  invisible(x)
}

#' Print Method for hybrid_helmert_tin_fit Objects
#' @param x An object of class `hybrid_helmert_tin_fit`.
#' @param ... Additional arguments (not used).
#' @export
print.hybrid_helmert_tin_fit <- function(x, ...) {
  print(summary(x, ...))
  invisible(x)
}

#' Summary Method for hybrid_affine_tin_fit Objects
#' @param object An object of class `hybrid_affine_tin_fit`.
#' @param ... Additional arguments (not used).
#' @export
summary.hybrid_affine_tin_fit <- function(object, ...) {
  tris <- interp::triangles(object$mesh)
  res <- list(
    lm_x = object$lm_x,
    lm_y = object$lm_y,
    n_points = nrow(object$gcp),
    n_triangles = nrow(tris),
    max_res_dx = max(abs(object$res_dx)),
    max_res_dy = max(abs(object$res_dy))
  )
  class(res) <- "summary.hybrid_affine_tin_fit"
  res
}

#' Print Method for summary.hybrid_affine_tin_fit Objects
#' @param x An object of class `summary.hybrid_affine_tin_fit`.
#' @param ... Additional arguments (not used).
#' @export
print.summary.hybrid_affine_tin_fit <- function(x, ...) {
  cat("Hybrid Affine-TIN Fit\n")
  cat("  Base Model: 6-Parameter Affine Polynomials (lm)\n")
  cat("  Local Mesh: Delaunay Triangulation on Residuals\n")
  cat("    Control Points:   ", x$n_points, "\n")
  cat("    Triangular Facets:", x$n_triangles, "\n")
  cat(sprintf("    Max Residual dx:  %.6f\n", x$max_res_dx))
  cat(sprintf("    Max Residual dy:  %.6f\n", x$max_res_dy))
  invisible(x)
}

#' Print Method for hybrid_affine_tin_fit Objects
#' @param x An object of class `hybrid_affine_tin_fit`.
#' @param ... Additional arguments (not used).
#' @export
print.hybrid_affine_tin_fit <- function(x, ...) {
  print(summary(x, ...))
  invisible(x)
}
