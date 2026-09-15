#' @title Apply a Trained PAI Model to Correct a Raster Map
#' @description Applies a trained `pai_model` object to a `terra` `SpatRaster`,
#'   rectifying its spatial alignment based on the learned transformation.
#'
#' @details
#' This function brings the Positional Accuracy Improvement (PAI) framework to
#' raster geospatial data (e.g., scanned historical maps, aerial imagery,
#' digital elevation models, or thematic raster grids).
#'
#' **Forward vs. Inverse Mapping in Raster Warping:**
#'
#' While vector PAI shifts existing vertices forward
#' (\eqn{(x, y) \to (x + dx, y + dy)}), raster data exists on a rigid, regular
#' grid. Applying forward mapping to raster pixels would result in an irregular
#' point cloud with gaps ("holes") in expanded areas and collisions in compressed
#' areas.
#'
#' To produce a valid, regular raster grid, `apply_pai_raster` employs
#' **inverse (backward) mapping**:
#' \enumerate{
#'   \item Defines the regular coordinate grid for the target (corrected) raster.
#'   \item Projects each target cell center backwards through the transformation
#'     to find its corresponding location in the source (distorted) image.
#'   \item Resamples the pixel value from the source raster using the requested
#'     interpolation method.
#' }
#'
#' **Model Inversion Handling:**
#' \itemize{
#'   \item \strong{Helmert models:} Uses the exact closed-form analytical inverse
#'     of the 4-parameter conformal similarity transformation.
#'   \item \strong{All other models (`gam_biv`, `tps`, `lm`, the TIN and hybrid
#'     families, and custom models):} Employs a damped fixed-point
#'     (Krasnosel'skii-Mann averaged) iterative coordinate inversion solver:
#'     \deqn{\mathbf{s}^{(k+1)} = \mathbf{s}^{(k)} - \lambda \left(\mathbf{s}^{(k)} + \mathbf{d}\left(\mathbf{s}^{(k)}\right) - \mathbf{t}\right)}
#'     where \eqn{\mathbf{t}} is the target cell center, \eqn{\mathbf{d}} is the
#'     forward displacement field, and \eqn{\lambda} (`lambda`, default 0.7) is the
#'     damping parameter. Writing the update as
#'     \eqn{\mathbf{s}^{(k+1)} = (1 - \lambda)\mathbf{s}^{(k)} + \lambda T(\mathbf{s}^{(k)})}
#'     with \eqn{T(\mathbf{s}) = \mathbf{t} - \mathbf{d}(\mathbf{s})} shows it to be
#'     the averaged (Krasnosel'skii-Mann) iteration; it converges linearly when the
#'     eigenvalues of \eqn{\mathbf{J} = \mathbf{I} + \partial\mathbf{d}/\partial\mathbf{s}}
#'     lie in \eqn{(0, 2/\lambda)}. Damping only helps near fold-over; for the usual
#'     case \eqn{\|\partial\mathbf{d}/\partial\mathbf{s}\| \ll 1}, `lambda = 1`
#'     converges faster.
#'
#'     Iteration halts when the fixed-point \emph{residual}
#'     \eqn{\|\mathbf{s}^{(k)} + \mathbf{d}(\mathbf{s}^{(k)}) - \mathbf{t}\|} falls
#'     below `tol`, taken as a maximum over all evaluated points, or when
#'     `max_iter` is reached. Note this is the residual, not the step size (the
#'     step is \eqn{\lambda} times the residual). If the tolerance is not met, or
#'     if the iteration diverges, a warning is issued reporting the residual
#'     actually achieved -- the result is \strong{not} silently returned as
#'     converged.
#'
#'     `lm` is included here for uniformity; it is affine and analytically
#'     invertible, so iterating is merely wasteful, not inaccurate.
#' }
#'
#' @references
#' \itemize{
#'   \item Wolberg, G. (1990). \emph{Digital Image Warping}. IEEE Computer Society Press.
#'   \item Vantas, K., & Mirkopoulou, E. (2025). \emph{mapAI: An R Package for Positional Accuracy Improvement of Vector Maps}.
#' }
#'
#' **Interpolation Methods:**
#' \itemize{
#'   \item \strong{"bilinear" (Default):} Bilinear interpolation. Recommended
#'     for continuous rasters such as elevation models, satellite/aerial imagery,
#'     and scanned paper maps.
#'   \item \strong{"near" or "simple":} Nearest-neighbor assignment. Essential for
#'     discrete, integer, or categorical rasters (e.g., land cover classification,
#'     soil classes, or parcel IDs) to ensure original class values are preserved
#'     without generating non-existent intermediate values.
#' }
#'
#' **Performance Optimization (`mesh_step`):**
#'
#' Evaluating non-linear models individually across millions of raster cells
#' can be computationally demanding. By setting `mesh_step` (e.g., 10 or 20),
#' `apply_pai_raster` computes exact displacements on a regular subsampled grid mesh
#' and uses high-performance C++ bilinear interpolation (`terra::resample`) to
#' reconstruct the continuous displacement field across the full raster grid.
#' This trades accuracy for speed. The displacement field is reconstructed by
#' bilinear interpolation between mesh nodes spaced \eqn{h = \code{mesh_step} \times \code{res}}
#' apart, so the error is \eqn{O(h^2 \|D^2 \mathbf{d}\|)}: it grows with the square
#' of the step and with the curvature of the displacement field. For the smooth,
#' gently varying fields typical of map rectification a modest `mesh_step` stays
#' well below one pixel, but a strongly curved TPS or GAM field with a large
#' `mesh_step` can depart from the exact solution by several pixels. Verify
#' against `mesh_step = NULL` on a representative subset before relying on it.
#'
#' All operations are strictly conducted in memory and return an in-memory
#' `terra::SpatRaster` object without creating temporary files or directories.
#'
#' @param pai_model An object of class `pai_model` returned by
#'   `train_pai_model()`.
#' @param raster A `terra` `SpatRaster` object, or a character string
#'   specifying the file path to a readable raster file.
#' @param method A character string specifying the interpolation method. One of:
#'   `"bilinear"` (default, for continuous data) or `"near"` / `"simple"`
#'   (nearest neighbor, for categorical data).
#' @param res An optional numeric vector of length 1 or 2 specifying the target
#'   cell resolution. If `NULL` (default), the resolution of `raster` is preserved.
#' @param mesh_step An optional positive integer specifying the subsampling step
#'   for grid mesh interpolation. If `NULL` (default) or `1`, displacements are
#'   computed directly for each cell center.
#' @param aoi An optional `sf` or `terra` `SpatVector` polygon object representing
#'   an Area of Interest. If provided, cells outside the AOI are masked to `NA`.
#' @param ext Target raster extent. Can be `NULL` (default, preserves source
#'   raster extent), `"auto"` (automatically computes bounding extent from
#'   forward-projected perimeter coordinates), a `terra::SpatExtent` object, or
#'   a numeric vector of length 4 \code{c(xmin, xmax, ymin, ymax)}.
#' @param max_iter Maximum number of iterations for the damped fixed-point
#'   iterative coordinate inversion. Defaults to 15.
#' @param tol Convergence tolerance in coordinate units (e.g., meters) for
#'   the iterative coordinate inversion, applied to the fixed-point residual as a
#'   maximum over all evaluated points. Defaults to 1e-4. Failure to reach it
#'   raises a warning.
#' @param lambda Damping parameter \eqn{\lambda} of the fixed-point iteration, in
#'   (0, 2]. Defaults to 0.7. Damping stabilises the iteration near fold-over;
#'   `lambda = 1` (undamped) converges faster for well-behaved displacement
#'   fields.
#' @param ... Additional arguments passed on to `predict.pai_model()`.
#'
#' @return An in-memory `terra` `SpatRaster` object with corrected spatial alignment.
#'
#' @importFrom terra rast res res<- crds values values<- nlyr extract resample mask project vect crs ext ext<-
#' @importFrom stats predict
#' @export
#' @examples
#' \dontrun{
#' library(terra)
#'
#' # Create demo data and train a model
#' demo_data <- create_demo_data()
#' gam_model <- train_pai_model(demo_data$gcp, method = "gam_biv")
#'
#' # Create an in-memory synthetic raster matching the GCP extent
#' gcp_ext <- terra::ext(
#'   min(demo_data$gcp$source_x), max(demo_data$gcp$source_x),
#'   min(demo_data$gcp$source_y), max(demo_data$gcp$source_y)
#' )
#' demo_rast <- terra::rast(gcp_ext, nrows = 30, ncols = 30)
#' terra::values(demo_rast) <- seq_len(terra::ncell(demo_rast))
#'
#' # Apply the model to correct the raster
#' corrected_rast <- apply_pai_raster(gam_model, demo_rast, method = "bilinear")
#' print(corrected_rast)
#' }
apply_pai_raster <- function(pai_model,
                             raster,
                             method = c("bilinear", "near", "simple"),
                             res = NULL,
                             mesh_step = NULL,
                             aoi = NULL,
                             ext = NULL,
                             max_iter = 15,
                             tol = 1e-4,
                             lambda = 0.7,
                             ...) {

  # --- 1. Input Validation ---
  if (!inherits(pai_model, "pai_model")) {
    stop("`pai_model` must be an object of class 'pai_model'.", call. = FALSE)
  }

  if (is.character(raster)) {
    if (!file.exists(raster)) {
      stop("Raster file not found: ", raster, call. = FALSE)
    }
    raster <- terra::rast(raster)
  }

  if (!inherits(raster, "SpatRaster")) {
    stop("`raster` must be a terra SpatRaster object or path to a valid raster file.", call. = FALSE)
  }

  method <- match.arg(tolower(method), c("bilinear", "near", "simple"))
  extract_method <- if (method %in% c("near", "simple")) "simple" else "bilinear"

  if (!is.null(mesh_step)) {
    if (!is.numeric(mesh_step) || mesh_step < 1) {
      stop("`mesh_step` must be a positive integer.", call. = FALSE)
    }
    mesh_step <- as.integer(mesh_step)
  }

  if (!is.null(aoi)) {
    if (!inherits(aoi, c("sf", "sfc", "SpatVector"))) {
      stop("`aoi` must be an sf or terra SpatVector polygon object.", call. = FALSE)
    }
  }

  if (!is.null(ext)) {
    if (!identical(ext, "auto") && !inherits(ext, "SpatExtent") &&
        !(is.numeric(ext) && length(ext) == 4)) {
      stop("`ext` must be 'auto', a terra SpatExtent, or a numeric vector of length 4 (xmin, xmax, ymin, ymax).",
           call. = FALSE)
    }
  }

  if (!is.numeric(max_iter) || max_iter < 1) {
    stop("`max_iter` must be a positive integer.", call. = FALSE)
  }
  max_iter <- as.integer(max_iter)

  if (!is.numeric(tol) || tol <= 0) {
    stop("`tol` must be a positive number.", call. = FALSE)
  }

  if (!is.numeric(lambda) || length(lambda) != 1 || lambda <= 0 || lambda > 2) {
    stop("`lambda` must be a single number in (0, 2].", call. = FALSE)
  }

  message("Applying PAI model to raster...")

  # --- 2. Define Target Raster Template (in memory) ---
  target_template <- terra::rast(raster)

  # Build a template that honours `out_res` EXACTLY and covers all of `want_ext`.
  # Using `ext<-` then `res<-` cannot do this: `ext<-` keeps nrow/ncol and
  # rescales the cell size, and a following `res<-` rounds the cell count, which
  # can shrink the extent and clip the warped image. Cells are whole, so the
  # realised extent is the requested one expanded by at most one cell on the
  # upper edges.
  make_template <- function(want_ext, out_res) {
    if (length(out_res) == 1) out_res <- c(out_res, out_res)
    ncol_new <- max(1L, as.integer(ceiling((want_ext$xmax - want_ext$xmin) / out_res[1])))
    nrow_new <- max(1L, as.integer(ceiling((want_ext$ymax - want_ext$ymin) / out_res[2])))
    terra::rast(
      xmin = want_ext$xmin, xmax = want_ext$xmin + ncol_new * out_res[1],
      ymin = want_ext$ymin, ymax = want_ext$ymin + nrow_new * out_res[2],
      ncols = ncol_new, nrows = nrow_new,
      crs = terra::crs(raster)
    )
  }
  if (!is.null(ext)) {
    if (identical(ext, "auto")) {
      e <- terra::ext(raster)

      # For an injective (fold-free) warp the image of the boundary bounds the
      # image of the region, so sampling the perimeter is sound in principle.
      # A fixed 10 samples per edge is not: a local bulge between samples is
      # clipped silently. Scale the sampling with the raster, and also probe a
      # coarse interior grid so that a fold-over (det(J) <= 0, where the boundary
      # argument fails outright) is far less likely to push content out of frame.
      n_edge <- max(50L, 2L * as.integer(max(terra::nrow(raster),
                                             terra::ncol(raster))))
      xs <- seq(e$xmin, e$xmax, length.out = n_edge)
      ys <- seq(e$ymin, e$ymax, length.out = n_edge)

      n_grid <- max(10L, as.integer(ceiling(sqrt(n_edge))))
      gx <- seq(e$xmin, e$xmax, length.out = n_grid)
      gy <- seq(e$ymin, e$ymax, length.out = n_grid)
      interior <- expand.grid(source_x = gx, source_y = gy)

      probe <- rbind(
        data.frame(
          source_x = c(xs, xs, rep(e$xmin, length(ys)), rep(e$xmax, length(ys))),
          source_y = c(rep(e$ymin, length(xs)), rep(e$ymax, length(xs)), ys, ys)
        ),
        interior
      )

      pred_probe <- stats::predict(pai_model, newdata = probe, ...)
      target_probe_x <- probe$source_x + pred_probe$dx
      target_probe_y <- probe$source_y + pred_probe$dy

      if (!any(is.finite(target_probe_x)) || !any(is.finite(target_probe_y))) {
        stop("Cannot determine an automatic target extent: the model produced ",
             "no finite predictions over the raster extent.", call. = FALSE)
      }

      # Pad by one cell so the bounding box does not cut the outermost pixels.
      pad <- terra::res(raster)
      auto_ext <- terra::ext(
        min(target_probe_x, na.rm = TRUE) - pad[1],
        max(target_probe_x, na.rm = TRUE) + pad[1],
        min(target_probe_y, na.rm = TRUE) - pad[2],
        max(target_probe_y, na.rm = TRUE) + pad[2]
      )
      target_template <- make_template(
        auto_ext, if (!is.null(res)) res else terra::res(raster))
    } else {
      target_template <- make_template(
        terra::ext(ext), if (!is.null(res)) res else terra::res(raster))
    }
  } else {
    if (!is.null(res)) {
      terra::res(target_template) <- res
    }
  }

  # --- 3. Compute Inverse Mapping (Target -> Source Coordinates) ---
  calc_source_coords <- function(target_coords) {
    tx <- target_coords[, 1]
    ty <- target_coords[, 2]

    {
      # Invert the forward model. Identify Helmert fits by the class of the
      # fitted object, which is what actually determines whether the closed-form
      # inverse below applies; matching on a user-supplied label could route a
      # custom model here and then index coefficients it does not have.
      is_helmert <- inherits(pai_model$model, "helmert")

      if (is_helmert) {
        coefs <- pai_model$model$coefficients
        cents <- pai_model$model$centroids
        a <- coefs["a"]
        b <- coefs["b"]
        det_ab <- a^2 + b^2

        dx0 <- tx - cents["x_mean"]
        dy0 <- ty - cents["y_mean"]

        u <- (a * dx0 + b * dy0) / det_ab
        v <- (-b * dx0 + a * dy0) / det_ab

        sx <- u + cents["u_mean"]
        sy <- v + cents["v_mean"]
      } else {
        # Damped fixed-point iterative coordinate inversion:
        # s^(k+1) = s^(k) - lambda * (s^(k) + d(s^(k)) - t)
        p0 <- stats::predict(pai_model, newdata = data.frame(source_x = tx, source_y = ty), ...)
        curr_sx <- tx - p0$dx
        curr_sy <- ty - p0$dy

        converged <- FALSE
        diverged <- FALSE
        n_unconverged <- length(tx)
        final_err <- NA_real_
        used_iter <- 0L

        for (iter in seq_len(max_iter)) {
          used_iter <- iter
          d <- stats::predict(pai_model,
                              newdata = data.frame(source_x = curr_sx,
                                                   source_y = curr_sy), ...)
          ex <- curr_sx + d$dx - tx
          ey <- curr_sy + d$dy - ty
          err <- sqrt(ex^2 + ey^2)

          finite_err <- err[is.finite(err)]
          if (length(finite_err) == 0) {
            # Every residual is non-finite: the inversion has failed outright.
            # Breaking here used to be treated as convergence.
            diverged <- TRUE
            break
          }

          max_err <- max(finite_err)
          final_err <- max_err
          n_unconverged <- sum(finite_err >= tol)

          if (!is.finite(max_err)) {
            diverged <- TRUE
            break
          }
          if (max_err < tol) {
            converged <- TRUE
            break
          }

          curr_sx <- curr_sx - lambda * ex
          curr_sy <- curr_sy - lambda * ey
        }

        if (diverged) {
          warning("Coordinate inversion diverged: the displacement field is not ",
                  "contractive under the damped fixed-point iteration (this ",
                  "happens where the transformation folds over). The returned ",
                  "raster is unreliable in the affected area.", call. = FALSE)
        } else if (!converged) {
          warning(sprintf(
            paste0("Coordinate inversion did not converge: maximum residual ",
                   "%.3g exceeds tol = %.3g after %d iteration(s); %d point(s) ",
                   "are still above tolerance. Increase `max_iter` or relax ",
                   "`tol`."),
            final_err, tol, used_iter, n_unconverged), call. = FALSE)
        }

        sx <- curr_sx
        sy <- curr_sy
      }
    }
    cbind(x = sx, y = sy)
  }

  # --- 4. Evaluate Displacements (Direct or Mesh-based) ---
  if (!is.null(mesh_step) && mesh_step > 1) {
    # Build the coarse mesh explicitly rather than via `res<-`, which keeps
    # xmin/ymin and rounds the cell count, so the coarse extent only lines up
    # with the target when mesh_step divides the dimensions exactly.
    #
    # The mesh is also grown by one coarse cell on every side. terra::resample()
    # interpolates between coarse CELL CENTRES, so without that margin the outer
    # half-cell ring of the target lies outside the coarse centre hull and comes
    # back NA -- silently trimming the warped image even when the extents match.
    t_ext <- terra::ext(target_template)
    t_res <- terra::res(target_template)
    coarse_res <- t_res * mesh_step

    r_coarse <- terra::rast(
      xmin = t_ext$xmin - coarse_res[1],
      xmax = t_ext$xmax + coarse_res[1],
      ymin = t_ext$ymin - coarse_res[2],
      ymax = t_ext$ymax + coarse_res[2],
      ncols = as.integer(ceiling(terra::ncol(target_template) / mesh_step)) + 2L,
      nrows = as.integer(ceiling(terra::nrow(target_template) / mesh_step)) + 2L,
      crs = terra::crs(target_template)
    )

    coarse_xy <- terra::crds(r_coarse)
    coarse_src <- calc_source_coords(coarse_xy)

    c_dx <- coarse_src[, 1] - coarse_xy[, 1]
    c_dy <- coarse_src[, 2] - coarse_xy[, 2]

    disp_coarse <- c(r_coarse, r_coarse)
    terra::values(disp_coarse[[1]]) <- c_dx
    terra::values(disp_coarse[[2]]) <- c_dy

    # Interpolate displacement field to full raster resolution
    disp_fine <- terra::resample(disp_coarse, target_template, method = "bilinear")
    target_xy <- terra::crds(target_template)
    source_xy <- cbind(
      x = target_xy[, 1] + terra::values(disp_fine[[1]])[, 1],
      y = target_xy[, 2] + terra::values(disp_fine[[2]])[, 1]
    )
  } else {
    target_xy <- terra::crds(target_template)
    source_xy <- calc_source_coords(target_xy)
  }

  # --- 5. Extract / Resample Pixel Values ---
  sampled <- terra::extract(raster, source_xy, method = extract_method)

  # --- 6. Assemble Corrected In-Memory Raster ---
  out_raster <- terra::rast(target_template, nlyrs = terra::nlyr(raster))
  names(out_raster) <- names(raster)
  terra::values(out_raster) <- as.matrix(sampled)

  # --- 7. Apply AOI Masking if Requested ---
  if (!is.null(aoi)) {
    if (inherits(aoi, c("sf", "sfc"))) {
      aoi <- terra::vect(aoi)
    }
    if (inherits(aoi, "SpatVector")) {
      if (terra::crs(aoi) != terra::crs(out_raster)) {
        aoi <- terra::project(aoi, terra::crs(out_raster))
      }
      out_raster <- terra::mask(out_raster, aoi)
    }
  }

  message("Correction complete.")
  return(out_raster)
}
