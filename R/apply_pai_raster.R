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
#'   \item \strong{Models trained with `direction = "inverse"`:} Directly evaluate
#'     the displacement from target to source coordinates in a single step.
#'   \item \strong{Helmert models (`direction = "forward"`):} Uses the exact
#'     closed-form analytical inverse of the 4-parameter conformal similarity transformation.
#'   \item \strong{Non-linear models (`gam_biv`, `tps`, `lm`, and custom models):}
#'     Employs a damped fixed-point (Picard-Mann) iterative coordinate inversion solver:
#'     \deqn{\mathbf{s}^{(k+1)} = \mathbf{s}^{(k)} - \lambda \left(\mathbf{s}^{(k)} + \mathbf{d}\left(\mathbf{s}^{(k)}\right) - \mathbf{t}\right)}
#'     where \eqn{\mathbf{t}} is the target cell center, \eqn{\mathbf{d}} is the forward displacement
#'     field, \eqn{\lambda = 0.7} is the damping parameter ensuring contraction stability, and
#'     iteration halts when \eqn{\|\mathbf{s}^{(k)} + \mathbf{d}(\mathbf{s}^{(k)}) - \mathbf{t}\| < \epsilon}
#'     (\code{tol = 1e-4}) or \code{max_iter} is reached.
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
#' This provides orders-of-magnitude speedups while maintaining sub-pixel accuracy.
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
#'   the iterative coordinate inversion. Defaults to 1e-4.
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

  message("Applying PAI model to raster...")

  # --- 2. Define Target Raster Template (in memory) ---
  target_template <- terra::rast(raster)
  if (!is.null(ext)) {
    if (identical(ext, "auto")) {
      e <- terra::ext(raster)
      xs <- seq(e$xmin, e$xmax, length.out = 10)
      ys <- seq(e$ymin, e$ymax, length.out = 10)
      perim <- data.frame(
        source_x = c(xs, xs, rep(e$xmin, length(ys)), rep(e$xmax, length(ys))),
        source_y = c(rep(e$ymin, length(xs)), rep(e$ymax, length(xs)), ys, ys)
      )
      pred_perim <- stats::predict(pai_model, newdata = perim, ...)
      target_perim_x <- perim$source_x + pred_perim$dx
      target_perim_y <- perim$source_y + pred_perim$dy
      auto_ext <- terra::ext(
        min(target_perim_x, na.rm = TRUE),
        max(target_perim_x, na.rm = TRUE),
        min(target_perim_y, na.rm = TRUE),
        max(target_perim_y, na.rm = TRUE)
      )
      terra::ext(target_template) <- auto_ext
      if (!is.null(res)) {
        terra::res(target_template) <- res
      } else {
        terra::res(target_template) <- terra::res(raster)
      }
    } else {
      terra::ext(target_template) <- terra::ext(ext)
      if (!is.null(res)) {
        terra::res(target_template) <- res
      }
    }
  } else {
    if (!is.null(res)) {
      terra::res(target_template) <- res
    }
  }

  # --- 3. Compute Inverse Mapping (Target -> Source Coordinates) ---
  model_dir <- attr(pai_model, "direction")
  if (is.null(model_dir)) {
    model_dir <- pai_model$direction
  }
  if (is.null(model_dir)) {
    model_dir <- "forward"
  }

  calc_source_coords <- function(target_coords) {
    tx <- target_coords[, 1]
    ty <- target_coords[, 2]

    if (model_dir == "inverse") {
      # Directly predict inverse displacements
      disp <- stats::predict(pai_model, newdata = data.frame(target_x = tx, target_y = ty), ...)
      sx <- tx + disp$dx
      sy <- ty + disp$dy
    } else {
      # Invert forward model
      is_helmert <- identical(pai_model$method, "helmert") ||
                    inherits(pai_model$model, "helmert") ||
                    (is.list(pai_model$method) && identical(pai_model$method$label, "Helmert Model"))

      if (is_helmert) {
        coefs <- pai_model$model$coefficients
        cents <- pai_model$model$centroids
        a <- coefs["a"]
        b <- coefs["b"]
        det <- a^2 + b^2

        dx0 <- tx - cents["x_mean"]
        dy0 <- ty - cents["y_mean"]

        u <- (a * dx0 + b * dy0) / det
        v <- (-b * dx0 + a * dy0) / det

        sx <- u + cents["u_mean"]
        sy <- v + cents["v_mean"]
      } else {
        # Damped fixed-point iterative coordinate inversion:
        # s^(k+1) = s^(k) - lambda * (s^(k) + d(s^(k)) - t)
        p0 <- stats::predict(pai_model, newdata = data.frame(source_x = tx, source_y = ty), ...)
        curr_sx <- tx - p0$dx
        curr_sy <- ty - p0$dy
        lambda <- 0.7
        for (iter in seq_len(max_iter)) {
          d <- stats::predict(pai_model, newdata = data.frame(source_x = curr_sx, source_y = curr_sy), ...)
          ex <- curr_sx + d$dx - tx
          ey <- curr_sy + d$dy - ty
          max_err <- max(sqrt(ex^2 + ey^2), na.rm = TRUE)
          if (!is.finite(max_err) || max_err < tol) {
            break
          }
          curr_sx <- curr_sx - lambda * ex
          curr_sy <- curr_sy - lambda * ey
        }
        sx <- curr_sx
        sy <- curr_sy
      }
    }
    cbind(x = sx, y = sy)
  }

  # --- 4. Evaluate Displacements (Direct or Mesh-based) ---
  if (!is.null(mesh_step) && mesh_step > 1) {
    # Generate coarse mesh
    r_coarse <- terra::rast(target_template)
    terra::res(r_coarse) <- terra::res(target_template) * mesh_step
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
