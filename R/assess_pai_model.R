#' @title Assess PAI Model Performance
#' @description Performs model validation to estimate a PAI model's predictive
#'   performance using k-fold cross-validation, spatial block CV, buffered spatial
#'   CV, or design-based probability sampling.
#' @details Model validation is crucial for understanding how well a model will
#' generalize to new spatial locations. This function automates this process
#' across both non-spatial and spatial cross-validation strategies.
#'
#' \strong{Validation Strategies}:
#'   \itemize{
#'     \item \strong{`random`}: Standard k-fold cross-validation with random fold assignment.
#'     \item \strong{`spatial`}: Spatial k-fold cross-validation clustering coordinates via k-means.
#'     \item \strong{`spatial_block`}: Regular geographic grid block cross-validation,
#'       partitioning space into contiguous rectangular tiles and assigning whole
#'       tiles to folds. Block \emph{size} is controlled by `block_size`,
#'       independently of `k_folds`.
#'     \item \strong{`spatial_buffered`}: Buffered spatial cross-validation. Partitions
#'       space into blocks and enforces a spatial exclusion dead-zone buffer \eqn{d_{\text{buffer}}}:
#'       training observations satisfying \eqn{\min \|\mathbf{s}_{\text{train}} - \mathbf{s}_{\text{test}}\| \le d_{\text{buffer}}}
#'       are omitted from the training set for each fold, which \emph{reduces}
#'       spatial autocorrelation data leakage (Roberts et al., 2017). It removes
#'       leakage only if \eqn{d_{\text{buffer}}} exceeds the range of residual
#'       spatial autocorrelation; `mapAI` does not estimate that range, so the
#'       default is a geometric 5\% of the map extent and should be replaced by a
#'       variogram-derived value where one is available.
#'     \item \strong{`probability`}: A single train/test split drawn by simple random
#'       sampling without replacement. This is a holdout, \strong{not}
#'       cross-validation: there is no replication and no dispersion estimate, and
#'       it carries no design-based inferential guarantee (that would require a
#'       probability sample of the map domain with known inclusion probabilities
#'       and a Horvitz-Thompson-type estimator, neither of which applies to an
#'       opportunistic GCP set).
#'     \item \strong{`stratified`}: Stratified k-fold cross-validation binned by displacement vector magnitude.
#'       Note that the strata are defined on a function of the response, so every
#'       fold carries the same displacement-magnitude distribution. This
#'       mechanically deflates `SD_RMSE_2D` and makes that dispersion figure
#'       non-comparable with the other schemes.
#'   }
#'
#' \strong{Performance Metrics Formulation}:
#' Out-of-sample predictive accuracy is evaluated using 2D Root Mean Square Error,
#' pooled over every out-of-fold point:
#' \deqn{\operatorname{RMSE}_{2D} = \sqrt{\frac{1}{n_{\text{val}}} \sum_{i=1}^{n_{\text{val}}} \left( (dx_i - \widehat{dx}_i)^2 + (dy_i - \widehat{dy}_i)^2 \right)}}
#' The sum runs over \emph{points}, not folds. Averaging the per-fold RMSEs is a
#' different estimator: it mis-weights folds of unequal size and, because
#' \eqn{\sqrt{\cdot}} is concave, sits below the pooled value. The spatial schemes
#' produce markedly unequal folds, so the two can differ by tens of percent.
#' Per-fold RMSEs are still returned in `details$fold_rmse` as a diagnostic.
#'
#' \strong{On `SD_RMSE_2D`}: this is the spread of the per-fold RMSEs. It has
#' \eqn{k - 1} degrees of freedom and, because the folds share training data, it
#' is \emph{not} a standard error of the cross-validation estimate; no unbiased
#' estimator of k-fold CV variance exists (Bengio & Grandvalet, 2004). Do not
#' construct confidence intervals from it.
#'
#' @references
#' \itemize{
#'   \item Roberts, D. R., et al. (2017). Cross-validation strategies for data with
#'     spatial, temporal, hierarchical, or phylogenetic structure.
#'     \emph{Ecography}, 40(8), 913-929.
#'   \item Bengio, Y., & Grandvalet, Y. (2004). No unbiased estimator of the variance
#'     of k-fold cross-validation. \emph{Journal of Machine Learning Research}, 5, 1089-1105.
#'   \item Valavi, R., Elith, J., Lahoz-Monfort, J. J., & Guillera-Arroita, G. (2019). blockCV: An R package for generating spatially or environmentally separated folds for k-fold cross-validation of species distribution models. \emph{Methods in Ecology and Evolution}, 10(2), 225-232.
#'   \item Vantas, K., & Mirkopoulou, E. (2025). \emph{mapAI: An R Package for Positional Accuracy Improvement of Vector Maps}.
#' }
#'
#' @param gcp_data An `gcp` object of homologous points.
#' @param method A character string or a custom model list for `train_pai_model`.
#' @param validation_type The validation strategy: `"random"`, `"spatial"`,
#'   `"spatial_block"`, `"spatial_buffered"`, `"probability"`, or `"stratified"`.
#'   Defaults to `"random"`.
#' @param k_folds Number of folds for CV (used for `"random"`, `"spatial"`,
#'   `"spatial_block"`, `"spatial_buffered"`, and `"stratified"`). Defaults to 5.
#' @param train_split_ratio Proportion of data for training (used for
#'   `"probability"` only). Defaults to 0.8.
#' @param n_strata Number of strata for stratified CV. Defaults to 4.
#' @param buffer_dist Numeric buffer distance for `"spatial_buffered"`, in the
#'   coordinate units of the GCPs. If `NULL` (default), 5\% of the larger map
#'   span is used. This geometric default is a convenience, not an estimate of the
#'   autocorrelation range. Distances are Euclidean, so the GCPs must be in a
#'   projected CRS.
#' @param block_size Edge length of the spatial blocks, in coordinate units, used
#'   by `"spatial_block"` and `"spatial_buffered"`. If `NULL` (default), a grid is
#'   chosen that yields roughly four blocks per fold, so each fold receives
#'   several spatially disjoint tiles. Set this from the range of residual spatial
#'   autocorrelation when you have estimated it. Note that block size is
#'   independent of `k_folds`: raising `k_folds` alone does not make the blocks
#'   smaller.
#' @param seed An integer for reproducibility. The caller's global RNG state is
#'   restored on exit.
#' @param pai_method Alias for `method` for backward compatibility.
#' @param ... Additional arguments passed to `train_pai_model`.
#'
#' @return An object of class `pai_assessment` containing a summary data frame,
#'   detailed prediction results, and validation parameters.
#'
#' @importFrom stats sd complete.cases quantile kmeans setNames
#' @export
#' @examples
#' \dontrun{
#' demo_data <- create_demo_data(seed = 1)
#' gcp_data <- demo_data$gcp
#'
#' # Assess with RANDOM k-fold CV
#' assess_pai_model(gcp_data, method = "lm", validation_type = "random", k_folds = 5)
#'
#' # Assess with SPATIAL BUFFERED CV
#' assess_pai_model(gcp_data, method = "lm", validation_type = "spatial_buffered", k_folds = 5)
#' }
#'
assess_pai_model <- function(gcp_data,
                             method,
                             validation_type = "random",
                             k_folds = 5,
                             train_split_ratio = 0.8,
                             n_strata = 4,
                             buffer_dist = NULL,
                             block_size = NULL,
                             seed = 123,
                             pai_method = NULL,
                             ...) {

  # Backward compatibility: support pai_method
  if (missing(method) && !is.null(pai_method)) {
    method <- pai_method
  }
  if (missing(method) || is.null(method)) {
    stop("`method` argument is required.", call. = FALSE)
  }

  # Capture the caller's global RNG state BEFORE any seeding, and restore it on
  # exit. set.seed() here (and inside each per-fold train_pai_model call) would
  # otherwise silently reset the stream of a user running this inside their own
  # simulation loop.
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    .old_seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
    on.exit(assign(".Random.seed", .old_seed, envir = globalenv()), add = TRUE)
  } else {
    on.exit(suppressWarnings(rm(".Random.seed", envir = globalenv())), add = TRUE)
  }

  set.seed(seed)

  # --- 1. Input Validation ---
  validate_assessment_inputs(gcp_data, method, validation_type,
                             k_folds, train_split_ratio, n_strata)

  # --- 2. Create Data Splits ---
  splits <- create_resampling_splits(gcp_data,
                                     validation_type,
                                     k_folds,
                                     train_split_ratio,
                                     n_strata,
                                     seed,
                                     buffer_dist,
                                     block_size)

  # --- 3. Run Validation Across All Splits ---
  message(paste("Starting", validation_type, "validation..."))
  all_predictions <- lapply(seq_along(splits), function(i) {
    split <- splits[[i]]
    train_data <- gcp_data[split$train, ]
    test_data <- gcp_data[split$test, ]

    if (length(splits) > 1) {
      message(paste("  Processing Fold", i, "of", length(splits), "..."))
    }
    if (nrow(test_data) == 0) {
      stop(sprintf(
        paste0("Fold %d has no test points. This happens when the number of ",
               "occupied blocks or clusters is smaller than k_folds (%d). ",
               "Reduce k_folds, or choose a validation_type whose partition ",
               "does not depend on the spatial footprint of the GCPs."),
        i, length(splits)), call. = FALSE)
    }

    model <- train_pai_model(gcp_data = train_data,
                             method = method,
                             seed = seed + i, ...)
    predictions <- predict(model, newdata = test_data)

    # Return clean data frame with true and predicted values
    data.frame(
      fold = i,
      n_train = nrow(train_data),
      true_dx = test_data$dx,
      true_dy = test_data$dy,
      pred_dx = predictions$dx,
      pred_dy = predictions$dy
    )
  })
  all_predictions_df <- do.call(rbind, all_predictions)

  # Non-finite predictions must be reported, not silently absorbed. The TIN and
  # hybrid models initialise their predictions to NA and rely on a convex-hull
  # fallback that can itself fail, so this is a live path.
  finite_pred <- is.finite(all_predictions_df$pred_dx) &
                 is.finite(all_predictions_df$pred_dy)
  n_bad <- sum(!finite_pred)
  if (n_bad > 0) {
    warning(sprintf(
      paste0("%d of %d validation prediction(s) were not finite and have been ",
             "excluded from the accuracy metrics. Affected fold(s): %s."),
      n_bad, nrow(all_predictions_df),
      paste(sort(unique(all_predictions_df$fold[!finite_pred])), collapse = ", ")),
      call. = FALSE)
  }
  if (!any(finite_pred)) {
    stop("No finite validation predictions were produced; cannot compute metrics.",
         call. = FALSE)
  }

  # --- 4. Calculate Final Metrics ---
  #
  # The reported RMSE_2D pools the squared residuals of every out-of-fold point,
  # which is the estimator documented above. Averaging the per-fold RMSEs instead
  # is NOT the same quantity: sqrt() is concave, and folds produced by the
  # spatial schemes differ greatly in size, so a fold-mean both mis-weights the
  # folds and sits below the pooled value by Jensen's inequality. The two can
  # differ by tens of percent in either direction.
  ok <- all_predictions_df[finite_pred, , drop = FALSE]
  sq_err <- (ok$true_dx - ok$pred_dx)^2 + (ok$true_dy - ok$pred_dy)^2
  mean_rmse <- sqrt(mean(sq_err))

  # Per-fold RMSEs are retained as a dispersion diagnostic only. Their SD has
  # k - 1 degrees of freedom and, because folds share training data, it is not a
  # standard error of the CV estimate: do not build intervals from it
  # (Bengio & Grandvalet, 2004).
  is_kfold <- validation_type %in% c("random", "spatial", "stratified",
                                     "spatial_block", "spatial_buffered")
  if (is_kfold) {
    fold_rmse <- vapply(split(sq_err, ok$fold), function(e) sqrt(mean(e)),
                        numeric(1))
    sd_rmse <- stats::sd(fold_rmse)
  } else {
    fold_rmse <- c(`1` = mean_rmse)
    sd_rmse <- NA_real_
  }

  # --- 5. Structure and Return Output ---
  summary_df <- data.frame(
    Method = if (is.character(method)) method else method$label,
    ValidationType = validation_type,
    Mean_RMSE_2D = mean_rmse,
    SD_RMSE_2D = sd_rmse
  )

  param_rules <- list(
    random           = c("k_folds"),
    spatial          = c("k_folds"),
    probability      = c("train_split_ratio"),
    stratified       = c("k_folds", "n_strata"),
    spatial_block    = c("k_folds"),
    spatial_buffered = c("k_folds", "buffer_dist")
  )

  valid_params <- param_rules[[validation_type]]

  if (!"k_folds" %in% valid_params) {
    k_folds <- NA_integer_
  }
  if (!"train_split_ratio" %in% valid_params) {
    train_split_ratio <- NA_real_
  }
  if (!"n_strata" %in% valid_params) {
    n_strata <- NA_integer_
  }
  if (!"buffer_dist" %in% valid_params) {
    buffer_dist <- NA_real_
  }

  details <- list(k_folds = k_folds,
                  train_split_ratio = train_split_ratio,
                  n_strata = n_strata,
                  buffer_dist = buffer_dist,
                  splits = splits,
                  fold_rmse = fold_rmse,
                  n_train_per_fold = vapply(splits, function(s) length(s$train),
                                            integer(1)),
                  n_test_per_fold = vapply(splits, function(s) length(s$test),
                                           integer(1)),
                  n_nonfinite_predictions = n_bad)

  result <- list(
    summary = summary_df,
    predictions = all_predictions_df,
    details = details
  )
  class(result) <- "pai_assessment"

  message("Assessment complete.")
  return(result)
}

#' @rdname assess_pai_model
#' @export
cv_pai_model <- assess_pai_model

# Minimum training-set size a buffered fold may have before it is refused, and
# the size below which it is merely flagged. Six is one more than the parameter
# count of the largest closed-form built-in model (the 6-parameter affine `lm`),
# so anything at or below it cannot be fitted with any residual degrees of
# freedom.
MIN_TRAIN_POINTS <- 6L
WARN_TRAIN_POINTS <- 20L

#' Assign GCPs to contiguous rectangular spatial blocks, then blocks to folds
#'
#' @description
#' Block *size* is deliberately decoupled from `k`. In block cross-validation the
#' block edge should reflect the range of residual spatial autocorrelation, and
#' many blocks are then distributed across the folds (Valavi et al., 2019).
#' Tying the grid to `k` (for example `nx = ceiling(sqrt(k))`) yields only about
#' `k` tiles, so each fold becomes a single large quadrant and fold sizes are
#' driven entirely by GCP density.
#'
#' With `block_size = NULL` the grid is chosen to give roughly
#' `BLOCKS_PER_FOLD * k` blocks, so every fold receives several spatially
#' disjoint tiles. Supply `block_size` in coordinate units to set the edge length
#' directly.
#'
#' @return A list with `fold_ids` (one fold index per GCP) and `k` (the realised
#'   number of folds, which is reduced if too few blocks are occupied).
#' @noRd
assign_spatial_blocks <- function(gcp_data, k, block_size = NULL) {
  BLOCKS_PER_FOLD <- 4

  rx <- range(gcp_data$source_x)
  ry <- range(gcp_data$source_y)
  span_x <- max(1e-6, diff(rx))
  span_y <- max(1e-6, diff(ry))

  if (is.null(block_size)) {
    target_blocks <- BLOCKS_PER_FOLD * k
    aspect <- span_x / span_y
    nx <- max(1L, as.integer(round(sqrt(target_blocks * aspect))))
    ny <- max(1L, as.integer(ceiling(target_blocks / nx)))
  } else {
    if (!is.numeric(block_size) || length(block_size) != 1 || block_size <= 0) {
      stop("`block_size` must be a single positive number in coordinate units.",
           call. = FALSE)
    }
    nx <- max(1L, as.integer(ceiling(span_x / block_size)))
    ny <- max(1L, as.integer(ceiling(span_y / block_size)))
  }

  eps <- 1e-9 * max(1, span_x, span_y)
  col_idx <- pmin(floor((gcp_data$source_x - rx[1]) / (span_x + eps) * nx) + 1, nx)
  row_idx <- pmin(floor((gcp_data$source_y - ry[1]) / (span_y + eps) * ny) + 1, ny)
  block_id <- (row_idx - 1) * nx + col_idx

  unique_blocks <- unique(block_id)
  n_occupied <- length(unique_blocks)

  if (n_occupied < 2) {
    stop("Spatial block cross-validation needs at least 2 occupied blocks, ",
         "but the GCPs fall into 1. Supply a smaller `block_size` (the current ",
         "grid is coarser than the extent of the control points).",
         call. = FALSE)
  }

  # rep(1:k, length.out = n_occupied) only emits labels 1..n_occupied when
  # n_occupied < k, which leaves the trailing folds with no test points. Reduce k
  # instead of producing empty folds.
  k_eff <- k
  if (n_occupied < k) {
    k_eff <- as.integer(n_occupied)
    warning(sprintf(
      paste0("Only %d spatial block(s) are occupied by GCPs, fewer than ",
             "k_folds = %d; using %d fold(s) instead. Supply a smaller ",
             "`block_size` to obtain more blocks."),
      n_occupied, k, k_eff), call. = FALSE)
  }

  # Randomise which fold each whole block belongs to; contiguous blocks stay
  # intact, which is what makes this spatial rather than random CV.
  block_fold_map <- stats::setNames(
    sample(rep(seq_len(k_eff), length.out = n_occupied)),
    as.character(unique_blocks)
  )

  list(fold_ids = unname(block_fold_map[as.character(block_id)]), k = k_eff)
}

#' Internal helper to create resampling splits
#' @noRd
#' @importFrom stats sd complete.cases quantile kmeans
create_resampling_splits <- function(gcp_data, type, k, ratio, n_strata, seed,
                                    buffer_dist = NULL, block_size = NULL) {

  set.seed(seed)

  n_pts <- nrow(gcp_data)
  indices <- seq_len(n_pts)

  switch(type,
         "random" = {
           fold_ids <- sample(rep(1:k, length.out = n_pts))
           lapply(
             1:k, function(i) list(train = which(fold_ids != i),
                                   test = which(fold_ids == i)))
         },
         "spatial" = {
           fold_ids <- stats::kmeans(gcp_data[, c("source_x", "source_y")],
                                     centers = k)$cluster
           lapply(
             1:k, function(i) list(train = which(fold_ids != i),
                                   test = which(fold_ids == i)))
         },
         "probability" = {
           train_indices <- sample.int(n = n_pts, size = floor(ratio * n_pts))
           list(list(train = train_indices,
                     test = setdiff(indices, train_indices)))
         },
         "stratified" = {
           # Calculate distortion magnitudes
           dist <- sqrt(gcp_data$dx^2 + gcp_data$dy^2)

           # Create unique break points
           breaks <- unique(stats::quantile(
             dist,
             probs = seq(0, 1, by = 1/n_strata),
             na.rm = TRUE, names = FALSE
           ))

           # Handle low cardinality in breaks
           if (length(breaks) < 2) {
             stop("Could not create sufficient strata from 'dx' and 'dy'.")
           }

           # Assign each point to a stratum bin
           strata_ids <- cut(dist,
                             breaks = breaks,
                             include.lowest = TRUE,
                             labels = FALSE)

           final_fold_ids <- vector("integer", n_pts)

           for (s in unique(strata_ids)) {
             idx_in_stratum <- which(strata_ids == s)
             n_in_stratum <- length(idx_in_stratum)

             if (n_in_stratum > 0) {
               folds_for_stratum <- sample(rep(1:k, length.out = n_in_stratum))
               final_fold_ids[idx_in_stratum] <- folds_for_stratum
             }
           }

           lapply(1:k, function(i) {
             list(
               train = which(final_fold_ids != i),
               test  = which(final_fold_ids == i)
             )
           })
         },
         "spatial_block" = {
           blk <- assign_spatial_blocks(gcp_data, k, block_size)
           fold_ids <- blk$fold_ids

           lapply(seq_len(blk$k), function(i) {
             list(
               train = which(fold_ids != i),
               test = which(fold_ids == i)
             )
           })
         },
         "spatial_buffered" = {
           span_x <- max(1e-6, diff(range(gcp_data$source_x)))
           span_y <- max(1e-6, diff(range(gcp_data$source_y)))

           blk <- assign_spatial_blocks(gcp_data, k, block_size)
           fold_ids <- blk$fold_ids

           # The default is a purely geometric 5% of the map extent. It is NOT
           # derived from the residual autocorrelation range, which is what
           # Roberts et al. (2017) call for; supply `buffer_dist` explicitly if
           # you have estimated that range.
           if (is.null(buffer_dist) || is.na(buffer_dist) || buffer_dist <= 0) {
             buffer_dist <- 0.05 * max(span_x, span_y)
           }

           lapply(seq_len(blk$k), function(i) {
             test_idx <- which(fold_ids == i)
             cand_train_idx <- which(fold_ids != i)

             if (length(test_idx) == 0 || length(cand_train_idx) == 0) {
               return(list(train = cand_train_idx, test = test_idx))
             }

             test_x <- gcp_data$source_x[test_idx]
             test_y <- gcp_data$source_y[test_idx]

             cand_x <- gcp_data$source_x[cand_train_idx]
             cand_y <- gcp_data$source_y[cand_train_idx]

             min_dist_to_test <- vapply(seq_along(cand_train_idx), function(ci) {
               min(sqrt((cand_x[ci] - test_x)^2 + (cand_y[ci] - test_y)^2))
             }, numeric(1))

             train_keep <- cand_train_idx[min_dist_to_test > buffer_dist]

             n_pruned <- length(cand_train_idx) - length(train_keep)

             # A buffer large relative to GCP spacing can leave a fold with too
             # few points to fit anything meaningful. Previously this was silent
             # below 3 points, and the "repair" re-admitted the very points the
             # buffer had excluded -- violating the dead-zone guarantee the
             # method exists to provide. Refuse instead, and say why.
             if (length(train_keep) < MIN_TRAIN_POINTS) {
               stop(sprintf(
                 paste0("Fold %d: the %.4g-unit buffer pruned %d of %d training ",
                        "points, leaving %d (minimum %d). Reduce `buffer_dist`, ",
                        "reduce `k_folds`, or use validation_type = ",
                        "\"spatial_block\"."),
                 i, buffer_dist, n_pruned, length(cand_train_idx),
                 length(train_keep), MIN_TRAIN_POINTS), call. = FALSE)
             }

             # Warn well before the hard floor: a fold this thin still produces a
             # meaningless RMSE that is pooled with the others.
             if (length(train_keep) < WARN_TRAIN_POINTS) {
               warning(sprintf(
                 paste0("Fold %d: the buffer pruned %d of %d training points, ",
                        "leaving only %d. The accuracy metric for this fold is ",
                        "unlikely to be meaningful."),
                 i, n_pruned, length(cand_train_idx), length(train_keep)),
                 call. = FALSE)
             }

             list(train = train_keep, test = test_idx)
           })
         }
  )
}


#' @title Print Method for pai_assessment Objects
#' @description Provides a concise and formatted summary of the model assessment
#'   results.
#' @param x An object of class `pai_assessment`.
#' @param ... Additional arguments (not used).
#' @export
#' @examples
#' # Assuming `assessment_results` is an object from assess_pai_model()
#' # print(assessment_results)
#'
print.pai_assessment <- function(x, ...) {
  cat("--- PAI Model Assessment Results ---\n\n")
  cat("Model Method:      ", x$summary$Method, "\n")
  cat("Validation Type:   ", x$summary$ValidationType, "\n")

  # Identity-model (no correction) RMSE, pooled over the same points and by the
  # same estimator as the model RMSE below, so the two are comparable.
  ok <- is.finite(x$predictions$pred_dx) & is.finite(x$predictions$pred_dy)
  rmse <- sqrt(mean(x$predictions$true_dx[ok]^2 + x$predictions$true_dy[ok]^2))

  is_kfold <- x$summary$ValidationType %in%
    c("random", "spatial", "stratified", "spatial_block", "spatial_buffered")

  if (is_kfold) {
    k_realised <- length(x$details$n_test_per_fold)
    if (!is.null(k_realised) && !is.na(x$details$k_folds) &&
        k_realised != x$details$k_folds) {
      cat("Folds:              ", k_realised,
          sprintf("(requested %d; reduced to fit the occupied blocks)\n",
                  x$details$k_folds))
    } else {
      cat("Folds:             ", x$details$k_folds, "\n")
    }
  }

  if (x$summary$ValidationType == "probability") {
    cat("Train/Test Split:  ",
        paste0(x$details$train_split_ratio * 100, "% / ",
               (1 - x$details$train_split_ratio) * 100, "%\n"))
  } else if (x$summary$ValidationType == "stratified") {
    cat("Strata:            ", x$details$n_strata, "\n")
  } else if (x$summary$ValidationType == "spatial_buffered") {
    cat("Buffer Distance:   ", sprintf("%.2f", x$details$buffer_dist), "\n")
  }

  cat("\n--- Performance Metrics ---\n\n")

  metric_label <- if (is_kfold) {
    "Model CV 2D RMSE:  "
  } else {
    "Model holdout RMSE:"
  }

  cat(sprintf("Baseline 2D RMSE:   %.3f\n", rmse))
  cat(sprintf("%s %.3f\n", metric_label, x$summary$Mean_RMSE_2D))

  if (!is.na(x$summary$SD_RMSE_2D)) {
    cat(sprintf("Std Dev of RMSE:    %.3f (spread across folds, not a standard error)\n",
                x$summary$SD_RMSE_2D))
  }

  if (!is_kfold) {
    cat("\nNote: this is a single train/test split, not cross-validation.\n")
    cat("It has no replication and no dispersion estimate.\n")
  }

  n_used <- sum(ok)
  cat("\nBased on", n_used, "out-of-sample predictions")
  if (!is.null(x$details$n_nonfinite_predictions) &&
      x$details$n_nonfinite_predictions > 0) {
    cat(sprintf(" (%d non-finite prediction(s) excluded)",
                x$details$n_nonfinite_predictions))
  }
  cat(".\n")

  invisible(x)
}
