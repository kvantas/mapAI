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
#'       partitioning space into independent rectangular spatial tiles.
#'     \item \strong{`spatial_buffered`}: Buffered spatial cross-validation. Partitions
#'       space into blocks and enforces a spatial exclusion dead-zone buffer \eqn{d_{\text{buffer}}}:
#'       training observations satisfying \eqn{\min \|\mathbf{s}_{\text{train}} - \mathbf{s}_{\text{test}}\| \le d_{\text{buffer}}}
#'       are omitted from the training set for each fold to eliminate spatial autocorrelation data leakage (Roberts et al., 2017).
#'     \item \strong{`probability`}: Design-based single train/test split using simple random sampling.
#'     \item \strong{`stratified`}: Stratified k-fold cross-validation binned by displacement vector magnitude.
#'   }
#'
#' \strong{Performance Metrics Formulation}:
#' Out-of-sample predictive accuracy is evaluated using 2D Root Mean Square Error:
#' \deqn{\operatorname{RMSE}_{2D} = \sqrt{\frac{1}{n_{\text{val}}} \sum_{i=1}^{n_{\text{val}}} \left( (dx_i - \widehat{dx}_i)^2 + (dy_i - \widehat{dy}_i)^2 \right)}}
#'
#' @references
#' \itemize{
#'   \item Roberts et al. (2017). Cross-validation strategies for data with spatial,
#'     temporal, or phylogenetic dependence. \emph{Ecography}, 40(8), 913-929.
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
#' @param buffer_dist Numeric buffer distance for `"spatial_buffered"`. If `NULL`
#'   (default), an automatic distance based on spatial extent is applied.
#' @param seed An integer for reproducibility.
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
                                     buffer_dist)

  # --- 3. Run Validation Across All Splits ---
  message(paste("Starting", validation_type, "validation..."))
  all_predictions <- lapply(seq_along(splits), function(i) {
    split <- splits[[i]]
    train_data <- gcp_data[split$train, ]
    test_data <- gcp_data[split$test, ]

    if (length(splits) > 1) {
      message(paste("  Processing Fold", i, "of", length(splits), "..."))
    }
    model <- train_pai_model(gcp_data = train_data,
                             method = method,
                             seed = seed + i, ...)
    predictions <- predict(model, newdata = test_data)

    # Return clean data frame with true and predicted values
    data.frame(
      fold = i,
      true_dx = test_data$dx,
      true_dy = test_data$dy,
      pred_dx = predictions$dx,
      pred_dy = predictions$dy
    )
  })
  all_predictions_df <- do.call(rbind, all_predictions)

  # --- 4. Calculate Final Metrics ---
  if (validation_type %in% c("random", "spatial", "stratified", "spatial_block", "spatial_buffered")) {
    # For CV, calculate RMSE for each fold, then summarize
    fold_rmse <- vapply(
      split(all_predictions_df, all_predictions_df$fold), function(df) {
        sqrt(mean((df$true_dx - df$pred_dx)^2 + (df$true_dy - df$pred_dy)^2))
      }, numeric(1))

    mean_rmse <- mean(fold_rmse, na.rm = TRUE)
    sd_rmse <- stats::sd(fold_rmse, na.rm = TRUE)
  } else {
    # For single splits, calculate one overall RMSE
    mean_rmse <- sqrt(
      mean((all_predictions_df$true_dx - all_predictions_df$pred_dx)^2 +
             (all_predictions_df$true_dy - all_predictions_df$pred_dy)^2))
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
                  splits = splits)

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

#' Internal helper to create resampling splits
#' @noRd
#' @importFrom stats sd complete.cases quantile kmeans
create_resampling_splits <- function(gcp_data, type, k, ratio, n_strata, seed, buffer_dist = NULL) {

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
           rx <- range(gcp_data$source_x)
           ry <- range(gcp_data$source_y)
           span_x <- max(1e-6, diff(rx))
           span_y <- max(1e-6, diff(ry))

           nx <- ceiling(sqrt(k))
           ny <- ceiling(k / nx)

           eps <- 1e-9 * max(1, span_x, span_y)
           col_idx <- pmin(floor((gcp_data$source_x - rx[1]) / (span_x + eps) * nx) + 1, nx)
           row_idx <- pmin(floor((gcp_data$source_y - ry[1]) / (span_y + eps) * ny) + 1, ny)
           block_id <- (row_idx - 1) * nx + col_idx

           unique_blocks <- unique(block_id)
           block_fold_map <- stats::setNames(
             sample(rep(1:k, length.out = length(unique_blocks))),
             as.character(unique_blocks)
           )
           fold_ids <- block_fold_map[as.character(block_id)]

           lapply(1:k, function(i) {
             list(
               train = which(fold_ids != i),
               test = which(fold_ids == i)
             )
           })
         },
         "spatial_buffered" = {
           rx <- range(gcp_data$source_x)
           ry <- range(gcp_data$source_y)
           span_x <- max(1e-6, diff(rx))
           span_y <- max(1e-6, diff(ry))

           nx <- ceiling(sqrt(k))
           ny <- ceiling(k / nx)

           eps <- 1e-9 * max(1, span_x, span_y)
           col_idx <- pmin(floor((gcp_data$source_x - rx[1]) / (span_x + eps) * nx) + 1, nx)
           row_idx <- pmin(floor((gcp_data$source_y - ry[1]) / (span_y + eps) * ny) + 1, ny)
           block_id <- (row_idx - 1) * nx + col_idx

           unique_blocks <- unique(block_id)
           block_fold_map <- stats::setNames(
             sample(rep(1:k, length.out = length(unique_blocks))),
             as.character(unique_blocks)
           )
           fold_ids <- block_fold_map[as.character(block_id)]

           if (is.null(buffer_dist) || is.na(buffer_dist) || buffer_dist <= 0) {
             buffer_dist <- 0.05 * max(span_x, span_y)
           }

           lapply(1:k, function(i) {
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

             min_pts_needed <- min(3, length(cand_train_idx))
             if (length(train_keep) < min_pts_needed) {
               ord <- order(min_dist_to_test, decreasing = TRUE)
               train_keep <- cand_train_idx[ord[seq_len(min_pts_needed)]]
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

  # compute identity model RMSE for comparison
  rmse <- sqrt(mean(x$predictions$true_dx^2 + x$predictions$true_dy^2))

  if (x$summary$ValidationType %in% c("random", "spatial", "stratified", "spatial_block", "spatial_buffered")) {
    cat("Folds:             ", x$details$k_folds, "\n")
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
  cat(sprintf("Baseline 2D RMSE:   %.3f\n", rmse))
  cat(sprintf("Model CV 2D RMSE:   %.3f\n", x$summary$Mean_RMSE_2D))

  if (!is.na(x$summary$SD_RMSE_2D)) {
    cat(sprintf("Std Dev of RMSE:    %.3f (across folds)\n",
                x$summary$SD_RMSE_2D))
  }

  cat("\nBased on", nrow(x$predictions), "total predictions.\n")

  invisible(x)
}
