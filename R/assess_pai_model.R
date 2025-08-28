#' @title Assess PAI Model Performance
#' @description Performs model validation to estimate a PAI model's predictive
#'   performance using k-fold cross-validation or design-based probability
#'   sampling.
#' @details Model validation is crucial for understanding how well a model will
#' generalize to new data. This function automates this process.
#'
#' \strong{Validation Types}:
#'   \itemize{
#'     \item \strong{`random`}: Standard k-fold cross-validation.
#'     \item \strong{`spatial`}: Spatial k-fold cross-validation using k-means
#'      clustering.
#'     \item \strong{`probability`}: A single train/test split using simple
#'      random sampling.
#'     \item \strong{`stratified`}: A single train/test split using stratified
#'       random sampling based on the magnitude of distortion vectors.
#'   }
#'
#' @param gcp_data An `gcp` object of homologous points.
#' @param pai_method A character string or a custom model list for
#'  `train_pai_model`.
#' @param validation_type The validation strategy: "random", "spatial",
#'  "probability", or "stratified".
#' @param k_folds Number of folds for CV. Defaults to 5.
#' @param train_split_ratio Proportion of data for training (for "probability"
#'  and "stratified"). Defaults to 0.8.
#' @param n_strata Number of strata for stratified sampling. Defaults to 4.
#' @param seed An integer for reproducibility.
#' @param ... Additional arguments passed to `train_pai_model`.
#'
#' @return An object of class `pai_assessment` containing a summary data frame,
#'   detailed prediction results, and validation parameters.
#'
#' @importFrom stats sd complete.cases quantile kmeans
#' @export
#' @examples
#' \dontrun{
#' # --- 1. create a demo data set
#' demo_data <- create_demo_data(seed = 1)
#' gcp_data <-demo_data$gcp
#'
#' # --- 2. Assess with RANDOM k-fold CV ---
#' random_assessment <- cv_pai_model(
#'   gcp_data, pai_method = "lm", validation_type = "random", k_folds = 5
#' )
#' print(random_assessment)
#'
#' # --- 3. Assess with SPATIAL k-fold CV ---
#' spatial_assessment <- cv_pai_model(
#'   gcp_data, pai_method = "lm",
#'   validation_type = "spatial",
#'   k_folds = 5
#' )
#' print(spatial_assessment)
#'
#' # --- 4. Assess with PROBABILITY (simple random) sampling ---
#' prob_assessment <- cv_pai_model(
#'   gcp_data, pai_method = "lm",
#'   validation_type = "probability",
#'   train_split_ratio = 0.75
#' )
#' print(prob_assessment)
#'
#' # --- 5. Assess with STRATIFIED probability sampling ---
#' stratified_assessment <- cv_pai_model(
#'   gcp_data,
#'   pai_method = "lm",
#'   validation_type = "stratified",
#'   train_split_ratio = 0.75,
#'   n_strata = 4 # Use quartiles for stratification
#' )
#' print(stratified_assessment)
#' }
#'
cv_pai_model <- function(gcp_data, pai_method,
                         validation_type = "random",
                         k_folds = 5,
                         train_split_ratio = 0.8,
                         n_strata = 4,
                         seed = 123, ...) {

  set.seed(seed)

  # --- 1. Input Validation ---
  validate_assessment_inputs(gcp_data, pai_method, validation_type,
                             k_folds, train_split_ratio)

  # --- 2. Create Data Splits ---
  splits <- create_resampling_splits(gcp_data,
                                     validation_type,
                                     k_folds,
                                     train_split_ratio,
                                     n_strata)

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
                             method = pai_method,
                             seed = seed + i, ...)
    predictions <- predict(model, newdata = test_data)

    # Return a clean data frame with true and predicted values
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
  if (validation_type %in% c("random", "spatial", "stratified")) {
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
    Method = if (is.character(pai_method)) pai_method else pai_method$label,
    ValidationType = validation_type,
    Mean_RMSE_2D = mean_rmse,
    SD_RMSE_2D = sd_rmse
  )

  # --- Sanitize non-applicable parameters using a lookup list ---

  # 1. Define the rules for each validation type
  param_rules <- list(
    random      = c("k_folds"),
    spatial     = c("k_folds"),
    probability = c("train_split_ratio"),
    stratified  = c("n_strata")
  )

  # 2. Get the list of parameters that are valid for the current type
  valid_params <- param_rules[[validation_type]]

  # 3. If a parameter is NOT in the valid list, set it to NA
  if (!"k_folds" %in% valid_params) {
    k_folds <- NA_integer_
  }
  if (!"train_split_ratio" %in% valid_params) {
    train_split_ratio <- NA_real_
  }
  if (!"n_strata" %in% valid_params) {
    n_strata <- NA_integer_
  }

  details <- list(k_folds = k_folds,
                  train_split_ratio = train_split_ratio,
                  n_strata = n_strata)

  result <- list(
    summary = summary_df,
    predictions = all_predictions_df,
    details = details
  )
  class(result) <- "pai_assessment"

  message("Assessment complete.")
  return(result)
}

#' Internal helper to create resampling splits
#' @noRd
#' @importFrom stats sd complete.cases quantile kmeans
create_resampling_splits <- function(gcp_data, type, k, ratio, n_strata) {

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
           dist <- sqrt(gcp_data$dx^2 + gcp_data$dy^2)
           breaks <- stats::quantile(
             dist, probs = seq(0, 1, by = 1/n_strata),
             na.rm = TRUE, names = FALSE)
           breaks <- unique(breaks)

           if (length(breaks) < 2) {
             warning(
               "Could not create strata; falling back to simple random sampling.",
               call. = FALSE)
             return(
               create_resampling_splits(gcp_data, "probability",
                                        k, ratio, n_strata))
           }

           strata_ids <- cut(dist, breaks = breaks,
                             include.lowest = TRUE, labels = FALSE)

           lapply(1:n_strata, function(i) {
             list(train = which(strata_ids != i),
                  test = which(strata_ids == i))
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
  rmse <-sqrt(mean(x$predictions$true_dx^2 + x$predictions$true_dy^2))

  if (x$summary$ValidationType %in% c("random", "spatial")) {
    cat("Folds:             ", x$details$k_folds, "\n")
  } else if (x$summary$ValidationType == "probability") {
    cat("Train/Test Split:  ",
        paste0(x$details$train_split_ratio * 100, "% / ",
               (1 - x$details$train_split_ratio) * 100, "%\n"))
  } else {
    cat("Strata:            ", x$details$n_strata, "\n")
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
