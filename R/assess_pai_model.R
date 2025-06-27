#' @title Assess PAI Model Performance Using Cross-Validation
#' @description Performs k-fold cross-validation to provide a robust estimate of
#'   a PAI model's predictive performance, offering both random and spatial CV
#'   methods.
#' @details Model validation is crucial for understanding how well a model will
#' generalize to new data. This function automates this process.
#'
#' \strong{Validation Types}:
#'   \itemize{
#'     \item \strong{`random` (default)}: Standard k-fold cross-validation. Data
#'      is randomly partitioned into folds. This can produce overly optimistic
#'      results for spatial data due to spatial autocorrelation.
#'     \item \strong{`spatial`}: Spatial Cross-Validation (SCV). Homologous
#'     points are clustered into `k_folds` spatially distinct groups using
#'     k-means clustering on their coordinates. The model is then trained on k-1
#'     groups and tested on the held-out group.
#'   }
#' The function loops through the folds, trains a temporary model on the
#' training data for each fold, predicts on the test data, and calculates the 2D
#' Root Mean Squared Error (RMSE). The final output is the mean and standard
#' deviation of the RMSEs across all folds.
#'
#' @param gcp_data An `sf` object of homologous points, from `read_gcps()`.
#' @param method A character string specifying the algorithm to assess. One of:
#'   "lm", "gam", "rf", "helmert", "tps".
#' @param validation_type A character string specifying the cross-validation
#'   strategy. One of "random" (default) or "spatial".
#' @param k_folds An integer specifying the number of folds for
#'   cross-validation. Defaults to 10 for "random" and 5 for "spatial".
#' @param seed An integer for setting the random seed for reproducibility.
#' @param ... Additional arguments passed to the underlying `train_pai_model`
#'   function (e.g., `num.threads` for `ranger`).
#'
#' @return A data frame summarizing the cross-validation results, containing:
#'   \item{Method}{The algorithm that was assessed.}
#'   \item{ValidationType}{The CV strategy used.}
#'   \item{Mean_RMSE_2D}{The average 2D RMSE across all k-folds.}
#'   \item{SD_RMSE_2D}{The standard deviation of the 2D RMSE across all k-folds.}
#'
#' @import sf
#' @importFrom stats kmeans sd
#' @export
#' @examples
#' \dontrun{
#' # --- 1. Generate and read demo data ---
#' demo_files <- create_demo_data(seed = 42)
#' gcp_data <- read_gcps(gcp_path = demo_files$gcp_path, crs = 3857)
#'
#' # --- 2. Assess a Random Forest model with RANDOM CV ---
#' random_assessment <- assess_pai_model(
#'   gcp_data,
#'   method = "rf",
#'   validation_type = "random",
#'   k_folds = 10
#' )
#' print(random_assessment)
#'
#' # --- 3. Assess the SAME model with SPATIAL CV ---
#' spatial_assessment <- assess_pai_model(
#'   gcp_data,
#'   method = "rf",
#'   validation_type = "spatial",
#'   k_folds = 5
#' )
#' print(spatial_assessment)
#' }
assess_pai_model <- function(gcp_data, method, validation_type = "random",
                             k_folds = 5, seed = 123, ...) {

  # --- 1. Input Validation ---
  if (!inherits(gcp_data, "sf")) {
    stop("`gcp_data` must be a valid `sf` object.", call. = FALSE)
  }
  supported_methods <- c("lm", "gam", "rf", "helmert", "tps")
  if (!method %in% supported_methods) {
    stop(paste0("Invalid `method`. Please choose one of: '", paste(supported_methods, collapse = "', '"), "'."), call. = FALSE)
  }
  supported_validation <- c("random", "spatial")
  if (!validation_type %in% supported_validation) {
    stop(paste0("Invalid `validation_type`. Please choose one of: '", paste(supported_validation, collapse = "', '"), "'."), call. = FALSE)
  }
  if (is.null(k_folds)) {
    k_folds <- ifelse(validation_type == "spatial", 5, 10)
  }

  # --- Sanitize data once at the beginning ---

  gcp_df <- sf::st_drop_geometry(gcp_data)
  complete_rows <- stats::complete.cases(gcp_df[, c("source_x", "source_y", "dx", "dy")])
  if (any(!complete_rows)) {
    n_removed <- sum(!complete_rows)
    warning(paste(n_removed, "row(s) with missing values were removed before assessment."), call. = FALSE)
    gcp_data <- gcp_data[complete_rows, ]
  }
  if (nrow(gcp_data) < k_folds) {
    stop("The number of complete GCPs is less than k_folds. Please use a smaller k_folds value.", call.=FALSE)
  }

  set.seed(seed)
  n_pts <- nrow(gcp_data)
  message(paste("Starting", k_folds, "-fold", validation_type, "cross-validation for method:", method, "on", n_pts, "complete points."))

  # --- 2. Create Folds ---
  fold_ids <- if (validation_type == "spatial") {
    message("Creating spatial folds...")
    coords <- sf::st_coordinates(gcp_data)
    stats::kmeans(coords, centers = k_folds)$cluster
  } else {
    message("Creating random folds...")
    sample(rep(1:k_folds, length.out = n_pts))
  }

  # --- 3. Loop through folds ---
  results_vector <- vector("numeric", length = k_folds)

  for (i in 1:k_folds) {
    message(paste("  Processing Fold", i, "of", k_folds, "..."))
    test_indices <- which(fold_ids == i)
    train_indices <- which(fold_ids != i)
    train_data <- gcp_data[train_indices, ]
    test_data <- gcp_data[test_indices, ]

    temp_model <- train_pai_model(gcp_data = train_data, method = method, seed = seed + i, ...)
    #temp_model <- train_pai_model(gcp_data = train_data, method = method, seed = seed + i)

    # Predict on the test data
    predictions <- predict(temp_model, newdata = sf::st_drop_geometry(test_data))

    # Align the true values with the predictions using row names.
    # This is robust to any internal na.action in predict methods.
    # Note: test_data must have its original row names preserved.
    # Subsetting with `[` preserves row names.
    prediction_rows <- rownames(predictions)
    true_dx <- test_data[prediction_rows, ]$dx
    true_dy <- test_data[prediction_rows, ]$dy

    fold_rmse <- sqrt(mean((true_dx - predictions$dx)^2 + (true_dy - predictions$dy)^2))
    results_vector[i] <- fold_rmse
  }

  # --- 4. Summarize and Return Results ---
  summary_df <- data.frame(
    Method = method,
    ValidationType = validation_type,
    Mean_RMSE_2D = mean(results_vector, na.rm = TRUE),
    SD_RMSE_2D = stats::sd(results_vector, na.rm = TRUE)
  )
  message("Assessment complete.")
  return(summary_df)
}
