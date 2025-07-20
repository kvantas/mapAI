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
#'     \item \strong{`spatial`}: Spatial k-fold cross-validation.
#'     \item \strong{`probability`}: Design-based validation using a single
#'       train/test split based on simple random sampling.
#'     \item \strong{`stratified`}: Design-based validation using stratified
#'       random sampling. A single train/test split is performed. Strata are
#'       created based on the quantiles of the Euclidean distance of the error
#'       vectors (`dx`, `dy`), ensuring the validation set represents all error
#'       magnitudes proportionally.
#'   }
#'
#' @param gcp_data An `sf` object of homologous points, from `read_gcps()`.
#' @param pai_method A character string specifying the algorithm to assess. One of:
#'   "lm", "gam", "rf", "helmert", "tps".
#' @param validation_type A character string specifying the validation
#'   strategy. One of "random", "spatial", "probability", or "stratified".
#' @param k_folds An integer for the number of folds in CV. Only used for
#'   `validation_type` "random" and "spatial". Defaults to 5.
#' @param train_split_ratio A numeric value between 0 and 1. The proportion of
#'   data for the training set. Used for "probability" and "stratified" types.
#'   Defaults to 0.8.
#' @param n_strata An integer specifying the number of strata to create for
#'   stratified sampling. Only used for `validation_type = "stratified"`.
#'   Defaults to 4 (quartiles).
#' @param seed An integer for setting the random seed for reproducibility.
#' @param ... Additional arguments passed to the `train_pai_model` function.
#'
#' @return A data frame summarizing the validation results.
#'
#' @import sf
#' @importFrom stats kmeans sd complete.cases quantile
#' @export
#' @examples
#' \dontrun{
#' # --- 1. create a demo data set
#' demo_files <- create_demo_data(seed = 42)
#' gcp_data <- read_gcps(gcp_path = demo_files$gcp_path)
#'
#' # --- 2. Assess with RANDOM k-fold CV ---
#' random_assessment <- assess_pai_model(
#'   gcp_data, pai_method = "rf", validation_type = "random", k_folds = 5
#' )
#' print(random_assessment)
#'
#' # --- 3. Assess with SPATIAL k-fold CV ---
#' spatial_assessment <- assess_pai_model(
#'   gcp_data, pai_method = "rf", validation_type = "spatial", k_folds = 5
#' )
#' print(spatial_assessment)
#'
#' # --- 4. Assess with PROBABILITY (simple random) sampling ---
#' prob_assessment <- assess_pai_model(
#'   gcp_data, pai_method = "rf", validation_type = "probability", train_split_ratio = 0.75
#' )
#' print(prob_assessment)
#'
#' # --- 5. Assess with STRATIFIED probability sampling ---
#' stratified_assessment <- assess_pai_model(
#'   gcp_data,
#'   pai_method = "rf",
#'   validation_type = "stratified",
#'   train_split_ratio = 0.75,
#'   n_strata = 4 # Use quartiles for stratification
#' )
#' print(stratified_assessment)
#' }
assess_pai_model <- function(gcp_data, pai_method, validation_type = "random",
                             k_folds = 5, train_split_ratio = 0.8, n_strata = 4,
                             seed = 123, ...) {

  # --- 1. Input Validation ---
  if (!inherits(gcp_data, "sf")) {
    stop("`gcp_data` must be a valid `sf` object.", call. = FALSE)
  }
  supported_methods <- c("lm", "gam", "rf", "helmert", "tps")
  if (!pai_method %in% supported_methods) {
    stop(paste0("Invalid `pai_method`. Please choose one of: '", paste(supported_methods, collapse = "', '"), "'."), call. = FALSE)
  }
  supported_validation <- c("random", "spatial", "probability", "stratified") # Added "stratified"
  if (!validation_type %in% supported_validation) {
    stop(paste0("Invalid `validation_type`. Please choose one of: '", paste(supported_validation, collapse = "', '"), "'."), call. = FALSE)
  }

  # --- Sanitize data ---
  gcp_df <- sf::st_drop_geometry(gcp_data)
  complete_rows <- stats::complete.cases(gcp_df[, c("source_x", "source_y", "dx", "dy")])
  if (any(!complete_rows)) {
    n_removed <- sum(!complete_rows)
    warning(paste(n_removed, "row(s) with missing values were removed before assessment."), call. = FALSE)
    gcp_data <- gcp_data[complete_rows, ]
  }
  n_pts <- nrow(gcp_data)

  if (validation_type %in% c("random", "spatial") && n_pts < k_folds) {
    stop("The number of complete GCPs is less than k_folds.", call. = FALSE)
  }

  set.seed(seed)

  # --- 2. Perform Validation based on type ---
  if (validation_type %in% c("random", "spatial")) {
    # --- K-FOLD CROSS-VALIDATION LOGIC ---
    message(paste("Starting", k_folds, "-fold", validation_type, "cross-validation..."))
    fold_ids <- if (validation_type == "spatial") {
      stats::kmeans(sf::st_coordinates(gcp_data), centers = k_folds)$cluster
    } else {
      sample(rep(1:k_folds, length.out = n_pts))
    }
    results_vector <- vector("numeric", length = k_folds)
    for (i in 1:k_folds) {
      message(paste("  Processing Fold", i, "of", k_folds, "..."))
      test_indices <- which(fold_ids == i)
      train_data <- gcp_data[-test_indices, ]
      test_data <- gcp_data[test_indices, ]
      temp_model <- train_pai_model(gcp_data = train_data, pai_method = pai_method, seed = seed + i, ...)
      predictions <- predict(temp_model, newdata = sf::st_drop_geometry(test_data))
      true_dx <- test_data[rownames(predictions), ]$dx
      true_dy <- test_data[rownames(predictions), ]$dy
      results_vector[i] <- sqrt(mean((true_dx - predictions$dx)^2 + (true_dy - predictions$dy)^2))
    }
    mean_rmse <- mean(results_vector, na.rm = TRUE)
    sd_rmse <- stats::sd(results_vector, na.rm = TRUE)

  } else {
    # --- PROBABILITY SAMPLING (SINGLE SPLIT) LOGIC ---
    train_indices <- c()
    if (validation_type == "probability") {
      message(paste0("Starting design-based validation with simple random sampling..."))
      train_indices <- sample.int(n = n_pts, size = floor(train_split_ratio * n_pts))
    } else if (validation_type == "stratified") {
      message(paste0("Starting design-based validation with stratified random sampling..."))

      # Calculate Euclidean distance for stratification
      gcp_data$dist <- sqrt(gcp_data$dx^2 + gcp_data$dy^2)

      # Create strata using quantiles
      breaks <- stats::quantile(gcp_data$dist, probs = seq(0, 1, by = 1/n_strata), na.rm = TRUE)
      # Ensure breaks are unique, which can be an issue with skewed data
      breaks <- unique(breaks)
      if(length(breaks) < 2) {
        warning("Could not create strata from Euclidean distance; too few unique values. Falling back to simple random sampling.", call. = FALSE)
        train_indices <- sample.int(n = n_pts, size = floor(train_split_ratio * n_pts))
      } else {
        gcp_data$strata <- cut(gcp_data$dist, breaks = breaks, include.lowest = TRUE, labels = FALSE)

        # Handle potential NAs from cut if a value is outside the range (unlikely with quantiles)
        gcp_data$strata[is.na(gcp_data$strata)] <- 1

        # Perform stratified sampling for the training set
        indices_by_stratum <- split(seq_len(n_pts), gcp_data$strata)
        train_indices <- unlist(lapply(indices_by_stratum, function(indices) {
          n_train_stratum <- floor(train_split_ratio * length(indices))
          if (length(indices) > 1 && n_train_stratum > 0) {
            sample(indices, size = n_train_stratum)
          } else {
            # If stratum is too small, assign all to training
            indices
          }
        }))
      }
    }

    test_indices <- setdiff(seq_len(n_pts), train_indices)

    # Check if split resulted in empty sets
    if(length(train_indices) == 0 || length(test_indices) == 0) {
      stop("The train/test split resulted in an empty set. Adjust train_split_ratio or check data.", call. = FALSE)
    }

    train_data <- gcp_data[train_indices, ]
    test_data <- gcp_data[test_indices, ]

    message(paste("Training model on", nrow(train_data), "points, validating on", nrow(test_data), "points."))

    final_model <- train_pai_model(gcp_data = train_data, pai_method = pai_method, seed = seed, ...)
    predictions <- predict(final_model, newdata = sf::st_drop_geometry(test_data))

    true_dx <- test_data[rownames(predictions), ]$dx
    true_dy <- test_data[rownames(predictions), ]$dy

    mean_rmse <- sqrt(mean((true_dx - predictions$dx)^2 + (true_dy - predictions$dy)^2))
    sd_rmse <- NA_real_
  }

  # --- Summarize and Return Results ---
  summary_df <- data.frame(
    Method = pai_method,
    ValidationType = validation_type,
    Mean_RMSE_2D = mean_rmse,
    SD_RMSE_2D = sd_rmse
  )
  message("Assessment complete.")
  return(summary_df)
}
