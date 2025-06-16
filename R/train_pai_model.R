#' @title Train a Spatial Correction Model using Tidymodels
#' @description Trains a supervised learning model to predict spatial displacements, using
#'   the `tidymodels` framework for a modern, modular approach.
#' @details This function implements a curated set of modeling methods.
#'   It uses the `tidymodels` ecosystem for preprocessing, model specification,
#'   hyperparameter tuning, and fitting. The special case for `mgcv::gam` is retained
#'   for its unique bivariate modeling capabilities. For other models, two separate
#'   univariate models are trained for the `dx` and `dy` displacement vectors.
#'
#' @param gcp_data An `sf` object of homologous points from `read_gcps()`.
#' @param method A character string specifying the algorithm. One of:
#'   "lm", "gam", "rf", "svm_radial", "svm_linear".
#' @param seed An integer for setting the random seed for reproducibility.
#'
#' @return A trained model object of class `pai_model`. For `tidymodels` methods,
#'   this contains two fitted `workflow` objects (`model_dx`, `model_dy`).
#'
#' @import mgcv
#' @importFrom recipes recipe step_normalize all_predictors
#' @importFrom parsnip linear_reg rand_forest svm_rbf svm_linear set_engine set_mode
#' @importFrom workflows workflow add_recipe add_model
#' @importFrom tune tune tune_grid select_best finalize_workflow
#' @importFrom dials parameters
#' @importFrom rsample vfold_cv
#' @importFrom dplyr select
#' @importFrom stats as.formula
#' @importFrom parsnip fit
#' @export
train_pai_model <- function(gcp_data, method, seed = 123) {
  set.seed(seed)
  df <- sf::st_drop_geometry(gcp_data) %>%
    dplyr::select(source_x, source_y, dx, dy) # Ensure only necessary columns

  # --- Handle special case: bivariate GAM ---
  if (method == "gam") {
    model_fit <- mgcv::gam(list(dx, dy) ~ s(source_x, source_y, bs = "tp"),
                           data = df, family = "mvn"(d=2))

    output <- list(model = model_fit, method = method)
    class(output) <- "pai_model"
    return(output)
  }

  # --- Internal helper function for training a single tidymodels workflow ---
  train_single_outcome <- function(data, outcome_var, model_spec, tune_grid) {
    rec_formula <- stats::as.formula(paste(outcome_var, "~ source_x + source_y"))
    rec <- recipes::recipe(rec_formula, data = data) %>%
      recipes::step_normalize(recipes::all_predictors())

    wf <- workflows::workflow() %>%
      workflows::add_recipe(rec) %>%
      workflows::add_model(model_spec)

    cv_folds <- rsample::vfold_cv(data, v = 10, strata = {{outcome_var}})

    tune_res <- tune::tune_grid(
      wf,
      resamples = cv_folds,
      grid = tune_grid,
      metrics = yardstick::metric_set(yardstick::rmse)
    )

    best_params <- tune::select_best(tune_res, metric = "rmse")
    final_wf <- tune::finalize_workflow(wf, best_params)
    final_fit <- parsnip::fit(final_wf, data = data)

    return(final_fit)
  }

  # --- Define model specifications and grids for tidymodels ---
  model_info <- switch(
    method,
    "lm" = list(
      spec = parsnip::linear_reg() %>% parsnip::set_engine("lm"),
      grid = tibble::tibble(.rows = 1) # No tuning params for lm
    ),
    "rf" = list(
      spec = parsnip::rand_forest(mtry = tune::tune()) %>%
        parsnip::set_engine("ranger") %>% parsnip::set_mode("regression"),
      grid = expand.grid(mtry = c(1, 2))
    ),
    "svm_radial" = list(
      spec = parsnip::svm_rbf(cost = tune::tune(), rbf_sigma = tune::tune()) %>%
        parsnip::set_engine("kernlab") %>% parsnip::set_mode("regression"),
      grid = expand.grid(cost = c(1, 10, 100, 1000), rbf_sigma = c(0.001, 0.01, 0.1, 1, 10, 100, 1000))
    ),
    "svm_linear" = list(
      spec = parsnip::svm_linear(cost = tune::tune()) %>%
        parsnip::set_engine("kernlab") %>% parsnip::set_mode("regression"),
      grid = expand.grid(cost = c(0.001, 0.01, 0.1, 1, 10, 100, 1000))
    ),
    stop("Invalid method. Choose from 'lm', 'gam', 'rf', 'svm_radial', 'svm_linear'.")
  )

  # --- Train models for dx and dy ---
  message(paste("Training models for dx using tidymodels and method:", method, "..."))
  model_dx <- train_single_outcome(df, "dx", model_info$spec, model_info$grid)

  message(paste("Training models for dy using tidymodels and method:", method, "..."))
  model_dy <- train_single_outcome(df, "dy", model_info$spec, model_info$grid)

  # --- Return final model object ---
  output <- list(
    model = list(model_dx = model_dx, model_dy = model_dy),
    method = method
  )
  class(output) <- "pai_model"
  return(output)
}
