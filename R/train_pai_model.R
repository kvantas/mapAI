#' @title Train a Positional Accuracy Improvement (PAI) Model
#' @description Trains a supervised learning model to predict spatial displacements
#'   using one of three methods: linear models, random forests, or bivariate GAMs.
#'
#' @details
#' This function is the core of the modeling workflow. It takes a set of Ground
#' Control Points (GCPs) and learns the mathematical relationship between the
#' distorted `source_x` and `source_y` coordinates and the `dx`/`dy` correction
#' vectors.
#'
#' The user can choose from three distinct modeling approaches:
#' \itemize{
#'   \item \strong{`"lm"` (Linear Model):} The fastest and simplest method. It assumes
#'     a global, linear distortion (like a simple skew or affine transformation).
#'     Best used as a baseline or for very simple distortions.
#'   \item \strong{`"rf"` (Random Forest):} A powerful machine learning model that can
#'     capture highly complex, non-linear relationships. It is robust and effective
#'     but less interpretable than other models. Implemented via the `ranger` package.
#'   \item \strong{`"gam"` (Generalized Additive Model):} A flexible model that balances
#'     predictive power with interpretability. It fits smooth, non-linear surfaces
#'     to the data and is ideal for distortions that vary smoothly across the map.
#'     This method uses a special bivariate `mgcv::gam` model to handle `dx` and
#'     `dy` simultaneously, which can improve accuracy.
#' }
#'
#' The `...` argument allows advanced users to pass parameters directly to the
#' underlying model-fitting functions (`stats::lm`, `ranger::ranger`, or `mgcv::gam`),
#' enabling fine-tuning of the model.
#'
#' @param gcp_data An `sf` object of homologous points, typically from `read_gcps()`.
#' @param method A character string specifying the algorithm. One of:
#'   `"lm"`, `"gam"`, or `"rf"`.
#' @param seed An integer for setting the random seed for reproducibility.
#' @param ... Additional arguments passed to the underlying model fitting functions
#'   (e.g., `importance = "permutation"` for `ranger`, or `weights` for `lm`).
#'
#' @return A trained model object of class `pai_model`. This is a list containing
#'   the fitted model(s) and the name of the method used.
#'
#' @import mgcv
#' @import ranger
#' @import dplyr
#' @importFrom stats lm as.formula
#' @export
#' @examples
#' \dontrun{
#' # --- 1. Load Data ---
#' data(gcps) # Load the package's built-in homologous points
#'
#' # --- 2. Train a Simple Linear Model ---
#' # This serves as a good baseline.
#' pai_model_lm <- train_pai_model(gcps, method = "lm")
#'
#' # Inspect the returned object
#' print(pai_model_lm$method)
#' summary(pai_model_lm$model$model_dx) # Summary of the model for dx
#'
#' # --- 3. Train a more complex Random Forest Model ---
#' # We can pass advanced arguments via the `...` parameter.
#' # For example, let's ask ranger to calculate variable importance.
#' pai_model_rf <- train_pai_model(gcps, method = "rf",
#'                                 importance = "permutation")
#'
#' # The model object now contains importance scores
#' print(pai_model_rf$model$model_dx$importance)
#'
#' # --- 4. Train a GAM for smooth, non-linear correction ---
#' pai_model_gam <- train_pai_model(gcps, method = "gam")
#'
#' # The GAM model is a single object handling both dx and dy
#' summary(pai_model_gam$model)
#' }
train_pai_model <- function(gcp_data, method, seed = 123, ...) {
  set.seed(seed)
  df <- sf::st_drop_geometry(gcp_data) %>%
    dplyr::select("source_x", "source_y", "dx", "dy")

  # --- Handle special case: bivariate GAM ---
  if (method == "gam") {

    formula_dx <- as.formula("dx ~ s(source_x, source_y)")
    formula_dy <- as.formula("dy ~ s(source_x, source_y)")

    model_fit <- mgcv::gam(
      list(formula_dx, formula_dy),
      data = df,
      family = mgcv::mvn(d = 2),
      method = "REML",
      ...
    )

    output <- list(model = model_fit, method = method)
    class(output) <- "pai_model"
    return(output)
  }

  # --- Train models for dx and dy ---
  formula <- as.formula("~ source_x + source_y")

  train_model <- function(data, outcome_var) {
    formula_full <- as.formula(paste(outcome_var, "~ source_x + source_y"))

    if (method == "lm") {
      return(do.call(stats::lm, list(formula = formula_full, data = data, ...)))
    } else if (method == "rf") {
      return(do.call(ranger::ranger, list(
        formula = formula_full,
        data = data,
        num.trees = 500,
        mtry = 1,
        ...
      )))
    }
  }
  # train different models for dx and dy
  model_dx <- train_model(df, "dx")
  model_dy <- train_model(df, "dy")

  # --- Return final model object ---
  output <- list(
    model = list(model_dx = model_dx,
                 model_dy = model_dy),
    method = method
  )
  class(output) <- "pai_model"
  return(output)
}
