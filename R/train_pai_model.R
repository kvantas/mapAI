#' @title Train a PAI Model
#' @description Trains a supervised learning model to predict spatial
#' displacements using random forests, linear regression, or bivariate GAMs.
#' @param gcp_data An `sf` object of homologous points from `read_gcps()`.
#' @param method A character string specifying the algorithm. One of:
#'    "lm", "gam", "rf".
#' @param seed An integer for setting the random seed for reproducibility.
#' @param ... Additional arguments passed to the underlying model fitting
#'    functions (`mgcv::gam`, `stats::lm`, `ranger::ranger`).
#'
#' @return A trained model object of class `pai_model`.
#'
#' @import mgcv
#' @import ranger
#' @import dplyr
#' @importFrom stats lm as.formula
#' @export
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
