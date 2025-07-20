#' @title Train a PAI Model
#' @description Trains a supervised learning or analytical model to define a
#'   spatial transformation.
#' @details This function serves as a factory for creating transformation
#'   models. It supports machine learning methods ("lm", "gam", "rf", "svmRadial", "svmLinear") that learn
#'   the relationship between source coordinates and displacement vectors, as
#'   well as the analytical "helmert" method which solves for a global
#'   similarity transformation.
#'
#'   **Important**: The more flexible machine learning models, `gam` and `rf`,
#'   require a sufficient number of data points to produce stable and reliable
#'   results. This function will prevent training these models with fewer than
#'   60 homologous points to avoid overfitting. If you have a small number of
#'   points, please use the more robust "lm" or "helmert" methods.
#'
#' @param gcp_data An `sf` object of homologous points from `read_gcps()`.
#' @param pai_method A character string specifying the algorithm. One of:
#'    "lm","tps", "gam", "rf", "svmRadial", "svmLinear", or "helmert".
#' @param seed An integer for setting the random seed for reproducibility.
#' @param ... Additional arguments passed to the underlying model fitting
#'    functions (`mgcv::gam`, `stats::lm`, `ranger::ranger`, `fields::Tps`, `e1071::svm`).
#'
#' @return A trained model object of class `pai_model`.
#'
#' @import mgcv
#' @import ranger
#' @importFrom fields Tps
#' @importFrom e1071 svm tune
#' @import dplyr
#' @importFrom stats lm as.formula
#' @export
train_pai_model <- function(gcp_data, pai_method, seed = 123, ...) {

  set.seed(seed)

  # Ensure the input is valid
  if (!inherits(gcp_data, "sf")) {
    stop("`gcp_data` must be an sf object created by `read_gcps()`.",
         call. = FALSE)
  }

  # Check if the pai_method is valid
  allowed_methods <- c("lm", "gam", "rf", "tps", "helmert", "svmRadial", "svmLinear")
  if (!pai_method %in% allowed_methods) {
    stop(
      "Invalid 'pai_method'. Please choose one of: ",
      paste(shQuote(allowed_methods), collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  df <- sf::st_drop_geometry(gcp_data)

  # --- 2. Data Requirement Guardrail ---
  # Check if a complex model is requested with insufficient data
  n_points <- nrow(gcp_data)
  if (pai_method %in% c("gam", "rf") && n_points < 60) {
    error_message <- sprintf(
      "Method '%s' requires at least 60 data points for stable results,
      but you provided %d. Please use simpler models like 'lm' or 'helmert'
      for small datasets.",
      pai_method, n_points
    )
    stop(error_message, call. = FALSE)
  }

  # --- Handle Helmert Transformation (calls the internal function) ---
  if (pai_method == "helmert") {
    message("Fitting Helmert transformation...")
    model_fit <- helmert(
      source_x = df$source_x,
      source_y = df$source_y,
      target_x = df$target_x,
      target_y = df$target_y
    )
    # The output from helmert() is already structured correctly
  } else if (pai_method == "tps") {
    message("Fitting Thin Plate Spline model...")
    source_coords <- as.matrix(df[, c("source_x", "source_y")])
    # Train separate models for dx and dy, similar to lm and rf
    model_fit <- list(
      model_dx = fields::Tps(x = source_coords, Y = df$dx, ...),
      model_dy = fields::Tps(x = source_coords, Y = df$dy, ...)
    )
  } else {
    # --- Handle Machine Learning Models ---
    df_ml <- dplyr::select(df, "source_x", "source_y", "dx", "dy")
    message("Training '", pai_method, "' model...")

    if (pai_method == "gam") {
      formula_list <- list(dx ~ s(source_x, source_y), dy ~ s(source_x, source_y))
      model_fit <- mgcv::gam(formula_list, data = df_ml, family = mgcv::mvn(d = 2), ...)
    } else if (pai_method %in% c("svmRadial", "svmLinear")) {
        train_svm <- function(data, outcome_var) {
        formula <- as.formula(paste(outcome_var, "~ source_x + source_y"))
        kernel_type <- if (pai_method == "svmRadial") "radial" else "linear"

        # SVM tuning can be computationally expensive, so we use a small subset of ranges
        # Users can pass more extensive tune_control for more thorough tuning
        tune_control <- e1071::tune.control(sampling = "cross", cross = 5)
        
        ranges <- if (kernel_type == "radial") {
          list(gamma = 2^(-1:1), cost = 2^(2:4))
        } else {
          list(cost = 2^(2:4))
        }

        tuned <- e1071::tune(
          e1071::svm,
          formula,
          data = data,
          kernel = kernel_type,
          ranges = ranges,
          tunecontrol = tune_control,
          ...
        )
        return(tuned$best.model)
      }
      model_fit <- list(
        model_dx = train_svm(df_ml, "dx"),
        model_dy = train_svm(df_ml, "dy")
      )
    }
    else {
      train_model <- function(data, outcome_var) {
        formula <- as.formula(paste(outcome_var, "~ source_x + source_y"))
        if (pai_method == "lm") {
          return(stats::lm(formula, data = data, ...))
        } else if (pai_method == "rf") {
          return(ranger::ranger(formula, data = data, ...))
        }
      }
      model_fit <- list(
        model_dx = train_model(df_ml, "dx"),
        model_dy = train_model(df_ml, "dy")
      )
    }
  }

  # --- Return final model object with a consistent class ---
  output <- list(model = model_fit, method = pai_method)
  class(output) <- "pai_model"
  return(output)
}
