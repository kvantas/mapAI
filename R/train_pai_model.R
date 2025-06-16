#' @title Train a Spatial Correction Model
#' @description Trains a supervised learning model to predict spatial displacements using
#'   the validated methods and hyperparameter grids.
#' @param pai_data An `pai_data` object from `read_data()`.
#' @param method A character string specifying the algorithm. One of:
#'   "lm", "gam", "rf", "svm_radial", "svm_linear".
#' @param seed An integer for setting the random seed for reproducibility.
#' @return A trained model object of class `mapAI_model`.
#' @import caret
#' @import mgcv
#' @import ranger
#' @import kernlab
#' @import nnet
#' @export
train_pai_model <- function(pai_data, method, seed = 123) {
  set.seed(seed)

  # Create a data frame for training models
  df <- sf::st_drop_geometry(pai_data$gcp_data)
  formula_dx <- dx ~ source_x + source_y
  formula_dy <- dy ~ source_x + source_y

  model_fit <- switch(
    method,
    "lm" = {
      # return an affine  PAI model with separate models for dx and dy
      list(model_dx = lm(formula_dx, data = df), model_dy = lm(formula_dy, data = df))
    },
    "gam" = {
      # return a single GAM model
      list(model = mgcv::gam(list(dx ~ s(source_x, source_y, bs = "tp"),
                                  dy ~ s(source_x, source_y, bs = "tp")),
                             family = mgcv::mvn(d = 2),
                             data = df))
    },
    "rf" = , "svm_radial" = , "svm_linear" = {
      train_control <- caret::trainControl(method = "cv", number = 10, verboseIter = FALSE)
      preprocess_steps <- c("center", "scale")
      tune_grids <- list(
        rf = expand.grid(mtry = c(1, 2), splitrule = c("variance"), min.node.size = c(5)),
        svm_radial = expand.grid(sigma = c(0.01, 0.1, 1, 10, 100), C = c(10, 100, 1000)),
        svmlinear = expand.grid(C = c(0.01, 0.1, 1, 10, 100))
      )
      caret_method <- switch(method, rf = "ranger", svm_radial = "svmRadial", svmlinear = "svmLinear")

      message(paste("Training model for dx using", method, "..."))
      model_dx <- caret::train(formula_dx, data = df, method = caret_method, trControl = train_control,
                               tuneGrid = tune_grids[[method]], preProcess = preprocess_steps,
                               trace = FALSE)
      message(paste("Training model for dy using", method, "..."))
      model_dy <- caret::train(formula_dy, data = df, method = caret_method, trControl = train_control,
                               tuneGrid = tune_grids[[method]], preProcess = preprocess_steps,
                               trace = FALSE)
      list(model_dx = model_dx, model_dy = model_dy)
    },
    stop("Invalid method. Choose from 'lm', 'gam', 'rf', 'svm_radial', 'svm_linear'.")
  )
  output <- list(model = model_fit, method = method)
  class(output) <- "mapAI_model"
  return(output)
}
