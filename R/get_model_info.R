#' Retrieve Model Information from the pai_model_list
#' @keywords internal
get_model_info <- function(method) {
  if (is.character(method) && length(method) == 1) {
    # Path 1: User provided a string
    model_info <- pai_model_list[[method]]
    if (is.null(model_info)) {
      stop(
        paste("Built-in model '",
              method,
              "' not found. Available methods are: ",
              paste(names(pai_model_list), collapse = ", ")),
        call. = FALSE)
    }
  } else if (is.list(method)) {
    # Path 2: User provided a custom list
    message("Using custom model definition provided as a list.")
    required <- c("label", "modelType", "fit", "predict")
    if (!all(required %in% names(method))) {
      stop("Custom model list is missing required elements.", call. = FALSE)
    }
    if (!is.function(method$fit) || !is.function(method$predict)) {
      stop("The 'fit' and 'predict' elements must be functions.", call. = FALSE)
    }
    if (!method$modelType %in% c("univariate", "bivariate")) {
      stop(
        "The 'modelType' element must be 'univariate' or 'bivariate'.",
        call. = FALSE)
    }
    model_info <- method
  } else {
    stop("`method` must be a character string or a list.", call. = FALSE)
  }
  return(model_info)
}
