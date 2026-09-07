#' mapAI: Positional Accuracy Improvement for Historical Maps
#'
#' @description The `mapAI` package provides a cohesive, end-to-end toolkit in R
#' for the Positional Accuracy Improvement (PAI) and distortion analysis of
#' vector maps.
#'
#' @details The package is designed for researchers and practitioners in
#' geomatics and GIS, It provides a complete, modular
#' workflow that guides the user from importing data to final analysis and
#' visualization.
#'
#' ## The Core Workflow
#'
#' The main workflow follows a logical sequence:
#'
#' 1.  **Data Handling:** Imports spatial data and control points using
#' `read_map()` and `read_gcps()`. The package also includes functions to
#' generate synthetic data (`create_demo_data()`) and save results
#' (`write_map()`).
#'
#' 2.  **Model Training & Validation:** Train a correction model using
#' `train_pai_model()` with a choice of methods `helmert`,`tps`, `gam`, `lm`,
#' `rf`, `svmRadial` and `svmLinear`. Robustly evaluate model performance
#' using spatial cross-validation with `assess_pai_model()`.
#'
#' 3.  **Geometric Correction:** Apply the trained model to the full vector map
#' to get a geometrically corrected version using `apply_pai_model()`.
#'
#' 4.  **Distortion Analysis & Visualization:** Go beyond correction to quantify
#' and understand the distortion that learns a PAI model itself.
#'     \itemize{
#'       \item Use `analyze_distortion()` to compute detailed metrics based on
#'       Tissot's indicatrix theory (e.g., areal scale, angular distortion).
#'       \item Use the dedicated plotting functions (`plot_displacement()`,
#'         `plot_residuals()`, `plot_distortion_surface()`,
#'         `plot_indicatrices()`) to create compelling, publication-quality
#'         visualizations of the error and distortion patterns.
#'     }
#'
#' @seealso To get started with a practical example, see the introductory
#' vignette: `vignette("getting-started", package = "mapAI")`
#'
#' For a more in-depth look at comparing models and validation strategies, see
#' the advanced vignette: `vignette("swiss-analysis", package = "mapAI")`
#'
#' The project's source code and issue tracker can be found on GitHub:
#' <https://github.com/kvantas/mapAI>
#'
#' @name mapAI
#' @aliases mapAI-package
#' @docType package
"_PACKAGE"
