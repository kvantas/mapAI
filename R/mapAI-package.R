#' mapAI: Positional Accuracy Improvement for Vector Maps
#'
#' @description The `mapAI` package provides a cohesive, end-to-end toolkit in R
#' for the Positional Accuracy Improvement (PAI) and distortion analysis of
#' vector maps.
#'
#' @details The package is designed for researchers and practitioners in
#'   geomatics and GIS, It provides a complete, modular workflow that guides the
#'   user from importing data to final analysis and visualization.
#'
#' ## The Core Workflow
#'
#' The main workflow follows a logical sequence:
#'
#' 1.  **Data Handling:** Imports spatial data and control points using
#' `read_map()` and `read_gcp()`. The package also includes functions to
#' generate synthetic data (`create_demo_data()`) and save results
#' (`write_map()`).
#'
#' 2.  **Model Training & Validation:** Train a correction model using
#' `train_pai_model()` with a choice of methods `helmert`,`tps`, `gam`, `lm`,
#' `rf`,  or a completely custom model on the fly.
#'
#' 3. Robustly evaluate model performance using spatial cross-validation with
#'  `cv_pai_model()`.
#'
#' 4.  **Geometric Correction:** Apply the trained model to the full vector map
#' to get a geometrically corrected version using `transform_map()`.
#'
#' 5.  **Distortion Analysis & Visualization:** Go beyond correction to quantify
#' and understand the distortion that learns a PAI model itself.
#'     \itemize{
#'       \item Use `analyze_distortion()` to compute detailed metrics based on
#'       Tissot's indicatrix theory (e.g., areal scale, angular distortion).
#'       \item Use  `plot` to create publication-quality
#'         visualizations of the error and distortion patterns.
#'     }
#'
#' The project's source code and issue tracker can be found on GitHub:
#' <https://github.com/kvantas/mapAI>
#'
#' @name mapAI
#' @aliases mapAI-package
#' @docType package
"_PACKAGE"
