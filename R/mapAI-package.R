#' mapAI: Positional Accuracy Improvement for Geospatial Vector and Raster Data
#'
#' @description The `mapAI` package provides a statistically rigorous,
#'   computationally efficient, and topology-resilient toolkit in R for the
#'   Positional Accuracy Improvement (PAI) and differential distortion analysis
#'   of geospatial vector and raster datasets.
#'
#' @details Designed for researchers and practitioners in geodesy, geomatics,
#'   and GIS, `mapAI` bridges classical geodetic transformations with modern
#'   spatial non-parametric regression and machine learning.
#'
#' ## The Core Workflow
#'
#' The workflow follows a structured sequence:
#'
#' 1.  **Data Ingestion & Ground Control Points:** Import spatial data and homologous
#'     points using `read_gcp()` (with CRS support) and `read_map()`, or generate
#'     controlled synthetic benchmarks using `create_demo_data()`.
#'
#' 2.  **Model Training & Integrated Validation:** Fit transformation models using
#'     `train_pai_model()` across a tiered algorithmic hierarchy:
#'     \itemize{
#'       \item `"helmert"`: 2D conformal similarity via Ordinary Least Squares (OLS)
#'         or Total Least Squares (TLS / SVD Procrustes) with analytical standard errors.
#'       \item `"lm"`: General affine bivariate linear regression.
#'       \item `"tps"`: Thin Plate Splines minimizing biharmonic bending energy.
#'       \item `"gam_biv"`: Bivariate Generalized Additive Models with thin plate
#'         regression splines and adaptive basis dimension \eqn{k}.
#'       \item Custom algorithms: Support Vector Machines, Random Forests, Neural Networks.
#'     }
#'     Integrated spatial cross-validation via `cv_pai_model()` or `train_pai_model(..., cv = TRUE)`
#'     supports six partition topologies (`"random"`, `"spatial"`, `"spatial_block"`,
#'     `"spatial_buffered"`, `"probability"`, `"stratified"`) to guard against spatial
#'     autocorrelation data leakage (Roberts et al., 2017).
#'
#' 3.  **Vector Transformation with Automated Topology Repair:** Apply the model to
#'     `sf` vector layers using `transform_map()`, preserving all OGC geometry types
#'     (including nested polygon rings), recalculating planar areas, and automatically
#'     repairing non-linear topological self-intersections via `sf::st_make_valid()`.
#'
#' 4.  **In-Memory Raster Rectification:** Correct continuous or discrete `terra`
#'     `SpatRaster` objects using `apply_pai_raster()`, utilizing a damped Picard-Mann
#'     iterative fixed-point coordinate inversion solver with regular mesh interpolation,
#'     operating 100% in RAM without temporary disk files.
#'
#' 5.  **Differential Distortion Analysis & Inversion Screening:** Quantify deformation
#'     fields using `analyze_distortion()` based on Tissot's indicatrices, Cauchy-Green
#'     metric tensor invariants, and signed Jacobian determinant \eqn{\det(\mathbf{J})},
#'     automatically flagging local topological fold-overs (\eqn{\det(\mathbf{J}) \le 0}).
#'
#' @references
#' \itemize{
#'   \item Tissot, A. (1881). \emph{Mémoire sur la représentation des surfaces et les projections des cartes géographiques}. Gauthier-Villars.
#'   \item Bookstein, F. L. (1989). Principal warps: Thin-plate splines and the decomposition of deformations. \emph{IEEE TPAMI}, 11(6), 567-585.
#'   \item Wood, S. N. (2003). Thin plate regression splines. \emph{JRSS-B}, 65(1), 95-114.
#'   \item Roberts, D. R. et al. (2017). Cross-validation strategies for data with spatial, temporal, or phylogenetic dependence. \emph{Ecography}, 40(8), 913-929.
#'   \item Valente, R., Vantas, K., & Carrera-Hernández, J. (2021). Positional accuracy assessment and improvement of historical maps using non-linear spline models. \emph{IJGIS}.
#' }
#'
#' @name mapAI
#' @aliases mapAI-package
#' @docType package
"_PACKAGE"
