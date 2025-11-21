#' @title The 1798 Map data of the Basel and Frickthal Region
#' @description A dataset of 343 Control Points (CPs) based on the sample
#' datasetfrom the MapAnalyst distortion analysis software. It's ideal for
#' analyzing complex, non-linear distortions.
#'
#' @details
#' This dataset is derived from the sample data provided with the MapAnalyst
#' software (<http://mapanalyst.cartography.ch>) in
#' https://github.com/mclaeysb/distortionAnalysis/tree/master/data_sample.
#'
#' It is a list that consists of an `gcp` object with 343 control points and
#' an `sf` object with a grid of 96 lines that represent the map in a vector
#' format.
#'
#' An old map to analyze, named "Die Landschaft Basel und das Frickthal -
#' entworfen und mit beweglichen Typen gesetzt von W. Haas". It shows part of
#' northern Switzerland and was produced in 1798 by W. Haas.
#'
#' previous analysis revealed: (a) a west-toeast scale variation from
#' approximately 1:175,000 to 1:190,000; and (b) significant distortions in the
#' south-eastern and northwestern corners
#'
#' This makes the dataset an excellent test case for evaluating the ability of
#' non linear models like `gam` to model and correct these challenging error
#' patterns, which a simple `helmert` or `lm` model would not be able to
#' address.
#'
#' @format An list with one `gcp` object (gcp) and one `sf` object (grid):
#' \describe{
#'   \item{gcp}{An `sf` object with 343 features and 6 variables:
#'     \describe{
#'       \item{source_x}{Numeric. The X-coordinate on the source map
#'       (already globally aligned).}
#'       \item{source_y}{Numeric. The Y-coordinate on the source map
#'       (already globally aligned).}
#'       \item{target_x}{Numeric. The X-coordinate on the reference map.}
#'       \item{target_y}{Numeric. The Y-coordinate on the reference map.}
#'       \item{dx}{Numeric. The residual difference in X (target_x - source_x).}
#'       \item{dy}{Numeric. The residual difference in Y (target_y - source_y).}
#'     }
#'   }
#'   \item{grid}{An `sf` object representing the map grid with 96 features
#'   (lines).}
#' }
#' @source Data originally provided as a sample dataset for the MapAnalyst
#'   distortion analysis software. See <http://mapanalyst.cartography.ch> and
#'   https://github.com/mclaeysb/distortionAnalysis/tree/master/data_sample.
#'
#' @examples
#' library(mapAI)
#'
#' # Load the dataset
#' data(basel_frickthal)
#'
#' # view what data contains
#' basel_frickthal$gcp
#' basel_frickthal$grid
#'
#' # plot old map distrortions
#' plot(basel_frickthal$gcp)
#'
"basel_frickthal"
