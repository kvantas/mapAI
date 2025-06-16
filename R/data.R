#' Kastoria 1925 Cadastre Map Data
#'
#' A dataset containing a part of the processed 1925 Cadastre map of Kastoria in
#' shape file format and a list of homologous points, prepared for positional
#' accuracy improvement modeling.
#'
#' @format A list of class `pai_data` with two elements:
#' \describe{
#'   \item{map_to_correct}{An `sf` object representing the original 1925 Cadastre map,
#'     including an `area_old` column for polygon geometries.}
#'   \item{gcp_data}{An `sf` object of homologous points with `dx` and `dy`
#'     displacement vectors.}
#' }
#' @source 1925 Cadastre map of Kastoria, Greece and homologous points.
#' @examples
#' \dontrun{
#' # This data is typically loaded automatically with the package.
#' # You can access it directly:
#' data(kastoria)
#' print(kastoria$map_to_correct)
#' print(kastoria$gcp_data)
#' }
"kastoria"
