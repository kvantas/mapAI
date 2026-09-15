#' @title Create a Simulated Historical Map Dataset for Demonstration
#' @description Generates a simulated dataset representing a distorted
#'   historical map and a corresponding set of homologous points (GCPs),
#'   returing them inside a list
#' @details This function implements the simulation framework described in
#'   Vantas and Mirkopoulou, 2025. It first creates a regular grid of points
#'   representing the "true" geography. It then applies one of three distortion
#'   types from the paper:
#'   \itemize{
#'     \item \strong{`"helmert"`}: A simple global transformation involving
#'     scale, rotation, and translation.
#'     \item \strong{`"nonlinear"`}: A Helmert transformation followed by a
#'     smooth polynomial warp, simulating material stretching.
#'     \item \strong{`"complex"` (Default)}: Combines the Helmert and nonlinear
#'     warp with a localized Gaussian deformation, simulating a complex mix of
#'     global, regional, and local errors.
#'   }
#'   Finally, independent Gaussian noise with standard deviation `noise_sd` is
#'   added to each distorted coordinate. Everything is returned in memory as a
#'   list; nothing is written to disk.
#'
#'   \strong{On the achievable error floor.} The per-axis noise implies a 2D RMSE
#'   floor of \eqn{\sqrt{2} \times} `noise_sd` for a model that recovers the
#'   deformation exactly. For `type = "complex"` the true floor is higher, because
#'   the localized Gaussian term is a radial field
#'   \eqn{A e^{-r^2 / 2\sigma^2}\hat{\mathbf{r}}} whose magnitude tends to `A` as
#'   \eqn{r \to 0} while its direction depends on the angle of approach: the field
#'   is discontinuous at its centre and no smooth model (GAM, TPS, Helmert) can
#'   reproduce it. With the defaults that centre also sits on the lower edge of
#'   the grid (`Nc = 0`), so only half the feature lies inside the domain, and
#'   \eqn{\sigma \approx 4.5} is resolved by roughly one grid interval. Treat
#'   `"complex"` as a stress test rather than as data with a clean, known error
#'   floor; use `"nonlinear"` when you need a smooth target.
#'
#' @param type A character string specifying the distortion type. One of
#'  "helmert", "nonlinear", or "complex". Defaults to "complex".
#' @param noise_sd A numeric value for the standard deviation of the Gaussian
#'  noise added to the distorted coordinates. Defaults to 0.5.
#' @param n_points An integer specifying the number of points along each axis of
#'  the initial grid. The total number of homologous points will be
#'  `n_points^2`. Defaults to 15.
#' @param seed An integer for setting the random seed for reproducibility.
#'  Defaults to 42.
#' @param grid_limits A numeric vector of the form `c(xmin, xmax, ymin, ymax)`
#'   defining the extent of the "true" grid. Defaults to `c(0, 100, 0, 100)`.
#' @param helmert_params A list of parameters for the Helmert transformation:
#'   `s` (scale), `angle_deg` (rotation in degrees), `tx` (translation in x),
#'   and `ty` (translation in y). Defaults to
#'    `list(s = 1.005, angle_deg = 1, tx = 2, ty = -3)`.
#' @param poly_params A list of coefficients (`cE1`, `cE2`, `cN1`, `cN2`) for the
#'  second-order polynomial warp, applied simultaneously to both axes:
#'  \eqn{x' = x + c_{E1}x^2 + c_{E2}xy} and \eqn{y' = y + c_{N1}y^2 + c_{N2}xy}.
#'  Defaults to `list(cE1 = 0.00002, cE2 = -0.0008, cN1 = 0.0002, cN2 = 0.0015)`.
#' @param gauss_params A list of parameters for the Gaussian warp: `A`
#'  (amplitude), `Ec`, `Nc` (center coordinates), and `sigma2` (variance).
#'  Defaults to `list(A = 4, Ec = 50, Nc = 0, sigma2 = 20)`.
#' @param raster A logical value indicating whether to also generate and return
#'   an in-memory `terra::SpatRaster` representing the distorted map. Defaults to `FALSE`.
#'
#' @return A list containing the generated data:
#'   \item{gcp}{The homologous data (`gcp` object)}
#'   \item{map}{The distorted map grid lines as an `sf` object}
#'   \item{raster}{The distorted map as an in-memory `terra::SpatRaster` object (when `raster = TRUE`)}
#'
#' @importFrom sf st_linestring st_sfc st_as_sf
#' @importFrom terra vect rast rasterize
#' @importFrom stats predict rnorm
#' @export
#' @examples
#' \dontrun{
#' # Generate the demonstration data with default complex distortion
#' demo_data <- create_demo_data(type = "complex", noise_sd = 0.5)
#' # plot homologous points
#' plot(demo_data$gcp)
#'
#' # plot distorted map grid
#' plot(demo_data$map, col = "black", main = "")
#' }
create_demo_data <- function(type = "complex",
                             noise_sd = 0.5,
                             n_points = 15,
                             seed = 42,
                             grid_limits = c(0, 100, 0, 100),
                             helmert_params = list(s = 1.005, angle_deg = 1,
                                                   tx = 2, ty = -3),
                             poly_params = list(cE1 = 0.00002, cE2 = -0.0008,
                                                cN1 = 0.0002, cN2 = 0.0015),
                             gauss_params = list(A = 4, Ec = 50, Nc = 0,
                                                 sigma2 = 20),
                             raster = FALSE) {

  # --- Parameter validation (assuming this function is defined externally)
  validate_demo_data_inputs(
    type, noise_sd, n_points, seed, grid_limits,
    helmert_params, poly_params, gauss_params
  )

  # Restore the caller's global RNG state on exit.
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    .old_seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
    on.exit(assign(".Random.seed", .old_seed, envir = globalenv()), add = TRUE)
  } else {
    on.exit(suppressWarnings(rm(".Random.seed", envir = globalenv())), add = TRUE)
  }

  # --- Internal Helper Functions ---
  set.seed(seed)

  # 1. Base grid generation
  generate_true_grid <- function(n_pts, limits) {
    expand.grid(
      x_true = seq(limits[1], limits[2], length.out = n_pts),
      y_true = seq(limits[3], limits[4], length.out = n_pts)
    )
  }

  # 2. Distortion functions
  apply_helmert <- function(data, params) {
    s <- params$s
    angle_rad <- params$angle_deg * pi / 180
    tx <- params$tx
    ty <- params$ty

    data$x_distorted <- tx +
      s * (data$x_true * cos(angle_rad) - data$y_true * sin(angle_rad))
    data$y_distorted <- ty +
      s * (data$x_true * sin(angle_rad) + data$y_true * cos(angle_rad))
    return(data)
  }

  apply_poly_warp <- function(data, params) {
    cE1 <- params$cE1; cE2 <- params$cE2
    cN1 <- params$cN1; cN2 <- params$cN2

    # Snapshot the inputs. Updating x in place and then reading it back when
    # computing y makes the warp sequential rather than simultaneous, which
    # injects a spurious third-order term cN2 * (cE1*x^2 + cE2*x*y) * y. With the
    # default coefficients that term reaches 1.17 units, over twice the default
    # noise_sd, so the data would not follow the second-order polynomial family
    # this function documents.
    x0 <- data$x_distorted
    y0 <- data$y_distorted

    data$x_distorted <- x0 + (cE1 * x0^2 + cE2 * x0 * y0)
    data$y_distorted <- y0 + (cN1 * y0^2 + cN2 * x0 * y0)
    return(data)
  }

  apply_gauss_warp <- function(data, params) {
    A <- params$A; Ec <- params$Ec; Nc <- params$Nc; sigma2 <- params$sigma2

    dx <- data$x_distorted - Ec
    dy <- data$y_distorted - Nc
    dist_sq <- dx^2 + dy^2
    dist <- sqrt(dist_sq)

    gauss_factor <- A * exp(-dist_sq / (2 * sigma2))

    # Avoid division by zero at the center of the warp
    delta_x <- ifelse(dist == 0, 0, gauss_factor * (dx / dist))
    delta_y <- ifelse(dist == 0, 0, gauss_factor * (dy / dist))

    data$x_distorted <- data$x_distorted + delta_x
    data$y_distorted <- data$y_distorted + delta_y
    return(data)
  }

  # 3. Noise function
  add_noise <- function(data, sd) {
    n_rows <- nrow(data)
    data$x_distorted <- data$x_distorted + rnorm(n_rows, 0, sd)
    data$y_distorted <- data$y_distorted + rnorm(n_rows, 0, sd)
    return(data)
  }

  # --- Main Data Generation Logic ---

  # Generating true grid
  true_data <- generate_true_grid(n_pts = n_points, limits = grid_limits)

  # Applying distortion of type
  distorted_data <- switch(
    type,
    "helmert" = {
      apply_helmert(true_data, params = helmert_params)
    },
    "nonlinear" = {
      helmert_dist <- apply_helmert(true_data, params = helmert_params)
      apply_poly_warp(helmert_dist, params = poly_params)
    },
    "complex" = {
      helmert_dist <- apply_helmert(true_data, params = helmert_params)
      poly_dist <- apply_poly_warp(helmert_dist, params = poly_params)
      apply_gauss_warp(poly_dist, params = gauss_params)
    },
    stop("Invalid 'type'. Choose from 'helmert', 'nonlinear', or 'complex'.")
  )

  # Adding random noise with sd
  final_data <- add_noise(distorted_data, sd = noise_sd)

  # --- Create Output Files ---

  # 4. Create and save the homologous points (GCPs) CSV
  # Assuming read_gcp is an existing function that takes vectors
  gcp <- read_gcp(
    source_x = final_data$x_distorted,
    source_y = final_data$y_distorted,
    target_x = final_data$x_true,
    target_y = final_data$y_true
  )

  # 5. Create and save the "old map" shapefile (as a distorted grid)
  distorted_pts_matrix <-
    as.matrix(final_data[, c("x_distorted", "y_distorted")])

  # Create horizontal lines
  horiz_lines <- lapply(1:n_points, function(i) {
    row_indices <- seq(from = i, to = n_points * n_points, by = n_points)
    sf::st_linestring(distorted_pts_matrix[row_indices, ])
  })

  # Create vertical lines
  vert_lines <- lapply(1:n_points, function(i) {
    col_indices <- ( (i - 1) * n_points + 1):(i * n_points)
    sf::st_linestring(distorted_pts_matrix[col_indices, ])
  })

  # Combine into an sf object Assign a common placeholder CRS; users should
  # specify their known CRS upon reading
  grid_sfc <- sf::st_sfc(c(horiz_lines, vert_lines), crs = 3857)
  map_sf <- sf::st_as_sf(data.frame(id = seq_along(grid_sfc)), geom = grid_sfc)

  # --- Return Results (strictly in-memory) ---
  if (isTRUE(raster)) {
    map_vect <- terra::vect(map_sf)
    rast_template <- terra::rast(map_vect, nrows = 150, ncols = 150)
    r_map <- terra::rasterize(map_vect, rast_template, field = 1, background = 0, touches = TRUE)
    names(r_map) <- "grid_lines"
    return(list(gcp = gcp, map = map_sf, raster = r_map))
  }

  return(list(gcp = gcp, map = map_sf))
}
