#' @title Create a Simulated Historical Map Dataset for Demonstration
#' @description Generates a simulated dataset representing a distorted
#'   historical map and a corresponding set of homologous points (GCPs), saving
#'   them as files.
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
#'   Finally, random noise is added to the distorted coordinates. The function
#'   outputs a shapefile representing the distorted grid (as a set of grid
#'   lines) and a CSV file of homologous points ready for use with
#'   `read_correction_data()`.
#'
#' @param type A character string specifying the distortion type. One of
#'  "helmert", "nonlinear", or "complex". Defaults to "complex".
#' @param noise_sd A numeric value for the standard deviation of the Gaussian
#'  noise added to the distorted coordinates. Defaults to 0.5.
#' @param n_points An integer specifying the number of points along each axis of
#'  the initial grid. The total number of homologous points will be
#'  `n_points^2`. Defaults to 15.
#' @param output_dir A character string specifying the directory where the demo
#'  files will be saved. Defaults to a temporary directory (`tempdir()`).
#' @param seed An integer for setting the random seed for reproducibility.
#'  Defaults to 42.
#' @param grid_limits A numeric vector of the form `c(xmin, xmax, ymin, ymax)`
#'   defining the extent of the "true" grid. Defaults to `c(0, 100, 0, 100)`.
#' @param helmert_params A list of parameters for the Helmert transformation:
#'   `s` (scale), `angle_deg` (rotation in degrees), `tx` (translation in x),
#'   and `ty` (translation in y). Defaults to
#'    `list(s = 1.005, angle_deg = 1, tx = 2, ty = -3)`.
#' @param poly_params A list of coefficients (`cE1`, `cE2`, `cN1`, `cN2`) for
#'  the polynomial warp. Defaults to
#'  `list(cE1 = 0.00002, cE2 = -0.0008, cN1 = 0.0002, cN2 = 0.0015)`.
#' @param gauss_params A list of parameters for the Gaussian warp: `A`
#'  (amplitude), `Ec`, `Nc` (center coordinates), and `sigma2` (variance).
#'  Defaults to `list(A = 4, Ec = 50, Nc = 0, sigma2 = 20)`.
#'
#' @return A list containing the full paths to the generated files:
#'   \item{shp_path}{The path to the 'demo_map.shp' shapefile.}
#'   \item{gcp_path}{The path to the 'demo_gcps.csv' file.}
#'
#' @import sf
#' @import dplyr
#' @importFrom stats predict rnorm
#' @importFrom utils write.csv
#' @export
#' @examples
#' \dontrun{
#' # --- 1. Generate the demonstration data with default complex distortion ---
#' demo_files <- create_demo_data(type = "complex", noise_sd = 0.5)
#'
#' # --- Generate data with only Helmert distortion and custom parameters ---
#' custom_helmert_params <- list(s = 1.0, angle_deg = 5, tx = 10, ty = 15)
#' helmert_files <- create_demo_data(
#'   type = "helmert",
#'   helmert_params = custom_helmert_params,
#'   n_points = 10
#'  )
#'
#' # --- 2. Use the generated files in the new workflow ---
#' # Read the GCPs and map separately
#' gcp_data <- read_gcps(gcp_path = demo_files$gcp_path)
#' map_to_correct <- read_map(shp_path = demo_files$shp_path)
#'
#' # --- 3. Train a model ---
#' rf_model <- train_pai_model(gcp_data, pai_method = "rf")
#'
#' # --- 4. Apply the trained model to the map ---
#' corrected_map <- apply_pai_model(pai_model = rf_model, map = map_to_correct)
#'
#' # --- 5. Visualize the results ---
#' plot_correction_surface(rf_model, gcp_data)
#' }
create_demo_data <- function(type = "complex",
                             noise_sd = 0.5,
                             n_points = 15,
                             output_dir = tempdir(),
                             seed = 42,
                             grid_limits = c(0, 100, 0, 100),
                             helmert_params = list(s = 1.005, angle_deg = 1,
                                                   tx = 2, ty = -3),
                             poly_params = list(cE1 = 0.00002, cE2 = -0.0008,
                                                cN1 = 0.0002, cN2 = 0.0015),
                             gauss_params = list(A = 4, Ec = 50, Nc = 0,
                                                 sigma2 = 20)) {
  set.seed(seed)

  # --- Internal Helper Functions ---

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

    data %>%
      dplyr::mutate(
        x_distorted = tx + s * (.data$x_true * cos(angle_rad) - .data$y_true * sin(angle_rad)),
        y_distorted = ty + s * (.data$x_true * sin(angle_rad) + .data$y_true * cos(angle_rad))
      )
  }

  apply_poly_warp <- function(data, params) {
    cE1 <- params$cE1; cE2 <- params$cE2
    cN1 <- params$cN1; cN2 <- params$cN2

    data %>%
      dplyr::mutate(
        x_distorted = .data$x_distorted + (cE1 * .data$x_distorted^2 + cE2 * .data$x_distorted * .data$y_distorted),
        y_distorted = .data$y_distorted + (cN1 * .data$y_distorted^2 + cN2 * .data$x_distorted * .data$y_distorted)
      )
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

    data %>% dplyr::mutate(
      x_distorted = .data$x_distorted + delta_x,
      y_distorted = .data$y_distorted + delta_y
    )
  }

  # 3. Noise function
  add_noise <- function(data, sd) {
    data %>%
      dplyr::mutate(
        x_distorted = .data$x_distorted + rnorm(dplyr::n(), 0, sd),
        y_distorted = .data$y_distorted + rnorm(dplyr::n(), 0, sd)
      )
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
  gcp_df <- final_data %>%
    dplyr::select(
      source_x = "x_distorted",
      source_y = "y_distorted",
      target_x = "x_true",
      target_y = "y_true"
    )
  gcp_path <- file.path(output_dir, "demo_gcps.csv")
  write.csv(gcp_df, gcp_path, row.names = FALSE)
  message(paste("   -> Homologous points saved to:", gcp_path))

  # 5. Create and save the "old map" shapefile (as a distorted grid)
  distorted_pts_matrix <- as.matrix(final_data[, c("x_distorted", "y_distorted")])

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

  shp_path <- file.path(output_dir, "demo_map.shp")
  sf::st_write(map_sf, shp_path, delete_layer = TRUE, quiet = TRUE)
  message(paste("   -> Distorted map saved to:", shp_path))

  # --- Return file paths ---
  return(list(shp_path = shp_path, gcp_path = gcp_path))
}
