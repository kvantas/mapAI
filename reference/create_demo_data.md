# Create a Simulated Historical Map Dataset for Demonstration

Generates a simulated dataset representing a distorted historical map
and a corresponding set of homologous points (GCPs), saving them as
files.

## Usage

``` r
create_demo_data(
  type = "complex",
  noise_sd = 0.5,
  n_points = 15,
  output_dir = tempdir(),
  seed = 42,
  grid_limits = c(0, 100, 0, 100),
  helmert_params = list(s = 1.005, angle_deg = 1, tx = 2, ty = -3),
  poly_params = list(cE1 = 2e-05, cE2 = -8e-04, cN1 = 2e-04, cN2 = 0.0015),
  gauss_params = list(A = 4, Ec = 50, Nc = 0, sigma2 = 20)
)
```

## Arguments

- type:

  A character string specifying the distortion type. One of "helmert",
  "nonlinear", or "complex". Defaults to "complex".

- noise_sd:

  A numeric value for the standard deviation of the Gaussian noise added
  to the distorted coordinates. Defaults to 0.5.

- n_points:

  An integer specifying the number of points along each axis of the
  initial grid. The total number of homologous points will be
  `n_points^2`. Defaults to 15.

- output_dir:

  A character string specifying the directory where the demo files will
  be saved. Defaults to a temporary directory
  ([`tempdir()`](https://rdrr.io/r/base/tempfile.html)).

- seed:

  An integer for setting the random seed for reproducibility. Defaults
  to 42.

- grid_limits:

  A numeric vector of the form `c(xmin, xmax, ymin, ymax)` defining the
  extent of the "true" grid. Defaults to `c(0, 100, 0, 100)`.

- helmert_params:

  A list of parameters for the Helmert transformation: `s` (scale),
  `angle_deg` (rotation in degrees), `tx` (translation in x), and `ty`
  (translation in y). Defaults to
  `list(s = 1.005, angle_deg = 1, tx = 2, ty = -3)`.

- poly_params:

  A list of coefficients (`cE1`, `cE2`, `cN1`, `cN2`) for the polynomial
  warp. Defaults to
  `list(cE1 = 0.00002, cE2 = -0.0008, cN1 = 0.0002, cN2 = 0.0015)`.

- gauss_params:

  A list of parameters for the Gaussian warp: `A` (amplitude), `Ec`,
  `Nc` (center coordinates), and `sigma2` (variance). Defaults to
  `list(A = 4, Ec = 50, Nc = 0, sigma2 = 20)`.

## Value

A list containing the full paths to the generated files:

- shp_path:

  The path to the 'demo_map.shp' shapefile.

- gcp_path:

  The path to the 'demo_gcps.csv' file.

## Details

This function implements the simulation framework described in Vantas
and Mirkopoulou, 2025. It first creates a regular grid of points
representing the "true" geography. It then applies one of three
distortion types from the paper:

- **`"helmert"`**: A simple global transformation involving scale,
  rotation, and translation.

- **`"nonlinear"`**: A Helmert transformation followed by a smooth
  polynomial warp, simulating material stretching.

- **`"complex"` (Default)**: Combines the Helmert and nonlinear warp
  with a localized Gaussian deformation, simulating a complex mix of
  global, regional, and local errors.

Finally, random noise is added to the distorted coordinates. The
function outputs a shapefile representing the distorted grid (as a set
of grid lines) and a CSV file of homologous points ready for use with
`read_correction_data()`.

## Examples

``` r
if (FALSE) { # \dontrun{
# --- 1. Generate the demonstration data with default complex distortion ---
demo_files <- create_demo_data(type = "complex", noise_sd = 0.5)

# --- Generate data with only Helmert distortion and custom parameters ---
custom_helmert_params <- list(s = 1.0, angle_deg = 5, tx = 10, ty = 15)
helmert_files <- create_demo_data(
  type = "helmert",
  helmert_params = custom_helmert_params,
  n_points = 10
 )

# --- 2. Use the generated files in the new workflow ---
# Read the GCPs and map separately
gcp_data <- read_gcps(gcp_path = demo_files$gcp_path)
map_to_correct <- read_map(shp_path = demo_files$shp_path)

# --- 3. Train a model ---
rf_model <- train_pai_model(gcp_data, pai_method = "rf")

# --- 4. Apply the trained model to the map ---
corrected_map <- apply_pai_model(pai_model = rf_model, map = map_to_correct)

# --- 5. Visualize the results ---
plot_correction_surface(rf_model, gcp_data)
} # }
```
