# Perform a Differential Distortion Analysis

Computes a comprehensive set of detailed distortion metrics for a PAI
model at specified locations, based on Tissot's indicatrix theory.

## Usage

``` r
analyze_distortion(pai_model, points_to_analyze, reference_scale = 1)
```

## Arguments

- pai_model:

  A model object of class `pai_model` from
  [`train_pai_model()`](https://kvantas.github.io/mapAI/reference/train_pai_model.md).

- points_to_analyze:

  An `sf` object of **points** where the analysis should be performed.

- reference_scale:

  A single numeric value used to normalize the area scale calculation.
  Defaults to `1` (no normalization).

## Value

An `sf` object containing the original points and new columns with all
calculated distortion metrics:

- a, b:

  The semi-major and semi-minor axes of the Tissot indicatrix.

- area_scale:

  The areal distortion factor (`a * b`).

- log2_area_scale:

  The base-2 logarithm of `area_scale`, a symmetric metric centered at
  0.

- max_shear:

  The maximum angular distortion in degrees.

- max_angular_distortion:

  The maximum angular distortion in radians (the `2Omega` metric).

- airy_kavrayskiy:

  The Airy-Kavrayskiy measure, a balanced metric combining areal and
  angular distortion.

- theta_a:

  The orientation of the axis of maximum scale (in degrees).

## Details

This function is the core analytical engine of the `mapAI` package. It
implements a differential analysis by calculating the first partial
derivatives of the spatial transformation learned by a `pai_model`. This
is achieved using a **numerical differentiation** (finite difference)
method that is universally applicable to all models in the package
(`helmert`, `tps`, `gam`, `lm`, `rf`,`svmRadial` and `svmLinear`).

From these derivatives, it calculates key distortion metrics that
describe how shape, area, and angles are warped at every point.

**Interpreting Results by Model Type:**

The nature of the output is **highly dependent** on the model used:

- **`gam` & `tps` (Recommended for this analysis)**: Produce a smooth,
  differentiable surface. The distortion metrics will be **spatially
  variable** and provide a rich, meaningful understanding of how
  distortion changes across the map.

- **`helmert` & `lm`**: Represent global transformations. The distortion
  metrics will be **constant for every point**.

- **`rf`**: Creates a step-like surface. The local derivatives may be
  effectively zero, resulting in metrics indicating no local distortion
  (e.g., `area_scale` = 1, `max_shear` = 0).

## Examples

``` r
# This example showcases the full analytical workflow.

library(magrittr)

# --- 1. Load data and train a GAM model ---
data(gcps)
gam_model <- train_pai_model(gcps, pai_method = "gam")
#> Training 'gam' model...

# --- 2. Create a regular grid of POINTS for analysis ---
analysis_points <- sf::st_make_grid(gcps, n = c(25, 25)) %>%
  sf::st_centroid() %>%
  sf::st_sf()

# --- 3. Run the distortion analysis ---
distortion_results <- analyze_distortion(gam_model, analysis_points)
#> Calculating distortion metrics for gam model...
#> Finalizing metrics from derivatives...
#> Distortion analysis complete.

# --- 4. Visualize the area scale ---
plot_distortion_surface(
  distortion_results,
  metric = "area_scale")
#> Regular grid detected. Creating a surface plot with geom_raster().
#> Warning: Raster pixels are placed at uneven horizontal intervals and will be shifted
#> ℹ Consider using `geom_tile()` instead.
#> Warning: Raster pixels are placed at uneven horizontal intervals and will be shifted
#> ℹ Consider using `geom_tile()` instead.

```
