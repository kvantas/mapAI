# Plot the Learned Correction Surface

Visualizes the spatial correction field (dx, dy) learned by a PAI model.

## Usage

``` r
plot_correction_surface(
  pai_model,
  gcp_data,
  n_grid = 100,
  plot_gcps = TRUE,
  dx_range = NULL,
  dy_range = NULL
)
```

## Arguments

- pai_model:

  A trained `pai_model` object returned by
  [`train_pai_model()`](https://kvantas.github.io/mapAI/reference/train_pai_model.md).

- gcp_data:

  The `sf` object of homologous points that was used for training the
  model.

- n_grid:

  The resolution of the interpolation grid used to create the smooth
  surface. Higher values create a more detailed plot but take longer to
  compute. Defaults to 100.

- plot_gcps:

  A logical value indicating whether to plot the GCP locations on the
  correction surfaces. Defaults to `TRUE`.

- dx_range:

  A numeric vector of length 2 specifying the limits for the `dx` color
  scale (e.g., `c(-10, 10)`). Defaults to `NULL`, which uses the data's
  range.

- dy_range:

  A numeric vector of length 2 specifying the limits for the `dy` color
  scale. Defaults to `NULL`.

## Value

A list containing two `ggplot` objects: `dx_plot` and `dy_plot`. You can
plot them individually.

## Details

This function serves as a key diagnostic tool for understanding the
behavior of a trained `pai_model`. It creates two raster plots: one for
the `dx` (East-West) corrections and one for the `dy` (North-South)
corrections.

The color intensity on the plots reveals the magnitude of the correction
at any given location. Contour lines show the gradient of the change,
and black crosses mark the location of the original Ground Control
Points (GCPs), showing where the model had direct information to learn
from.

By examining these surfaces, users can:

- Understand the spatial nature of the distortion their model has
  learned.

- Identify areas of high vs. low correction.

- Spot potential issues like extreme corrections or unusual artifacts,
  especially at the edges of the data where the model is extrapolating.

## Examples

``` r
if (FALSE) { # \dontrun{
# This example demonstrates how to train a model and then visualize
# what it has learned.

# --- 1. Load required data from the package ---
data(gcps)

# --- 2. Train a model to visualize ---
# A GAM (Generalized Additive Model) is a great choice for this
# because it naturally produces smooth surfaces that are easy to interpret.
pai_model_gam <- train_pai_model(gcps, pai_method = "gam")

# --- 3. Generate and display the plot ---
# This creates a list with two plots, one for dx and one for dy.
correction_plots <- plot_correction_surface(
  pai_model = pai_model_gam,
  gcp_data = gcps,
  n_grid = 75 # Using a slightly coarser grid for a quick example
)

# To display the plots, you can print them individually.
print(correction_plots$dx_plot)
print(correction_plots$dy_plot)

# --- 4. Set custom ranges for the color scales ---
custom_range_plots <- plot_correction_surface(
  pai_model = pai_model_gam,
  gcp_data = gcps,
  dx_range = c(-1, 1),
  dy_range = c(-1, 1)
)
print(custom_range_plots$dx_plot)
} # }
```
