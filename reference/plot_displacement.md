# Plot Displacement Vectors

Creates a visualization of the displacement vectors from source
(distorted) to target (true) coordinates for a set of homologous points.

## Usage

``` r
plot_displacement(
  gcp_data,
  title = "Distortion Displacement Vectors",
  subtitle = "Arrows point from distorted to true locations",
  arrow_color = "darkred",
  point_color = "red",
  exaggeration_factor = 1
)
```

## Arguments

- gcp_data:

  An `sf` object of homologous points, typically the output of
  [`read_gcps()`](https://kvantas.github.io/mapAI/reference/read_gcps.md).
  Must contain `source_x`, `source_y`, `target_x`, and `target_y`
  columns.

- title:

  A character string for the plot's main title.

- subtitle:

  A character string for the plot's subtitle.

- arrow_color:

  A character string specifying the color of the displacement arrows.

- point_color:

  A character string specifying the color of the points marking the
  source locations.

- exaggeration_factor:

  A numeric value to scale the length of the displacement vectors. A
  value of 2, for instance, will double their plotted length, making
  subtle displacements more visible. Defaults to 1 (no exaggeration).

## Value

A `ggplot` object, which can be further customized using standard
`ggplot2` syntax.

## Details

This function is a key exploratory tool for understanding the nature and
magnitude of positional error in a dataset before correction. It plots
arrows that originate at the distorted `source` coordinates and point to
the correct `target` coordinates. This provides an immediate visual
sense of the spatial patterns in the distortion (e.g., rotation,
scaling, or non-linear warping).

## Examples

``` r
# --- 1. First, create a demo dataset to work with ---
demo_files <- create_demo_data(type = "complex")
#>    -> Homologous points saved to: /tmp/RtmpnwcKtE/demo_gcps.csv
#>    -> Distorted map saved to: /tmp/RtmpnwcKtE/demo_map.shp
gcp_data <- read_gcps(gcp_path = demo_files$gcp_path)

# --- 2. Create the default displacement plot ---
plot_displacement(gcp_data)


# --- 3. Customize the plot with different titles and colors ---
plot_displacement(
  gcp_data,
  title = "My Custom Displacement Plot",
  subtitle = "Visualizing error vectors",
  arrow_color = "blue",
  point_color = "orange"
)


# --- 4. Exaggerate the displacement vectors for clearer visualization ---
plot_displacement(gcp_data, exaggeration_factor = 5)

```
