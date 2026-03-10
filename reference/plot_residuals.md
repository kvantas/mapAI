# Plot Model Residuals at Homologous Point Locations

Creates a vector plot showing the residual errors of a trained PAI model
at the location of the Ground Control Points (GCPs).

## Usage

``` r
plot_residuals(
  pai_model,
  gcp_data,
  title = "Model Residual Error Vectors",
  subtitle = "Arrows point from predicted to true target locations",
  arrow_color = "darkblue",
  point_color = "blue",
  exaggeration_factor = 1
)
```

## Arguments

- pai_model:

  An object of class `pai_model` from
  [`train_pai_model()`](https://kvantas.github.io/mapAI/reference/train_pai_model.md).

- gcp_data:

  An `sf` object of homologous points, from
  [`read_gcps()`](https://kvantas.github.io/mapAI/reference/read_gcps.md).

- title:

  A character string for the plot's main title.

- subtitle:

  A character string for the plot's subtitle.

- arrow_color:

  A character string specifying the color of the residual arrows.

- point_color:

  A character string specifying the color of the points marking the
  predicted locations.

- exaggeration_factor:

  A numeric value to scale the length of the residual vectors. A value
  of 2, for instance, will double their plotted length, making subtle
  residuals more visible. Defaults to 1 (no exaggeration).

## Value

A `ggplot` object, which can be further customized.

## Details

This is a crucial diagnostic function for assessing model performance.
It answers the question: "What errors did the model fail to correct?"

The function first predicts the correction for each GCP. It then
calculates the model's predicted target coordinate for each point. The
resulting arrows are drawn starting from this **predicted target
location** and pointing to the **true target location**.

A perfect model would have zero-length residual vectors. The presence of
long arrows or clear spatial patterns in the residuals may indicate that
the chosen model was not complex enough to capture the full distortion
pattern.

## Examples

``` r
# --- 1. Load data and train a simple model ---
data(swiss_cps)
# A helmert model is used as it will leave significant residuals.
helmert_model <- train_pai_model(swiss_cps, pai_method = "helmert")
#> Fitting Helmert transformation...

# --- 2. Plot the residuals with default colors ---
plot_residuals(helmert_model, swiss_cps)
#> Calculating model residuals...


# --- 3. Exaggerate the residuals to make them more visible ---
plot_residuals(helmert_model, swiss_cps, exaggeration_factor = 3)
#> Calculating model residuals...


# --- 4. Compare with a more advanced model ---
gam_model <- train_pai_model(swiss_cps, pai_method = "gam")
#> Training 'gam' model...

# The residuals for the GAM model should be much smaller and more random.
plot_residuals(gam_model, swiss_cps,
               arrow_color = "#006d2c", # A dark green
               point_color = "#66c2a5"  # A lighter green
)
#> Calculating model residuals...

```
