# Advanced Analysis with Swiss Data

This vignette demonstrates a workflow for analyzing and correcting a
historical map with complex, non-linear distortions. While the
`Getting Started` vignette used a synthetic data set, this one focuses
on a more challenging real-world scenario using the `swiss_cps` dataset.

The key feature of this dataset is that the gross, linear distortions
have already been removed with a Helmert transformation. The remaining
errors are subtle, non-linear, and spatially variable. Our goal is to
assess different validation techniques, train a robust model for these
non-linear residuals, and conduct an in-depth analysis of the spatial
distortion.

### Setup

First, we load all the necessary libraries for our workflow.

``` r
# Load the necessary libraries
library(mapAI)
library(sf)
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(ggplot2)
library(knitr)
```

### Initial Data Analysis

We begin by loading the `swiss_cps` dataset. This `sf` object contains
343 control points where the `source` coordinates have been pre-aligned,
so we can focus on the residual `dx` and `dy` errors.

``` r
# Load the built-in Swiss control points dataset
data(swiss_cps)

# Inspect the data structure
glimpse(swiss_cps)
#> Rows: 343
#> Columns: 8
#> $ Index    <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18…
#> $ source_x <dbl> 612293.0, 612225.0, 615470.3, 616339.8, 616927.1, 617571.9, 6…
#> $ source_y <dbl> 267353.6, 270412.7, 270195.1, 269367.5, 269507.6, 269273.5, 2…
#> $ target_x <dbl> 611375.9, 611573.1, 615840.8, 616129.6, 617031.0, 618168.7, 6…
#> $ target_y <dbl> 267719.1, 270370.6, 270463.6, 269380.6, 268932.1, 269179.3, 2…
#> $ dx       <dbl> -917.1063, -651.9320, 370.4737, -210.1946, 103.8862, 596.7662…
#> $ dy       <dbl> 365.468249, -42.050863, 268.478200, 13.144895, -575.530782, -…
#> $ geometry <POINT [m]> POINT (612293 267353.6), POINT (612225 270412.7), POINT…

# Visualize the residual displacement vectors
plot_displacement(swiss_cps, title = "Displacement Vectors") +
  labs(title = "", subtitle = "",
       x = "x (m)",
       y = "y (m)")
```

![](reference/figures/sw_load_data-1.png)

The plot shows clear spatial patterns in the remaining errors, which a
simple linear model could not fix. This confirms the need for a
non-linear approach.

### Error Assessment and Model Validation

Before training a final model, it is crucial to assess its expected
performance. The `assess_pai_model` function provides a robust framework
for this. We will compare four different validation strategies to
understand how the model might perform on new data.

- **Standard & Spatial Cross-Validation**: We test a 10-fold
  cross-validation, once with random data splits and once with “spatial”
  splits, which ensures that training and validation sets are spatially
  separated.
- **Design-Based Validation**: We also test validation based on sampling
  probability (`probability`) and stratified random sampling
  (`stratified`), which are useful when the data points have a known
  design or structure.

``` r
# Run four different validation methods using a GAM
cvra <- assess_pai_model(swiss_cps, pai_method = "gam", validation_type = "random", k_folds = 10, seed = 1)
cvsp <- assess_pai_model(swiss_cps, pai_method = "gam", validation_type = "spatial", k_folds = 10, seed = 1)
valr <- assess_pai_model(swiss_cps, pai_method = "gam", validation_type = "probability", seed = 1)
vals <- assess_pai_model(swiss_cps, pai_method = "gam", validation_type = "stratified", seed = 1)

# Combine and format the results into a table
validation_df <- rbind(cvra, cvsp, valr, vals) %>%
  select(Model_Method = Method, Mean_2D_RMSE_m = Mean_RMSE_2D, Std_Deviation_of_RMSE_m = SD_RMSE_2D) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1)))

validation_df$type <- c("Standard 10-fold cross-validation",
                       "Spatial 10-fold cross-validation",
                       "Design-based validation",
                       "Design-based validation using stratified random sampling")

# Print the comparison table
kable(validation_df, caption = "Table: Validation performance across different assessment methods.")
```

| Model_Method | Mean_2D_RMSE_m | Std_Deviation_of_RMSE_m | type                                                     |
|:-------------|---------------:|------------------------:|:---------------------------------------------------------|
| gam          |          778.6 |                   176.8 | Standard 10-fold cross-validation                        |
| gam          |         1254.8 |                   796.7 | Spatial 10-fold cross-validation                         |
| gam          |          876.7 |                      NA | Design-based validation                                  |
| gam          |          702.3 |                      NA | Design-based validation using stratified random sampling |

Table: Validation performance across different assessment methods.

### Final Correction and Visualization

Now, confident in our modeling choice, we train a final **Generalized
Additive Model (`gam`)** on the entire dataset. To visualize the effect
of the correction, we will apply this model to a regular reference grid.

``` r
# Train the final GAM model on all data points
final_gam_model <- train_pai_model(swiss_cps, pai_method = "gam")
#> Training 'gam' model...

# Create a regular line grid to serve as our reference map
source_grid <- sf::st_make_grid(swiss_cps, n = c(15, 15)) %>% 
  st_cast("MULTILINESTRING") %>% 
  st_sf()

# Apply the trained model to correct the grid
gam_corrected_grid <- apply_pai_model(final_gam_model, source_grid)
#> Applying PAI model to map features...
#> Correction complete.

# Prepare data for plotting
source_grid$status <- "Original (Distorted)"
gam_corrected_grid$status <- "GAM"
comparison_data  <- rbind(source_grid[, "status"], gam_corrected_grid[, "status"])

# Create the comparison plot
ggplot(comparison_data) +
  geom_sf(aes(color = status, linetype = status), fill = NA, linewidth = 0.7) +
  scale_color_manual(name = "Map Status", values = c("Original (Distorted)" = "grey50", "GAM" = "#e41a1c")) +
  geom_point(data = swiss_cps, aes(source_x, source_y), size = 0.2) +
  scale_linetype_manual(name = "Map Status", values = c("Original (Distorted)" = "dashed", "GAM" = "solid")) +
  labs(x = "x (m)", y = "y (m)") +
  coord_sf(datum = sf::st_crs(21781)) +
  ggplot2::theme_minimal()
```

![](reference/figures/sw_trainmodel-1.png)

The plot shows how the regular grid (dashed grey) is warped by the model
into the corrected grid (solid red). This transformation is precisely
what’s needed to align the distorted map features with their true
locations.

### Residual Analysis

After applying the correction, we should examine the remaining errors,
or residuals. This helps us confirm that our model has captured the
systematic distortion patterns and that the remaining errors are small
and random.

``` r
# Plot the residuals after applying the final GAM model
plot_residuals(final_gam_model, swiss_cps) +
  labs(
    title = "",
    subtitle = "",
    x = "x (m)",
    y = "y (m)"
  )
#> Calculating model residuals...
```

![](reference/figures/sw_residuals-1.png)

The residual vectors are much smaller and show no obvious spatial
pattern, indicating a successful model fit.

### Advanced Distortion Analysis

To quantify the nature of the distortion, we use the
`analyze_distortion` function on a dense grid of points. This allows us
to map out different distortion metrics across the entire area. We will
visualize three key metrics:

- **Area Scale**: Shows how much areas are stretched or compressed. A
  value of 1 means no distortion.
- **Maximum Angular Distortion**: Measures the maximum change in angle
  at any point.
- **Airy-Kavrayskiy Measure**: A combined measure of both areal and
  angular distortion.

``` r
# Create a dense grid of points for a high-resolution analysis
analysis_points <- sf::st_make_grid(swiss_cps, n = c(100, 100)) %>% st_centroid() %>% st_sf()
distortion_results <- analyze_distortion(final_gam_model, analysis_points)
#> Calculating distortion metrics for gam model...
#> Finalizing metrics from derivatives...
#> Distortion analysis complete.

# Plot 1: Area scale distortion
plot_distortion_surface(distortion_results, "area_scale", diverging = TRUE) +
  labs(title = "Area Scale Distortion", subtitle = "", x = "x (m)", y = "y (m)", fill = 'σ')
#> Regular grid detected. Creating a surface plot with geom_raster().
#> Warning: Raster pixels are placed at uneven horizontal intervals and will be shifted
#> ℹ Consider using `geom_tile()` instead.
#> Raster pixels are placed at uneven horizontal intervals and will be shifted
#> ℹ Consider using `geom_tile()` instead.
```

![](reference/figures/sw_distortions-1.png)

``` r

# Plot 2: Maximum angular distortion
plot_distortion_surface(distortion_results, "max_angular_distortion", palette = "magma") +
  labs(title = "Maximum Angular Distortion", subtitle = "", x = "x (m)", y = "y (m)", fill = "2Ω (rad)")
#> Regular grid detected. Creating a surface plot with geom_raster().
#> Warning: Raster pixels are placed at uneven horizontal intervals and will be shifted
#> ℹ Consider using `geom_tile()` instead.
#> Raster pixels are placed at uneven horizontal intervals and will be shifted
#> ℹ Consider using `geom_tile()` instead.
```

![](reference/figures/sw_distortions-2.png)

``` r

# Plot 3: Airy-Kavrayskiy distortion measure
plot_distortion_surface(distortion_results, "airy_kavrayskiy") +
  labs(title = "Airy-Kavrayskiy Combined Distortion", subtitle = "", x = "x (m)", y = "y (m)", fill = expression(E["AK"]))
#> Regular grid detected. Creating a surface plot with geom_raster().
#> Warning: Raster pixels are placed at uneven horizontal intervals and will be shifted
#> ℹ Consider using `geom_tile()` instead.
#> Raster pixels are placed at uneven horizontal intervals and will be shifted
#> ℹ Consider using `geom_tile()` instead.
```

![](reference/figures/sw_distortions-3.png)

We can also summarize these distortion metrics numerically.

``` r
# Calculate and print summary statistics for the distortion metrics
summary_stats <- st_drop_geometry(distortion_results) %>%
  reframe(
    Metric = c("Areal Scale", "Max Angular Distortion (rad)", "Airy-Kavrayskiy Measure"),
    Minimum = round(c(min(area_scale), min(max_angular_distortion), min(airy_kavrayskiy)), 2),
    Mean = round(c(mean(area_scale), mean(max_angular_distortion), mean(airy_kavrayskiy)), 2),
    Maximum = round(c(max(area_scale), max(max_angular_distortion), max(airy_kavrayskiy)), 2)
  )
print(kable(summary_stats, caption = "Table: Summary of distortion metrics from GAM model analysis."))
#> 
#> 
#> Table: Table: Summary of distortion metrics from GAM model analysis.
#> 
#> |Metric                       | Minimum| Mean| Maximum|
#> |:----------------------------|-------:|----:|-------:|
#> |Areal Scale                  |    0.45| 0.87|    1.41|
#> |Max Angular Distortion (rad) |    0.00| 0.21|    0.86|
#> |Airy-Kavrayskiy Measure      |    0.00| 0.04|    0.28|
```

### Visualizing Distortion with Indicatrices

A classic way to visualize map distortion is with **Tissot’s
Indicatrix**, which shows how an infinitesimal circle on the original
surface is deformed into an ellipse. We can plot these at our control
point locations to see the local distortion characteristics.

``` r
# Analyze distortion specifically at the control point locations
distortion_at_gcps <- analyze_distortion(final_gam_model, points_to_analyze = swiss_cps)
#> Calculating distortion metrics for gam model...
#> Finalizing metrics from derivatives...
#> Distortion analysis complete.

# Plot the indicatrices (ellipses) at each point
plot_indicatrices(distortion_at_gcps,  scale_factor = 700) +
  labs(
    title = "Distortion Indicatrices at Control Points",
    subtitle = "",
    x = "x (m)",
    y = "y (m)"
  )
#> Generating indicatrix polygons at source locations...
```

![](reference/figures/sw_indicatrices-1.png)

The size and shape of the ellipses clearly show the spatial variation in
both the magnitude and direction of the distortion that our model has
successfully captured.

This vignette has demonstrated a comprehensive workflow for a complex,
real-world PAI task. By combining robust validation, non-linear
modeling, and advanced distortion analysis, `mapAI` provides a powerful
toolkit for any historical map correction project.
