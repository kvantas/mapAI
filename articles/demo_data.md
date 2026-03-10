# Getting Started with mapAI: A Complete Workflow

This vignette provides a complete, step-by-step workflow for using the
`mapAI` package. For users new to the package, this is the best place to
start.

We will begin by generating a synthetic dataset that mimics a distorted
historical map. This allows us to accurately measure how much our model
improves the map’s accuracy. The workflow consists of two main parts:

1.  **Positional Correction**: A basic workflow to correct the geometry
    of a distorted map.
2.  **Advanced Distortion Analysis**: A deeper dive into quantifying and
    visualizing the nature of the distortion itself using techniques
    based on Tissot’s indicatrix theory.

``` r
# Load the necessary libraries
library(mapAI)
library(ggplot2)
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
library(magrittr) # For the %>% pipe operator
```

## Positional Correction Workflow

### Generate and Load Synthetic Data

First, we use
[`create_demo_data()`](https://kvantas.github.io/mapAI/reference/create_demo_data.md)
to create our test case. We’ll choose the `"complex"` distortion type as
it represents a challenging combination of errors. We then load these
files into R using the package’s reading functions.

``` r
# Create the shapefile and GCPs CSV in a temporary directory
demo_files <- create_demo_data(type = "complex", seed = 123)
#>    -> Homologous points saved to: /tmp/RtmpPi3tZn/demo_gcps.csv
#>    -> Distorted map saved to: /tmp/RtmpPi3tZn/demo_map.shp

# Load the GCPs (homologous points) from the demo file
gcp_data <- read_gcps(gcp_path = demo_files$gcp_path)

# Load the vector map that needs correction from the demo file
map_to_correct <- read_map(shp_path = demo_files$shp_path)
#> Reading layer `demo_map' from data source `/tmp/RtmpPi3tZn/demo_map.shp' using driver `ESRI Shapefile'
#> Simple feature collection with 30 features and 1 field
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -0.06808406 ymin: -4.288813 xmax: 102.7102 ymax: 114.2245
#> Projected CRS: WGS 84 / Pseudo-Mercator
```

### Assess Initial Error and Train a Model

We calculate the initial 2D Root Mean Squared Error (RMSE) to get a
baseline for the positional error and then we use cross validation to
estimate the out-of-sample error of a **Generalized Additive Model
(`gam`)**. GAMs are well-suited for capturing the smooth, non-linear
distortions in our synthetic data.

``` r
# Calculate the initial error before correction
identity_rmse <- sqrt(mean(gcp_data$dx^2 + gcp_data$dy^2))
print(paste("Initial (Identity) 2D RMSE:", round(identity_rmse, 4)))
#> [1] "Initial (Identity) 2D RMSE: 5.3465"

# use cross validation to assess a GAM's model accuracy
cv_res <- assess_pai_model(gcp_data,pai_method = "gam")
print(cv_res)
#>   Method ValidationType Mean_RMSE_2D SD_RMSE_2D
#> 1    gam         random    0.7495636 0.06099847
# Train the GAM model using our GCP data
pai_model_gam <- train_pai_model(gcp_data, pai_method = "gam")
```

### Apply Correction and Visualize the Result

We apply our trained model to the map grid and then visualize the result
by overlaying the corrected grid on the original.

``` r
# Apply the trained model to the full map
corrected_map <- apply_pai_model(pai_model = pai_model_gam, map = map_to_correct)
#> Applying PAI model to map features...
#> Correction complete.

# For easy plotting, add a "status" column and combine the maps
map_to_correct$status <- "Original (Distorted)"
corrected_map$status <- "Corrected"
comparison_data <- rbind(map_to_correct[, "status"], corrected_map[, "status"])

# Create the final comparison plot
ggplot(comparison_data) +
  geom_sf(aes(color = status, linetype = status), fill = NA, linewidth = 0.7) +
  scale_color_manual(values = c("Original (Distorted)" = "grey50", "Corrected" = "red")) +
  scale_linetype_manual(values = c("Original (Distorted)" = "dashed", "Corrected" = "solid")) +
  labs(
    title = "Map Correction Comparison",
    subtitle = "Overlay of the original and corrected map grids"
  ) +
  theme_minimal()
```

![](reference/figures/gets_visualize-1.png) The plot clearly shows how
the warped grid has been adjusted back to a regular shape, demonstrating
a successful correction.

------------------------------------------------------------------------

## Advanced Distortion Analysis

Beyond simple correction, `mapAI` provides powerful tools to quantify
and visualize the distortion itself. This is useful for understanding
the history of a map’s creation or identifying areas of high
uncertainty.

### Step 4: Run the Distortion Analysis

We first create a regular grid of **points** for our analysis. Then, we
use the
[`analyze_distortion()`](https://kvantas.github.io/mapAI/reference/analyze_distortion.md)
function with our trained `gam_model` to calculate the detailed
distortion metrics at each point.

``` r
# Create a regular grid of points for the analysis
analysis_points <- sf::st_make_grid(gcp_data, n = c(25, 25)) %>%
  sf::st_centroid() %>%
  sf::st_sf()

# Run the analysis
distortion_results <- analyze_distortion(pai_model_gam, analysis_points)

# Glimpse the new columns added to our data
glimpse(distortion_results)
#> Rows: 625
#> Columns: 9
#> $ geometry               <POINT> POINT (1.987481 -1.918547), POINT (6.09861 -1…
#> $ a                      <dbl> 1.0201617, 1.0132633, 1.0082489, 1.0056066, 1.0…
#> $ b                      <dbl> 0.9378401, 0.9356228, 0.9331371, 0.9299045, 0.9…
#> $ area_scale             <dbl> 0.9567486, 0.9480322, 0.9408344, 0.9351181, 0.9…
#> $ log2_area_scale        <dbl> -0.06378823, -0.07699201, -0.08798727, -0.09677…
#> $ max_shear              <dbl> 2.409636, 2.283177, 2.217313, 2.241536, 2.30422…
#> $ max_angular_distortion <dbl> 0.08411216, 0.07969792, 0.07739883, 0.07824438,…
#> $ airy_kavrayskiy        <dbl> 0.002258491, 0.002300784, 0.002428281, 0.002656…
#> $ theta_a                <dbl> -0.01012249, -4.08984269, -8.30682561, -11.7028…
```

### Step 5: Visualize Distortion Metrics

Now we can use
[`plot_distortion_surface()`](https://kvantas.github.io/mapAI/reference/plot_distortion_surface.md)
to visualize these metrics.

#### Areal Distortion (log2σ) and Angular Distortion (2Ω)

These are two of the most fundamental distortion metrics.
`log2_area_scale` shows where the map has been stretched or shrunk,
while `max_angular_distortion_rad` shows where angles have been deformed
(shear).

``` r
# Plot for log2(sigma) - areal distortion
plot_area <- plot_distortion_surface(
  distortion_results,
  metric = "log2_area_scale",
  diverging = TRUE
) +
  labs(title = "Areal Distortion (log2σ)")
#> Regular grid detected. Creating a surface plot with geom_raster().

# Plot for 2*Omega - angular distortion
plot_shear <- plot_distortion_surface(
  distortion_results,
  metric = "max_angular_distortion",
  palette = "magma"
) +
  labs(title = "Angular Distortion (2Ω)")
#> Regular grid detected. Creating a surface plot with geom_raster().


  plot_shear
#> Warning: Raster pixels are placed at uneven horizontal intervals and will be shifted
#> ℹ Consider using `geom_tile()` instead.
#> Raster pixels are placed at uneven horizontal intervals and will be shifted
#> ℹ Consider using `geom_tile()` instead.
```

![](reference/figures/gets_plotsurfaces-1.png)

``` r
  plot_area
#> Warning: Raster pixels are placed at uneven horizontal intervals and will be shifted
#> ℹ Consider using `geom_tile()` instead.
#> Raster pixels are placed at uneven horizontal intervals and will be shifted
#> ℹ Consider using `geom_tile()` instead.
```

![](reference/figures/gets_plotsurfaces-2.png)

#### Airy-Kavrayskiy Measure

This is a combined measure of distortion that balances both areal and
angular errors. We can calculate it from the `a` and `b` values produced
by `analyze_distortion`.

``` r
# Plot the Airy-Kavrayskiy metric
plot_distortion_surface(
  distortion_results,
  metric = "airy_kavrayskiy",
  palette = "cividis"
) +
  labs(title = "Airy-Kavrayskiy Combined Distortion Measure")
#> Regular grid detected. Creating a surface plot with geom_raster().
#> Warning: Raster pixels are placed at uneven horizontal intervals and will be shifted
#> ℹ Consider using `geom_tile()` instead.
#> Raster pixels are placed at uneven horizontal intervals and will be shifted
#> ℹ Consider using `geom_tile()` instead.
```

![](reference/figures/gets_plotairy-1.png)

#### Tissot’s Indicatrices

Finally, `plot_indicatrices` provides the richest visualization, showing
the distortion ellipses themselves. The shape, size, and orientation of
each ellipse represent the local distortion pattern.

``` r
# Plot the indicatrices at the locations of the analysis points
# We use a large scale_factor to make the ellipses clearly visible.
plot_indicatrices(
  distortion_sf = distortion_results,
  scale_factor = 1,
  fill_color = "lightblue",
  border_color = "black"
)
#> Generating indicatrix polygons at source locations...
```

![](reference/figures/gets_plotindicatrices-1.png) This advanced
analysis demonstrates how `mapAI` can be used not only to fix historical
maps, but to deeply understand their geometric properties.
