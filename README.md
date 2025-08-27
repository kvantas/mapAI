
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mapAI

<!-- badges: start -->

[![packageversion](https://img.shields.io/badge/Package%20version-0.4.0-orange.svg?style=flat-square)](https://github.com/kvantas/mapAI)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15767080.svg)](https://doi.org/10.5281/zenodo.15767080)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R-CMD-check](https://github.com/kvantas/mapAI/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kvantas/mapAI/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The `mapAI` package is designed to provide a comprehensive and
accessible PAI (positional accuracy improvement) framework for vector
geospatial data.

## Overview

The `mapAI` package provides a comprehensive and accessible framework
for Positional Accuracy Improvement (PAI) of vector geospatial data.
This package’s main contributions are:

1)  the unification of a set of PAI methods from classical adjustments
    to statistical and machine learning algorithms, within a framework
    engineered to modify the geometry of spatial features;

2)  the application of modern best practices regarding predictive
    accuracy assessment using;

3)  the integration of distortion analysis into the PAI workflow,
    providing powerful diagnostics.

## Installation

You can install the development version of `mapAI` from
[GitHub](https://github.com/) using the `pak` package:

``` r
# install.packages("pak")
pak::pak("kvantas/mapAI")
```

## Core Workflow: A Complete Example

This example demonstrates the primary workflow. We will first generate a
synthetic dataset representing a distorted map and then use the
package’s functions to correct it.

### 1. Load Libraries and Create Demo Data

We begin by using `create_demo_data()` to generate a test case with
complex, noisy distortions.

``` r
library(mapAI)
library(sf)
#> Linking to GEOS 3.13.1, GDAL 3.10.2, PROJ 9.5.1; sf_use_s2() is TRUE
library(ggplot2)

# Generate a shapefile and a GCPs CSV with complex noisy distortions
# The function returns a list containing the paths to these new files.
demo_data <- create_demo_data(type = "complex", seed = 1)
demo_data
#> $gcp
#> GCP Object with 225 points
#> Displaying first 10 points:
#>     source_x  source_y  target_x target_y         dx       dy
#> 1   1.691653 -2.105651  0.000000        0 -1.6916531 2.105651
#> 2   9.292090 -3.078302  7.142857        0 -2.1492333 3.078302
#> 3  15.978465 -3.618299 14.285714        0 -1.6927508 3.618299
#> 4  24.390552 -2.617043 21.428571        0 -2.9619808 2.617043
#> 5  30.954591 -2.481485 28.571429        0 -2.3831625 2.481485
#> 6  37.487039 -3.019571 35.714286        0 -1.7727530 3.019571
#> 7  43.693338 -4.710053 42.857143        0 -0.8361952 4.710053
#> 8  54.951007 -4.716323 50.000000        0 -4.9510072 4.716323
#> 9  60.221787 -1.969328 57.142857        0 -3.0789297 1.969328
#> 10 66.636189 -2.089572 64.285714        0 -2.3504744 2.089572
#> 
#> $map
#> Simple feature collection with 30 features and 1 field
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -0.179499 ymin: -4.716323 xmax: 103.3594 ymax: 114.0282
#> Projected CRS: WGS 84 / Pseudo-Mercator
#> First 10 features:
#>    id                           geom
#> 1   1 LINESTRING (1.691653 -2.105...
#> 2   2 LINESTRING (9.29209 -3.0783...
#> 3   3 LINESTRING (15.97847 -3.618...
#> 4   4 LINESTRING (24.39055 -2.617...
#> 5   5 LINESTRING (30.95459 -2.481...
#> 6   6 LINESTRING (37.48704 -3.019...
#> 7   7 LINESTRING (43.69334 -4.710...
#> 8   8 LINESTRING (54.95101 -4.716...
#> 9   9 LINESTRING (60.22179 -1.969...
#> 10 10 LINESTRING (66.63619 -2.089...
```

### 2. Read Data and Train a Model

We load the generated files and train a **Generalized Additive Model
(`gam`)**, which is ideal for capturing the smooth, non-linear
distortions present in the demo data.

``` r
# Load the homologous points (GCPs) and the distorted vector map
gcp_data <- demo_data$gcp
map_to_correct <- demo_data$map

# Train a bivariate GAM model using the GCPs
gam_model <- train_pai_model(gcp_data, "gam_biv")
#> Training Bivariate GAM model...
```

### 3. Apply Correction and Visualize

We apply the trained model to our distorted grid. The resulting plot,
which overlays the corrected grid on the original, provides a clear
visual confirmation of what the model does to the distorted map.

``` r
# Apply the model to the distorted map
corrected_map <- transform_map(gam_model, map_to_correct)
#> Applying PAI model to map features...
#> Correction complete.

# For easy plotting, add a 'status' column and combine the maps
map_to_correct$status <- "Original (Distorted)"
corrected_map$status <- "Corrected"
comparison_data <- rbind(map_to_correct[, "status"], corrected_map[, "status"])

# Create the final comparison plot
ggplot(comparison_data) +
  geom_sf(aes(color = status, linetype = status), fill = NA, linewidth = 0.7) +
  scale_color_manual(name = "Map Status", values = c("Original (Distorted)" = "grey50", "Corrected" = "#e41a1c")) +
  scale_linetype_manual(name = "Map Status", values = c("Original (Distorted)" = "dashed", "Corrected" = "solid")) +
  labs(title = "Positional Correction of a Distorted Grid",
       subtitle = "Overlay of original (dashed) and mapAI-corrected (solid) geometries") +
  theme_minimal()
```

<img src="man/figures/README-apply-and-visualize-1.png" width="100%" />

------------------------------------------------------------------------

## From Correction to Explanation: Advanced Distortion Analysis

A key challenge with data-driven models is understanding *what* they
have learned. `mapAI` directly addresses this by providing tools to
“open the black box” and analyze the properties of the learned
transformation.

### 4. Quantify and Visualize the Distortion Field

The `analyze_distortion()` function computes local distortion metrics
across the map space. This allows us to move from a simple visual
assessment to a quantitative map of the distortion.

``` r
# 1. Analyze the distortion using our trained GAM model
distortion_results <- analyze_distortion(gam_model, gcp_data)

# 2. Plot the distortion surfaces
plot(distortion_results,
                  metric = "log2_area_scale",
                  diverging = TRUE
) +
  labs(title = "Areal Distortion (log2σ)")
```

<img src="man/figures/README-advanced-analysis-1.png" width="100%" />

``` r

plot(
  distortion_results, metric = "max_shear"
) + 
  labs(title = "Maximum Shear Distortion (°)")
```

<img src="man/figures/README-advanced-analysis-2.png" width="100%" />

## Meta

- Bug reports, suggestions, and code are welcome.

- License:

  - All code is licensed MIT.
