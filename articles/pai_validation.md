# Comparing PAI Methods and Validation Strategies

This vignette is for users who are comfortable with the basic workflow
of `mapAI` and want to explore more advanced topics. Here, we will
address two key questions that arise in any real-world project:

1.  Which PAI model (`lm`, `rf`, or `gam`) is best for my data?
2.  How do I reliably estimate the accuracy of my chosen model?

To answer these, we will use a real-world dataset from the 1925 Kastoria
Cadastral Map, which is included with the package. We will focus on
comparing the performance of the three main correction pai_methods and
demonstrate the crucial difference between standard random
cross-validation and spatial cross-validation.

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
```

## Load and Explore the Package Data

The `mapAI` package comes with two datasets from the Kastoria study:

- `parcels`: An sf object of cadastral polygons from the 1925 map.
- `gcps`: An sf object of the homologous points (GCPs) for this map,
  including the calculated dx and dy displacements.

Since this data is already included in the package, we can load it
directly with the data() function.

``` r
# Load the built-in datasets
data(parcels)
data(gcps)

# Let's calculate the baseline error of the uncorrected map
# This "Identity RMSE" is our starting point.
identity_rmse <- sqrt(mean(gcps$dx^2 + gcps$dy^2))
print(paste("Initial average error (RMSE) of the Kastoria map:", round(identity_rmse, 2), "meters"))
#> [1] "Initial average error (RMSE) of the Kastoria map: 0.69 meters"
```

## Comparing Validation Strategies

A key challenge in spatial data science is **spatial autocorrelation**,
meaning that points closer together are more likely to be similar.
Standard **random k-fold cross-validation (CV)** ignores this, which can
lead to **optimistic** and misleadingly low error estimates because test
points might be too close to training points.

To address this, two distinct validation philosophies are often
considered:

------------------------------------------------------------------------

### Cross-Validation Strategies

- **Spatial Cross-Validation (SCV)** is one alternative to random CV. It
  creates geographically distinct folds, often using k-means clustering.
  While this avoids the optimism of random CV, it can provide a
  **pessimistic** estimate by testing the model’s ability to extrapolate
  into entirely new areas.

------------------------------------------------------------------------

### Design-Based Validation: A More Robust Assessment

More importantly, and in line with best practices for map accuracy
assessment, a **design-based validation** through a single train/test
split is often preferred. This approach simulates the evaluation of a
final model against a truly independent validation set, offering a more
statistically sound assessment of predictive accuracy. This package
implements this via: \* **Simple random sampling** for the split. \*
**Stratified random sampling**, which ensures the validation set
contains a representative sample of points across the full spectrum of
error magnitudes.

By offering this comprehensive suite of pai_methods (random CV, SCV, and
design-based validation), you can move beyond simply comparing
traditional CV strategies to perform a more robust assessment of your
model’s predictive accuracy.

Let’s compare the results from these pai_methods for all three models
(`lm`, `rf`, and `gam`).

``` r
# Helper function to run validation for all models
validate_all_pai_methods <- function(gcp_data, validation_type) {
  message(paste("\nRunning", validation_type, "cross-validation..."))
  
  # Validate each model type
  lm_results <- assess_pai_model(gcp_data, pai_method = "lm",k_folds = 10, seed = 1, validation_type = validation_type)
  rf_results <- assess_pai_model(gcp_data, pai_method = "rf",k_folds = 10, seed = 1, validation_type = validation_type)
  gam_results <- assess_pai_model(gcp_data, pai_method = "gam",k_folds = 10, seed = 1, validation_type = validation_type)
  hlm_results <- assess_pai_model(gcp_data, pai_method = "helmert",k_folds = 10, seed = 1, validation_type = validation_type)
  svm_results <- assess_pai_model(gcp_data, pai_method = "svmLinear",k_folds = 10, seed = 1, validation_type = validation_type)  
  tps_results <- assess_pai_model(gcp_data, pai_method = "tps",k_folds = 10, seed = 1, validation_type = validation_type)   
  # Combine and return the results
  rbind(lm_results, rf_results, gam_results, hlm_results, svm_results, tps_results)
}

# Run both random and spatial cross-validation
random_cv_results <- validate_all_pai_methods(gcps, "random")
spatial_cv_results <- validate_all_pai_methods(gcps, "spatial")
stratified_results <- validate_all_pai_methods(gcps, "stratified")

# Combine into one table for comparison
all_results <- rbind(random_cv_results, spatial_cv_results, stratified_results)

# Print the results table
knitr::kable(all_results, caption = "Comparison of Validation Results", digits = 2)
```

| Method    | ValidationType | Mean_RMSE_2D | SD_RMSE_2D |
|:----------|:---------------|-------------:|-----------:|
| lm        | random         |         0.52 |       0.05 |
| rf        | random         |         0.51 |       0.05 |
| gam       | random         |         0.50 |       0.04 |
| helmert   | random         |         0.54 |       0.05 |
| svmLinear | random         |         0.53 |       0.05 |
| tps       | random         |         0.52 |       0.05 |
| lm        | spatial        |         0.56 |       0.16 |
| rf        | spatial        |         0.64 |       0.16 |
| gam       | spatial        |         0.64 |       0.10 |
| helmert   | spatial        |         0.58 |       0.16 |
| svmLinear | spatial        |         0.56 |       0.17 |
| tps       | spatial        |         0.82 |       0.49 |
| lm        | stratified     |         0.52 |         NA |
| rf        | stratified     |         0.54 |         NA |
| gam       | stratified     |         0.53 |         NA |
| helmert   | stratified     |         0.52 |         NA |
| svmLinear | stratified     |         0.51 |         NA |
| tps       | stratified     |         0.60 |         NA |

Comparison of Validation Results

Now, let’s visualize these results to make the comparison clear.

``` r
ggplot(all_results, aes(x = Method, y = Mean_RMSE_2D, fill = ValidationType)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(ymin = Mean_RMSE_2D - SD_RMSE_2D, ymax = Mean_RMSE_2D + SD_RMSE_2D),
    width = 0.2, position = position_dodge(0.9)
  ) +
  labs(
    title = "Comparing Validation Results",
    subtitle = "Note how spatial CV provides a higher error estimate",
    x = "Correction Model",
    y = "Mean 2D RMSE (meters)",
    fill = "Validation Type"
  ) +
  theme_minimal()
```

![](reference/figures/valid_plot_results-1.png)

## Train the Final Model and Analyze It

Based on our validation, we choose `lm` as our final model and train it
on the **entire** set of GCPs.

``` r

# Train the final model on all available data
pai_model <- train_pai_model(gcps, pai_method = "lm")
#> Training 'lm' model...

# Let's look at the statistical summary of our trained model
print(summary(pai_model$model$model_dx))
#> 
#> Call:
#> stats::lm(formula = formula, data = data)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -1.27422 -0.18591  0.00183  0.15729  1.18893 
#> 
#> Coefficients:
#>               Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) -6.644e+03  7.461e+02  -8.904   <2e-16 ***
#> source_x     5.886e-05  1.373e-04   0.429    0.668    
#> source_y     1.477e-03  1.663e-04   8.880   <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.3619 on 297 degrees of freedom
#> Multiple R-squared:  0.2109, Adjusted R-squared:  0.2056 
#> F-statistic: 39.68 on 2 and 297 DF,  p-value: 5.333e-16
print(summary(pai_model$model$model_dy))
#> 
#> Call:
#> stats::lm(formula = formula, data = data)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -1.22190 -0.22767 -0.03835  0.20294  1.60099 
#> 
#> Coefficients:
#>               Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) -6.104e+03  7.848e+02  -7.778 1.22e-13 ***
#> source_x    -1.228e-03  1.444e-04  -8.505 9.02e-16 ***
#> source_y     1.434e-03  1.749e-04   8.196 7.53e-15 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.3806 on 297 degrees of freedom
#> Multiple R-squared:  0.3129, Adjusted R-squared:  0.3083 
#> F-statistic: 67.64 on 2 and 297 DF,  p-value: < 2.2e-16
```

## Apply and Visualize the Final Correction

Finally, we apply our chosen and analyzed gam model to the Kastoria
parcels data. We will plot the first five parcels to zoom in an area.

``` r
# Apply the model to the parcel polygons
corrected_parcels <- apply_pai_model(pai_model = pai_model, map = parcels)
#> Applying PAI model to map features...
#> Calculating area of corrected polygons...
#> Correction complete.

# Inspect the output - note the new 'area_new' column
print("Original vs. Corrected Dataframe Head:")
#> [1] "Original vs. Corrected Dataframe Head:"
head(corrected_parcels)
#> Simple feature collection with 6 features and 3 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 268541.9 ymin: 4488641 xmax: 268607.5 ymax: 4488718
#> Projected CRS: GGRS87 / Greek Grid
#>      KAK                       geometry        area_old        area_new
#> 1 151415 POLYGON ((268546.3 4488664,...  455.2082 [m^2]  455.8884 [m^2]
#> 2 151416 POLYGON ((268541.9 4488652,...  259.0621 [m^2]  259.4492 [m^2]
#> 3 151419 POLYGON ((268595.9 4488651,...  191.7725 [m^2]  192.0591 [m^2]
#> 4 151418 POLYGON ((268583.3 4488669,...  209.8621 [m^2]  210.1757 [m^2]
#> 5 151417 POLYGON ((268578.7 4488670,...  188.5859 [m^2]  188.8677 [m^2]
#> 6 151412 POLYGON ((268602.3 4488682,... 1352.2642 [m^2] 1354.2848 [m^2]

# Create the final comparison plot
ggplot() +
  geom_sf(data = parcels[1:5, ], aes(color = "Original"), fill = "grey75", linetype = "dashed") +
  geom_sf(data = corrected_parcels[1:5, ], aes(color = "Corrected"), fill = NA) +
  scale_color_manual(
    name = "Parcel Status",
    values = c("Original" = "grey25", "Corrected" = "#e41a1c")
  ) +
  labs(
    title = "Positional Correction of 1925 Kastoria Parcels",
    subtitle = "Overlay of original (dashed) and corrected (solid) polygons"
  ) +
    coord_sf(datum = sf::st_crs(2100)) +
  theme_minimal()
```

![](reference/figures/valid_visualize-1.png)

This final plot shows the tangible result of our work: the corrected
parcels are shifted and slightly reshaped, representing a more accurate
historical reality. This vignette has demonstrated how to move from a
basic workflow to a robust comparative analysis, giving you confidence
in both your choice of model and its final accuracy.
