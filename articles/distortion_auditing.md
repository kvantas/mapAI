# Model-Agnostic Distortion Diagnostics for Positional Accuracy Improvement

## 1. Introduction: The High-Stakes Dilemma of Vector Rectification

Historical cartography, legacy cadastral surveys, and administrative
vector datasets provide irreplaceable baselines for analyzing long-term
environmental, geomorphic, and land-use change. In historical geographic
information systems (HGIS) and landscape ecology, researchers frequently
synthesize these historical geometries with modern basemaps to measure
phenomena such as:

- Decadal wetland contraction and drainage,
- Centennial forest fragmentation and agricultural expansion,
- River channel migration and coastal erosion, and
- Cadastral boundary evolution and land tenure stability.

To align historical layers with modern coordinate reference systems
(CRS), **Positional Accuracy Improvement (PAI)** methods are essential.
Because historical maps suffer from non-linear paper shrinkage,
composite surveying campaigns, and unrecorded projection variations,
global rigid transformations (e.g., Helmert or affine) are rarely
sufficient. Consequently, researchers increasingly deploy flexible
spatial transformations and machine learning learners, such as Thin
Plate Splines (TPS), Generalized Additive Models (GAMs), and Support
Vector Machines (SVMs).

### The Danger: Transformation Artifacts Mimic Real Landscape Signals

Flexible non-linear models locally warp coordinate space to eliminate
positional discrepancies at known control points. However, in the
unmonitored space between control points, flexible transformations risk
introducing severe **non-conformal stretching, local areal inflation or
deflation, and angular shear**.

Crucially, in a landscape change study, **a spatially-varying
transformation artifact is visually and statistically indistinguishable
from a true environmental signal**. For instance:

- If an unconstrained model artificially compresses local area by 30% to
  fit adjacent control points, a researcher measuring parcel or wetland
  dynamics will erroneously report a **30% real-world habitat loss**.
- If a model introduces local angular shear, orthogonal historical
  structures and straight river channels will falsely appear bent or
  deformed.

### The Blind Spot of Standard Metrics

Conventional spatial validation relies on point-based summary metrics:
Root Mean Square Error (RMSE), Mean Absolute Error (MAE), and spatial
$`k`$-fold cross-validation. While these metrics assess how well the
model predicts target coordinates at discrete points, **they provide
zero information about whether the vector fabric between points was
compressed, rotated, or sheared**. A model can achieve an impressively
low cross-validation RMSE while severely tearing or squishing the
continuous geometry of the landscape.

### The Methodological Barrier: Generalizing Beyond Thin Plate Splines

Differential distortion diagnostics based on **Tissot’s indicatrix
theory** provide the mathematical solution to this dilemma by
quantifying local scale factor, areal deformation, and angular shear.
However, since the foundational work of Jenny & Hurni (2011) and Claeys
Boùùaert et al. (2016), such diagnostics have remained tethered to
**closed-form analytical solutions derived strictly for Thin Plate
Splines**.

Analytical partial derivatives cannot be readily obtained for black-box
or non-parametric machine learning models (e.g., penalized spline GAMs,
random forests, neural regressors). The `mapAI` package breaks this
15-year dependency by introducing a **model-agnostic numerical Jacobian
estimation engine**. By decoupling distortion analysis from analytical
calculus, `mapAI` enables rigorous distortion auditing for *any*
differentiable spatial transformation.

------------------------------------------------------------------------

## 2. Mathematical Framework: Numerical Jacobian and Tissot’s Indicatrix

Let $`f: \mathbb{R}^2 \to \mathbb{R}^2`$ represent any continuous,
trained spatial transformation mapping source coordinates $`(x, y)`$ to
predicted target coordinates $`(x', y')`$:

``` math
\begin{pmatrix} x' \\ y' \end{pmatrix} = f(x, y) = \begin{pmatrix} f_x(x, y) \\ f_y(x, y) \end{pmatrix}
```

The local behavior of $`f`$ around any point $`(x, y)`$ is governed by
its differential linearization, captured by the **Jacobian matrix**
$`\mathbf{J}`$:

``` math
\mathbf{J}(x, y) = \begin{pmatrix} \frac{\partial x'}{\partial x} & \frac{\partial x'}{\partial y} \\ \frac{\partial y'}{\partial x} & \frac{\partial y'}{\partial y} \end{pmatrix} = \begin{pmatrix} a_{11} & a_{12} \\ a_{21} & a_{22} \end{pmatrix}
```

### Numerical Jacobian Estimation

Rather than requiring an analytical derivation of $`f`$,
[`mapAI::analyze_distortion()`](https://kvantas.github.io/mapAI/reference/analyze_distortion.md)
computes $`\mathbf{J}`$ using **central finite differences**:

``` math
\frac{\partial x'}{\partial x} \approx \frac{f_x(x + h, y) - f_x(x - h, y)}{2h}, \quad \frac{\partial x'}{\partial y} \approx \frac{f_x(x, y + h) - f_x(x, y - h)}{2h}
```

``` math
\frac{\partial y'}{\partial x} \approx \frac{f_y(x + h, y) - f_y(x - h, y)}{2h}, \quad \frac{\partial y'}{\partial y} \approx \frac{f_y(x, y + h) - f_y(x, y - h)}{2h}
```

where $`h`$ is an infinitesimal perturbation step (defaulting to
$`10^{-4}`$ in projected coordinate units). Because this formulation
queries $`f`$ strictly as a callable coordinate mapping, it is fully
model-agnostic.

### Derivation of Principal Deformation Semi-Axes

Following Tissot’s theorem and the singular value decomposition of
$`\mathbf{J}`$, an infinitesimal circle on the source plane deforms into
an ellipse on the target plane. The principal semi-major axis $`a`$
(maximum local scale factor) and semi-minor axis $`b`$ (minimum local
scale factor) are given by:

``` math
a = \frac{1}{2} \sqrt{(a_{11} + a_{22})^2 + (a_{21} - a_{12})^2} + \frac{1}{2} \sqrt{(a_{11} - a_{22})^2 + (a_{21} + a_{12})^2}
```

``` math
b = \frac{1}{2} \left| \sqrt{(a_{11} + a_{22})^2 + (a_{21} - a_{12})^2} - \sqrt{(a_{11} - a_{22})^2 + (a_{21} + a_{12})^2} \right|
```

### Operational Distortion Metrics

From $`a`$ and $`b`$, `mapAI` computes three core diagnostic metrics
across the study domain:

1.  **Areal Scale Factor ($`\sigma`$)**:
    ``` math
    \sigma = a \cdot b = \det(\mathbf{J}) = a_{11} a_{22} - a_{12} a_{21}
    ```
    - Interpretation: $`\sigma = 1`$ indicates local area preservation
      (equivalent mapping). Values of $`\sigma > 1`$ indicate artificial
      local expansion, while $`\sigma < 1`$ indicates artificial local
      compression.
2.  **Maximum Angular Distortion ($`2\Omega`$)**:
    ``` math
    2\Omega = 2 \arcsin\left(\frac{a - b}{a + b}\right)
    ```
    - Interpretation: Quantifies local shear in radians (or degrees). A
      value of $`2\Omega = 0`$ indicates local conformality (shape
      preservation; right angles remain orthogonal).
3.  **Airy-Kavrayskiy Combined Metric ($`E_{AK}`$)**:
    ``` math
    E_{AK} = \frac{(a - 1)^2 + (b - 1)^2}{2}
    ```
    - Interpretation: A composite index penalizing both areal and
      angular deviation from an ideal isometric transformation
      ($`a = b = 1`$).

------------------------------------------------------------------------

## 3. Comparative Context: Desktop GIS and MapAnalyst

Before running the workflow, it is instructive to compare how vector
adjustment is handled across existing GIS platforms:

| Feature / Capability | ArcGIS Pro (Spatial Adjustment) | QGIS (Vector Bender) | MapAnalyst | `mapAI` Package |
|:---|:---|:---|:---|:---|
| **Adjustment Paradigms** | Piecewise Linear TIN / Natural Neighbor rubbersheet | Piecewise Linear TIN | Rigid Affine / Thin Plate Splines | Unified: Geodetic, Regularized Splines (TPS, GAM), and ML |
| **Mathematical Continuity** | $`C^0`$ (Linear TIN kinks) or $`C^1`$ | $`C^0`$ (Linear TIN kinks) | $`C^2`$ (Analytical TPS) | $`C^2`$ to $`C^\infty`$ (Smooth regularized splines) |
| **Point Accuracy Metrics** | Link Table ($`dx, dy`$) & RMSE | Point residual vectors | Residual vectors & RMSE | Design-based sampling & Spatial cross-validation |
| **Continuous Distortion Diagnostics** | **None** | **None** | **TPS-Only** (Analytical isolines & grids) | **Model-Agnostic** ($`\sigma, 2\Omega, E_{AK}`$ surfaces via numerical Jacobian) |
| **Vector Geometry Mutation** | Interactive manual session | Interactive manual session | **None** (Diagnostic-only; cannot transform vectors) | Programmatic: recursive vertex mutation across all `sf` types |

- **The Desktop GIS Limitation**: ArcGIS Pro and QGIS use Triangulated
  Irregular Networks (TINs) for rubbersheeting. Linear TIN facets
  exhibit $`C^0`$ continuity: derivatives are discontinuous along
  triangle boundaries, introducing artificial sharp kinks into
  continuous linear features (roads, rivers, parcel boundaries).
  Furthermore, they report only discrete link residuals, leaving users
  blind to inter-point fabric distortion.
- **The MapAnalyst Limitation**: MapAnalyst (Jenny & Hurni, 2011)
  pioneered distortion visualization for historical maps. However, it is
  **strictly diagnostic**: it cannot ingest and transform vector feature
  layers (`sf` objects) into analysis-ready GIS layers, and its
  mathematical formulation is tied exclusively to analytical Thin Plate
  Splines.

------------------------------------------------------------------------

## 4. Empirical Workflow: Auditing Cadastral Vector Renewal (1798 Swiss Dataset)

We demonstrate this methodology using the built-in `swiss_cps` dataset:
343 homologous ground control points from Wilhelm Haas’s 1798 cadastral
map of Basel and Frickthal, aligned with the modern Swiss national
coordinate reference system (CH1903 / LV03).

### Setup and Data Inspection

``` r

library(mapAI)
library(sf)
library(dplyr)
library(ggplot2)
library(knitr)

# Load the historical Swiss control points dataset
data(swiss_cps)

# Inspect dataset
head(swiss_cps, 3)
#> Simple feature collection with 3 features and 7 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 612225 ymin: 267353.6 xmax: 615470.3 ymax: 270412.7
#> Projected CRS: CH1903 / LV03
#>   Index source_x source_y target_x target_y        dx        dy
#> 1     1 612293.0 267353.6 611375.9 267719.1 -917.1063 365.46825
#> 2     2 612225.0 270412.7 611573.1 270370.6 -651.9320 -42.05086
#> 3     3 615470.3 270195.1 615840.8 270463.6  370.4737 268.47820
#>                    geometry
#> 1   POINT (612293 267353.6)
#> 2   POINT (612225 270412.7)
#> 3 POINT (615470.3 270195.1)
```

We begin by visualizing the raw residual displacement vectors:

``` r

plot_displacement(swiss_cps) +
  labs(
    title = "Initial Displacement Vectors (Basel and Frickthal, 1798)",
    x = "x (m)",
    y = "y (m)"
  )
```

![](reference/figures/dist_displacement_plot-1.png)

The displacement vectors reveal systematic, spatially clustered errors,
particularly in the southeastern quadrant, confirming that a non-linear
approach is required.

------------------------------------------------------------------------

### The Illusion of Point Accuracy: Model Validation

Before evaluating fabric distortion, we train a **Generalized Additive
Model (`gam`)** with penalized thin-plate regression splines and assess
its point-level predictive accuracy using both spatial cross-validation
and design-based probability sampling:

``` r

# 5-fold spatial cross-validation
cv_spatial <- assess_pai_model(
  swiss_cps,
  pai_method = "gam",
  validation_type = "spatial",
  k_folds = 5,
  seed = 42
)

# Design-based validation using simple random sampling
val_prob <- assess_pai_model(
  swiss_cps,
  pai_method = "gam",
  validation_type = "probability",
  seed = 42
)

# Design-based validation using stratified random sampling
val_strat <- assess_pai_model(
  swiss_cps,
  pai_method = "gam",
  validation_type = "stratified",
  seed = 42
)

# Combine into a summary table
val_summary <- data.frame(
  Strategy = c(
    "Spatial 5-Fold Cross-Validation",
    "Design-Based (Simple Random Sample)",
    "Design-Based (Stratified Sample)"
  ),
  Mean_RMSE_2D_m = round(c(cv_spatial$Mean_RMSE_2D, val_prob$Mean_RMSE_2D, val_strat$Mean_RMSE_2D), 1)
)

kable(val_summary, caption = "Table 1: GAM Point Predictive Accuracy Metrics.")
```

| Strategy                            | Mean_RMSE_2D_m |
|:------------------------------------|---------------:|
| Spatial 5-Fold Cross-Validation     |         1352.7 |
| Design-Based (Simple Random Sample) |          615.1 |
| Design-Based (Stratified Sample)    |          789.2 |

Table 1: GAM Point Predictive Accuracy Metrics. {.table}

Under standard GIS reporting criteria, the model appears well-behaved:
residual RMSE is substantially reduced across all validation schemes.

------------------------------------------------------------------------

### The Diagnostic Unmasking: Continuous Distortion Auditing

Now, we train the final model on the full dataset and deploy
[`analyze_distortion()`](https://kvantas.github.io/mapAI/reference/analyze_distortion.md)
across a regular grid over the study area:

``` r

# Train the final GAM model
final_gam <- train_pai_model(swiss_cps, pai_method = "gam")

# Construct a regular evaluation grid
eval_grid <- sf::st_make_grid(swiss_cps, n = c(40, 40)) %>%
  sf::st_centroid() %>%
  sf::st_sf()

# Perform model-agnostic distortion analysis via numerical Jacobian
distortion_field <- analyze_distortion(final_gam, eval_grid)
```

#### 1. Areal Scale Surface ($`\sigma`$)

``` r

plot_distortion_surface(distortion_field, "area_scale", diverging = TRUE) +
  labs(
    title = expression(paste("Areal Scale Factor (", sigma, ")")),
    subtitle = expression(paste(sigma, " > 1: Expansion; ", sigma, " < 1: Compression")),
    x = "x (m)",
    y = "y (m)",
    fill = expression(sigma)
  )
```

![](reference/figures/dist_surface_area-1.png)

The areal scale surface uncovers severe localized deformation: \* The
center-east experiences artificial dilation up to $`\sigma = 1.41`$ (a
**41% artificial expansion**). \* The southeast experiences extreme
compression down to $`\sigma = 0.45`$ (a **55% artificial shrinkage**).

#### 2. Maximum Angular Distortion Surface ($`2\Omega`$)

``` r

plot_distortion_surface(distortion_field, "max_angular_distortion", palette = "magma") +
  labs(
    title = expression(paste("Maximum Angular Shear (2", Omega, ")")),
    subtitle = "Zero indicates conformality; high values reveal non-orthogonal shearing",
    x = "x (m)",
    y = "y (m)",
    fill = "2Ω (rad)"
  )
```

![](reference/figures/dist_surface_shear-1.png)

Angular shearing reaches up to $`0.86\text{ rad}`$ ($`49.3^\circ`$),
concentrated in the southeastern quadrant. This confirms that orthogonal
angles in historical structures would be distorted into acute or obtuse
angles by the transformation.

#### 3. Airy-Kavrayskiy Combined Metric ($`E_{AK}`$)

``` r

plot_distortion_surface(distortion_field, "airy_kavrayskiy") +
  labs(
    title = expression(paste("Airy-Kavrayskiy Metric (", E[AK], ")")),
    subtitle = "Combined index of total geometric distortion",
    x = "x (m)",
    y = "y (m)",
    fill = expression(E[AK])
  )
```

![](reference/figures/dist_surface_airy-1.png)

#### 4. Tissot Indicatrices at Control Points

We can also inspect Tissot’s indicatrices directly at the control point
locations:

``` r

distortion_gcps <- analyze_distortion(final_gam, points_to_analyze = swiss_cps)

plot_indicatrices(distortion_gcps, scale_factor = 700) +
  labs(
    title = "Tissot Indicatrices at Ground Control Points",
    subtitle = "Circular = isometric; Eccentric ellipses = severe local shear and scaling",
    x = "x (m)",
    y = "y (m)"
  )
```

![](reference/figures/dist_indicatrices_plot-1.png)

In the center and northwest, the indicatrices are nearly circular
($`a \approx b \approx 1`$), confirming shape and area fidelity. In the
southeast, the indicatrices flatten into highly eccentric ellipses
aligned with the direction of maximum shear.

#### Numerical Summary of Distortion

``` r

summary_tbl <- sf::st_drop_geometry(distortion_field) %>%
  reframe(
    Metric = c("Areal Scale Factor (σ)", "Max Angular Shear 2Ω (rad)", "Airy-Kavrayskiy Metric (E_AK)"),
    Minimum = round(c(min(area_scale), min(max_angular_distortion), min(airy_kavrayskiy)), 2),
    Mean = round(c(mean(area_scale), mean(max_angular_distortion), mean(airy_kavrayskiy)), 2),
    Maximum = round(c(max(area_scale), max(max_angular_distortion), max(airy_kavrayskiy)), 2)
  )

kable(summary_tbl, caption = "Table 2: Continuous Distortion Summary Statistics (GAM Model).")
```

| Metric                        | Minimum | Mean | Maximum |
|:------------------------------|--------:|-----:|--------:|
| Areal Scale Factor (σ)        |    0.45 | 0.87 |    1.41 |
| Max Angular Shear 2Ω (rad)    |    0.00 | 0.21 |    0.86 |
| Airy-Kavrayskiy Metric (E_AK) |    0.00 | 0.04 |    0.28 |

Table 2: Continuous Distortion Summary Statistics (GAM Model). {.table}

------------------------------------------------------------------------

## 5. Operational Vector Renewal & Scientific Safeguards

### Mutating Vector Features: `apply_pai_model`

Unlike MapAnalyst, `mapAI` actively applies the learned transformation
to vector geometries. The
[`apply_pai_model()`](https://kvantas.github.io/mapAI/reference/apply_pai_model.md)
function traverses every vertex of simple feature objects (`POINT`,
`LINESTRING`, `POLYGON`, `MULTIPOLYGON`):

``` r

# Create a regular line grid to illustrate geometry transformation
sample_grid <- sf::st_make_grid(swiss_cps, n = c(12, 12)) %>%
  sf::st_cast("MULTILINESTRING") %>%
  sf::st_sf()

# Apply the trained GAM model
corrected_grid <- apply_pai_model(final_gam, sample_grid)

# Plot comparison
sample_grid$Status <- "Original Grid"
corrected_grid$Status <- "GAM Corrected"
grid_comp <- rbind(sample_grid[, "Status"], corrected_grid[, "Status"])

ggplot(grid_comp) +
  geom_sf(aes(color = Status, linetype = Status), linewidth = 0.6) +
  scale_color_manual(values = c("Original Grid" = "grey50", "GAM Corrected" = "#d95f02")) +
  scale_linetype_manual(values = c("Original Grid" = "dashed", "GAM Corrected" = "solid")) +
  theme_minimal() +
  labs(
    title = "Continuous Fabric Transformation (Regular Grid Warping)",
    x = "x (m)",
    y = "y (m)"
  )
```

![](reference/figures/dist_apply_model-1.png)

We confirm residual behavior by plotting post-correction residuals:

``` r

plot_residuals(final_gam, swiss_cps) +
  labs(
    title = "Post-Correction Residual Vectors",
    x = "x (m)",
    y = "y (m)"
  )
```

![](reference/figures/dist_residual_plot-1.png)

### The Geoscientific Takeaway: Avoiding False Change Detection

The southeastern anomaly in the 1798 Basel map is a documented
cartographic artifact—likely caused by Wilhelm Haas relying on older,
unverified surveys for that quadrant.

Had a researcher applied an un-audited GAM transformation to measure
200-year land-use change in the southeast, the model’s **55% artificial
compression ($`\sigma = 0.45`$)** would have generated massive **phantom
deforestation or parcel shrinkage signals**. Standard RMSE metrics never
warned the analyst because the GAM fitted the control points accurately.

### The 3-Step Distortion-Audited Protocol

To ensure data integrity, we recommend the following protocol when
renewing legacy vector data for scientific reuse:

1.  **Candidate Modeling & Point Assessment**: Train candidate models
    (Helmert, TPS, GAM, ML) and calculate unbiased out-of-sample RMSE
    using spatial cross-validation or design-based probability sampling.
2.  **Fabric Distortion Auditing**: Query the model across a regular
    evaluation grid using
    [`analyze_distortion()`](https://kvantas.github.io/mapAI/reference/analyze_distortion.md).
    Inspect surfaces for areal deformation $`\sigma`$ and angular shear
    $`2\Omega`$. If local scaling exceeds project tolerance thresholds
    (e.g., $`|\sigma - 1| > 0.15`$), increase model regularization or
    collect additional control points.
3.  **Safe Vector Renewal**: Once the continuous fabric integrity is
    verified, execute
    [`apply_pai_model()`](https://kvantas.github.io/mapAI/reference/apply_pai_model.md)
    to transform the vector layers for downstream environmental change
    detection.

------------------------------------------------------------------------

## References

- Claeys Boùùaert, M., De Sloover, L., & De Maeyer, P. (2016).
  Cartographic deformation analysis of historical maps using thin-plate
  splines. *Cartography and Geographic Information Science*, 43(4),
  356–368.
- Jenny, B., & Hurni, L. (2011). Studying cartographic heritage:
  Analysis and visualization of geometric distortions. *Computers &
  Geosciences*, 37(1), 71–81.
- Laskowski, P. C. (1989). The traditional and modern look at Tissot’s
  indicatrix. *The American Cartographer*, 16(2), 123–133.
- Wadoux, A. M. J. C., Brus, D. J., & Heuvelink, G. B. M. (2021).
  Sampling design for validating digital soil maps: A tutorial.
  *Geoderma*, 388, 114923.
