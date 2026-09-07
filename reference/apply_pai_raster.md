# Apply a Trained PAI Model to Correct a Raster Map

Applies a trained `pai_model` object to a `terra` `SpatRaster`,
rectifying its spatial alignment based on the learned transformation.

## Usage

``` r
apply_pai_raster(
  pai_model,
  raster,
  method = c("bilinear", "near", "simple"),
  res = NULL,
  mesh_step = NULL,
  aoi = NULL,
  ...
)
```

## Arguments

- pai_model:

  An object of class `pai_model` returned by
  [`train_pai_model()`](https://kvantas.github.io/mapAI/reference/train_pai_model.md).

- raster:

  A `terra` `SpatRaster` object, or a character string specifying the
  file path to a readable raster file.

- method:

  A character string specifying the interpolation method. One of:
  `"bilinear"` (default, for continuous data) or `"near"` / `"simple"`
  (nearest neighbor, for categorical data).

- res:

  An optional numeric vector of length 1 or 2 specifying the target cell
  resolution. If `NULL` (default), the resolution of `raster` is
  preserved.

- mesh_step:

  An optional positive integer specifying the subsampling step for grid
  mesh interpolation. If `NULL` (default) or `1`, displacements are
  computed directly for each cell center.

- aoi:

  An optional `sf` or `terra` `SpatVector` polygon object representing
  an Area of Interest. If provided, cells outside the AOI are masked to
  `NA`.

- ...:

  Additional arguments passed on to
  [`predict.pai_model()`](https://kvantas.github.io/mapAI/reference/predict.pai_model.md).

## Value

A `terra` `SpatRaster` object with corrected spatial alignment.

## Details

This function brings the Positional Accuracy Improvement (PAI) framework
to raster geospatial data (e.g., scanned historical maps, aerial
imagery, digital elevation models, or thematic raster grids).

**Forward vs. Inverse Mapping in Raster Warping:**

While vector PAI shifts existing vertices forward (\\(x, y) \to (x + dx,
y + dy)\\), raster data exists on a rigid, regular grid. Applying
forward mapping to raster pixels would result in an irregular point
cloud with gaps ("holes") in expanded areas and collisions in compressed
areas.

To produce a valid, regular raster grid, `apply_pai_raster` employs
**inverse (backward) mapping**:

1.  Defines the regular coordinate grid for the target (corrected)
    raster.

2.  Projects each target cell center backwards through the
    transformation to find its corresponding location in the source
    (distorted) image.

3.  Resamples the pixel value from the source raster using the requested
    interpolation method.

**Model Inversion Handling:**

- **Models trained with `direction = "inverse"`:** Directly evaluate the
  displacement from target to source coordinates in a single step.

- **Helmert models (`direction = "forward"`):** Uses the exact
  closed-form analytical inverse of the 4-parameter similarity
  transformation.

- **Non-linear models (`gam`, `tps`, `rf`, `svmRadial`, `lm`):** Employs
  a rapid iterative fixed-point backward mapping algorithm that
  converges to sub-millimeter precision within 2 iterations.

**Interpolation Methods:**

- **"bilinear" (Default):** Bilinear interpolation. Recommended for
  continuous rasters such as elevation models, satellite/aerial imagery,
  and scanned paper maps.

- **"near" or "simple":** Nearest-neighbor assignment. Essential for
  discrete, integer, or categorical rasters (e.g., land cover
  classification, soil classes, or parcel IDs) to ensure original class
  values are preserved without generating non-existent intermediate
  values.

**Performance Optimization (`mesh_step`):**

Evaluating non-linear models (like GAM or Random Forest) individually
across millions of raster cells can be computationally demanding. By
setting `mesh_step` (e.g., 10 or 20), `apply_pai_raster` computes exact
displacements on a regular subsampled grid mesh and uses
high-performance C++ bilinear interpolation
([`terra::resample`](https://rspatial.github.io/terra/reference/resample.html))
to reconstruct the continuous displacement field across the full raster
grid. This provides orders-of-magnitude speedups while maintaining
sub-pixel accuracy.

## Examples

``` r
library(terra)
#> terra 1.9.46
#> 
#> Attaching package: ‘terra’
#> The following objects are masked from ‘package:magrittr’:
#> 
#>     extract, inset

# Load built-in homologous points
data(gcps)

# Train a GAM PAI model
gam_model <- train_pai_model(gcps, pai_method = "gam")
#> Training 'gam' model...

# Create a small synthetic raster matching the GCP extent
gcp_ext <- terra::ext(
  min(gcps$source_x), max(gcps$source_x),
  min(gcps$source_y), max(gcps$source_y)
)
demo_rast <- terra::rast(gcp_ext, nrows = 30, ncols = 30, crs = sf::st_crs(gcps)$wkt)
terra::values(demo_rast) <- seq_len(terra::ncell(demo_rast))

# Apply the model to correct the raster
corrected_rast <- apply_pai_raster(gam_model, demo_rast, method = "bilinear")
#> Applying PAI model to raster...
#> Correction complete.
print(corrected_rast)
#> class       : SpatRaster
#> size        : 30, 30, 1  (nrow, ncol, nlyr)
#> resolution  : 20.1864, 17.02769  (x, y)
#> extent      : 268456, 269061.6, 4488191, 4488702  (xmin, xmax, ymin, ymax)
#> coord. ref. : GGRS87 / Greek Grid (EPSG:2100)
#> source(s)   : memory
#> name        :      lyr.1
#> min value   :    1.36902
#> max value   : 897.841958
```
