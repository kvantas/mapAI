# Ground Control Points (GCPs) for Map Correction

A dataset containing Ground Control Points (GCPs) used for correcting
the vector map parcels of the package. These points represent homologous
locations identified on both the source (uncorrected) map and a modern
land survey.

## Usage

``` r
gcps
```

## Format

An `sf` object with 300 features and 6 variables:

- source_x:

  Numeric. The X-coordinate of the GCP on the source map.

- source_y:

  Numeric. The Y-coordinate of the GCP on the source map.

- target_x:

  Numeric. The X-coordinate of the GCP on the target map.

- target_y:

  Numeric. The Y-coordinate of the GCP on the target map.

- dx:

  Numeric. The difference in X-coordinates (target_x - source_x).

- dy:

  Numeric. The difference in Y-coordinates (target_y - source_y).

- geometry:

  `sfc_POINT`. The `sf` point geometry representing the GCPs in the
  target CRS (EPSG:2100).

## Source

Part of the dataset is used in the paper
https://doi.org/10.1111/tgis.70076

## Examples

``` r
if (interactive()) {
  # plot the difference in X-coordinates dx
  plot((gcps["dx"]))
}
```
