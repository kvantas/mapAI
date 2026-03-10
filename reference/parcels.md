# Vector Map Parcels for Correction

A dataset containing vector map parcels (polygons from a cadastral map)
that are intended to be corrected using geospatial transformation
methods.

## Usage

``` r
parcels
```

## Format

An `sf` object with 493 parcel polygon geometries and several variables:

- KAK:

  Numeric. A unique identifier for each parcel.

- area_old:

  `units` object. The original area of each polygon feature, calculated
  by
  [`read_map()`](https://kvantas.github.io/mapAI/reference/read_map.md)

- geometry:

  `sfc_POLYGON` or `sfc_MULTIPOLYGON`. The `sf` polygon geometry
  representing the parcels in the target CRS (EPSG:2100).

## Source

Part from the dataset of the 1925 urban cadastral map of Kastoria,
Greece as used in http://dx.doi.org/10.1111/tgis.70076

## Examples

``` r
if (interactive()) {
  plot(sf::st_geometry(parcels))
}
```
