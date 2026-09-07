# Read a Map (Vector or Raster) for Correction

Reads a vector map (e.g., shapefile, GeoPackage) or a raster map (e.g.,
GeoTIFF) that is intended to be corrected.

## Usage

``` r
read_map(map_path, shp_path = NULL, ...)
```

## Arguments

- map_path:

  A character string specifying the path to the input map file (e.g., a
  shapefile or GeoTIFF).

- shp_path:

  An alias for `map_path` kept for backward compatibility.

- ...:

  Arguments passed to
  [`sf::st_read`](https://r-spatial.github.io/sf/reference/st_read.html)
  (for vector maps) or
  [`terra::rast`](https://rspatial.github.io/terra/reference/rast.html)
  (for raster maps).

## Value

An `sf` object (for vector maps) or a
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
object (for raster maps).

## Details

This function automatically detects whether the input file is a vector
dataset or a raster dataset:

- **Vector maps** (e.g., `.shp`, `.gpkg`, `.geojson`): Read using
  [`sf::st_read()`](https://r-spatial.github.io/sf/reference/st_read.html).
  If the file contains polygon geometries and lacks an `area_old`
  column, initial feature areas are calculated and added.

- **Raster maps** (e.g., `.tif`, `.tiff`, `.geotiff`, `.asc`, `.grd`):
  Read using
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html),
  returning a
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  object.

## Examples

``` r
if (FALSE) { # \dontrun{
# First, create demo files (creates both vector and raster maps)
demo_files <- create_demo_data()

# Read the vector map that needs correction
vector_map <- read_map(demo_files$shp_path, quiet = TRUE)

# Read the raster map that needs correction
raster_map <- read_map(demo_files$raster_path)
print(raster_map)
} # }
```
