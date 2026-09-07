# Write a Spatial Object (Vector or Raster) to a File

A robust and user-friendly wrapper around
[`sf::st_write`](https://r-spatial.github.io/sf/reference/st_write.html)
(for vector maps) and
[`terra::writeRaster`](https://rspatial.github.io/terra/reference/writeRaster.html)
(for raster maps) to save spatial objects to disk.

## Usage

``` r
write_map(map, file_path, overwrite = FALSE, ...)
```

## Arguments

- map:

  An `sf` object or a `terra` `SpatRaster` object to be written to a
  file.

- file_path:

  A character string specifying the path and filename for the output
  file (e.g., `"path/to/map.shp"` or `"path/to/raster.tif"`).

- overwrite:

  A logical value. If `TRUE`, it will overwrite an existing file at the
  specified path. Defaults to `FALSE`.

- ...:

  Additional arguments to be passed directly to
  [`sf::st_write`](https://r-spatial.github.io/sf/reference/st_write.html)
  or
  [`terra::writeRaster`](https://rspatial.github.io/terra/reference/writeRaster.html).

## Value

Invisibly returns the input `map` object, allowing it to be used in a
pipe chain.

## Details

This function provides a straightforward way to save the output of the
`mapAI` workflow (or any `sf` or
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
object) to a file on disk.

For vector data (`sf`), it supports any file format that
[`sf::st_write`](https://r-spatial.github.io/sf/reference/st_write.html)
can handle and automatically infers the driver from the file extension
(e.g., `.shp` -\> `"ESRI Shapefile"`, `.gpkg` -\> `"GPKG"`, `.geojson`
-\> `"GeoJSON"`).

For raster data
([`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)),
it writes the raster using
[`terra::writeRaster`](https://rspatial.github.io/terra/reference/writeRaster.html),
supporting formats such as GeoTIFF (`.tif`), GPKG raster (`.gpkg`), and
others.

For safety, the function defaults to `overwrite = FALSE`, which will
prevent accidentally overwriting an existing file. Advanced users can
pass additional arguments directly to the underlying writing functions
([`sf::st_write`](https://r-spatial.github.io/sf/reference/st_write.html)
or
[`terra::writeRaster`](https://rspatial.github.io/terra/reference/writeRaster.html))
via the `...` parameter.

## Examples

``` r
if (FALSE) { # \dontrun{
# --- 1. Write an sf vector object ---
data(parcels)
sample_map <- parcels[1:5, ]
output_shp <- tempfile(fileext = ".shp")
write_map(sample_map, output_shp, overwrite = TRUE)

# --- 2. Write a SpatRaster object ---
library(terra)
r <- rast(nrows = 10, ncols = 10, vals = 1:100)
output_tif <- tempfile(fileext = ".tif")
write_map(r, output_tif, overwrite = TRUE)
} # }
```
