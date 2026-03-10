# Read a Vector Map for Correction

Reads a vector map (e.g., shapefile) that is intended to be corrected.

## Usage

``` r
read_map(shp_path, ...)
```

## Arguments

- shp_path:

  A character string specifying the path to the input map file (e.g., a
  shapefile).

- ...:

  Arguments passed to
  [`sf::st_read`](https://r-spatial.github.io/sf/reference/st_read.html)

## Value

An `sf` object of the map to be corrected.

## Details

This function reads the geospatial file using
[`sf::st_read`](https://r-spatial.github.io/sf/reference/st_read.html).
If the file contains polygon geometries and does not already have an
area column named 'area_old', this function will calculate the area of
each feature and add it.

## Examples

``` r
if (FALSE) { # \dontrun{
# First, create a demo shapefile
demo_files <- create_demo_data()

# Read the map that needs correction
map_to_correct <- read_map(shp_path = demo_files$shp_path, quiet = TRUE)

# The output is now ready to be used in the `correct_map()` function
# after a model has been trained.
plot(sf::st_geometry(map_to_correct))
} # }
```
