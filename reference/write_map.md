# Write a Spatial Object to a File

A robust and user-friendly wrapper around
[`sf::st_write`](https://r-spatial.github.io/sf/reference/st_write.html)
to save an `sf` object, for example, a corrected map.

## Usage

``` r
write_map(map, file_path, overwrite = FALSE, ...)
```

## Arguments

- map:

  An `sf` object to be written to a file.

- file_path:

  A character string specifying the path and filename for the output
  file (e.g., `"path/to/my_corrected_map.shp"`).

- overwrite:

  A logical value. If `TRUE`, it will overwrite an existing file at the
  specified path. Defaults to `FALSE`.

- ...:

  Additional arguments to be passed directly to
  [`sf::st_write`](https://r-spatial.github.io/sf/reference/st_write.html)
  (e.g., `layer_options`).

## Value

Invisibly returns the input `map` object, allowing it to be used in a
pipe chain.

## Details

This function provides a straightforward way to save the output of the
`mapAI` workflow (or any `sf` object) to a file on disk. It supports any
file format that
[`sf::st_write`](https://r-spatial.github.io/sf/reference/st_write.html)
can handle. The function automatically infers the correct driver from
the file extension (e.g., `.shp` -\> `"ESRI Shapefile"`, `.gpkg` -\>
`"GPKG"`).

For safety, the function defaults to `overwrite = FALSE`, which will
prevent accidentally overwriting an existing file. Advanced users can
pass additional arguments directly to
[`sf::st_write`](https://r-spatial.github.io/sf/reference/st_write.html)
via the `...` parameter.

## Examples

``` r
if (FALSE) { # \dontrun{
# This example shows how to save data and ensure all created files are removed.

# --- 1. Create a sample sf object to write ---
data(parcels)
sample_map <- parcels[1:5, ]

# --- 2. Define a temporary file path ---
# Using tempfile() is best practice for examples.
output_path <- tempfile(fileext = ".shp")

# --- 3. Write the map to the file ---
write_map(sample_map, output_path, overwrite = TRUE)

# --- 4. Clean up ALL created files ---
# A shapefile creates multiple "sidecar" files (.dbf, .shx, .prj, etc.).
# This code finds all files with the same base name and removes them.
base_name <- tools::file_path_sans_ext(output_path)
files_to_remove <- list.files(dirname(base_name),
                              pattern = basename(base_name),
                              full.names = TRUE)
file.remove(files_to_remove)

# Check that the files are gone
print(list.files(dirname(output_path), pattern = basename(base_name)))
} # }
```
