# Read and Prepare Homologous Points (GCPs)

Reads a CSV file containing homologous points and prepares them for
modeling by calculating displacement vectors and creating an `sf`
object.

## Usage

``` r
read_gcps(gcp_path)
```

## Arguments

- gcp_path:

  A character string specifying the path with the file name to the CSV
  file of homologous points.

## Value

An `sf` object of the homologous points, with point geometries based on
the source coordinates and calculated `dx` and `dy` displacement
columns.

## Details

This function is the first step in the modeling workflow. It requires a
CSV file with four specific columns: `source_x` and `source_y`
(coordinates from the old/distorted map) and `target_x` and `target_y`
(coordinates from the reference/true map). It calculates the `dx` and
`dy` errors that the models will learn to predict.

## Examples

``` r
if (FALSE) { # \dontrun{
# First, create a demo CSV file
demo_files <- create_demo_data()

# Read the GCPs
gcp_data <- read_gcps(gcp_path = demo_files$gcp_path)

# The output is now ready for model training
print(gcp_data)
} # }
```
