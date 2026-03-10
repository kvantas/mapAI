# Internal Helmert 2D Transformation Solver

Calculates the four parameters of a 2D similarity (Helmert)
transformation using a standard, numerically stable least-squares
solution based on centroids. This is an internal function called by
`train_pai_model`.

## Usage

``` r
helmert(source_x, source_y, target_x, target_y)
```

## Arguments

- source_x:

  Numeric vector of approximate ('from') x coordinates.

- source_y:

  Numeric vector of approximate ('from') y coordinates.

- target_x:

  Numeric vector of actual ('to') x coordinates.

- target_y:

  Numeric vector of actual ('to') y coordinates.

## Value

An object of class `helmert` containing the calculated coefficients and
the original data centroids, ready for use by `predict.pai_model`.
