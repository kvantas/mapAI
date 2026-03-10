# Predict Method for pai_model Objects

Predicts spatial corrections (dx, dy) from a trained `pai_model` object.
This is an S3 method for the generic
[`predict()`](https://rdrr.io/r/stats/predict.html) function.

## Usage

``` r
# S3 method for class 'pai_model'
predict(object, newdata, ...)
```

## Arguments

- object:

  A trained model object of class `pai_model` returned by
  [`train_pai_model()`](https://kvantas.github.io/mapAI/reference/train_pai_model.md).

- newdata:

  A `data.frame` with `source_x` and `source_y` columns for which to
  generate predictions.

- ...:

  Additional arguments passed on to the underlying predict methods
  (e.g., `predict.lm`, `predict.gam`, `predict.svm`).

## Value

A `data.frame` with predicted `dx` and `dy` columns, having the same
number of rows as `newdata`.

## Details

This function provides the core prediction logic for all models created
by
[`train_pai_model()`](https://kvantas.github.io/mapAI/reference/train_pai_model.md).
As an S3 method, it should not be called directly (e.g.,
`predict.pai_model(...)`), but rather through the generic
[`predict()`](https://rdrr.io/r/stats/predict.html) function (e.g.,
`predict(my_model, ...)`).

Key features of this method include:

- **Automatic Model Handling:** It transparently handles the different
  output structures of `helmert`,`tps`, `gam`, `lm`, `rf`, `svmRadial`
  and `svmLinear` models, always returning a consistent `data.frame`.

- **Robust NA Handling:** It correctly handles `NA` values in the
  `newdata` predictors. Rows with `NA` inputs will produce `NA` outputs,
  ensuring the output has the same number of rows as the input and
  preventing errors from underlying prediction functions.

## Examples

``` r
if (FALSE) { # \dontrun{
# This example shows how the generic `predict()` function can be used
# on any model trained by `train_pai_model()`.

# --- 1. Load Data and Train Models ---
data(gcps) # Load the package's built-in homologous points

# Train two different types of models
pai_model_gam <- train_pai_model(gcps, pai_method = "gam")
pai_model_rf <- train_pai_model(gcps, pai_method = "rf")

# --- 2. Create New Data for Prediction ---
# We'll create a small data frame of new points.
# Note the third row contains an NA to demonstrate robust NA handling.
new_points_to_predict <- data.frame(
  source_x = c(241643.0, 241650.0, NA),
  source_y = c(4477383, 4477370, 4477390)
)

# --- 3. Use the Generic `predict()` Function ---
# The same `predict()` call works for both model objects.

# Predict using the GAM model
predictions_from_gam <- predict(pai_model_gam, newdata = new_points_to_predict)

# Predict using the Random Forest model
predictions_from_rf <- predict(pai_model_rf, newdata = new_points_to_predict)

# --- 4. Inspect the Results ---
print("Predictions from GAM model:")
print(predictions_from_gam)
#>           dx        dy
#> 1  0.5898319 -0.163833
#> 2  0.5908929 -0.161099
#> 3         NA        NA

print("Predictions from Random Forest model:")
print(predictions_from_rf)
} # }
```
