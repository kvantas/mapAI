# Train a PAI Model

Trains a supervised learning or analytical model to define a spatial
transformation.

## Usage

``` r
train_pai_model(gcp_data, pai_method, seed = 123, ...)
```

## Arguments

- gcp_data:

  An `sf` object of homologous points from
  [`read_gcps()`](https://kvantas.github.io/mapAI/reference/read_gcps.md).

- pai_method:

  A character string specifying the algorithm. One of: "lm","tps",
  "gam", "rf", "svmRadial", "svmLinear", or "helmert".

- seed:

  An integer for setting the random seed for reproducibility.

- ...:

  Additional arguments passed to the underlying model fitting functions
  ([`mgcv::gam`](https://rdrr.io/pkg/mgcv/man/gam.html),
  [`stats::lm`](https://rdrr.io/r/stats/lm.html),
  [`ranger::ranger`](http://imbs-hl.github.io/ranger/reference/ranger.md),
  [`fields::Tps`](https://rdrr.io/pkg/fields/man/Tps.html),
  [`e1071::svm`](https://rdrr.io/pkg/e1071/man/svm.html)).

## Value

A trained model object of class `pai_model`.

## Details

This function serves as a factory for creating transformation models. It
supports machine learning methods ("lm", "gam", "rf", "svmRadial",
"svmLinear") that learn the relationship between source coordinates and
displacement vectors, as well as the analytical "helmert" method which
solves for a global similarity transformation.

**Important**: The more flexible machine learning models, `gam` and
`rf`, require a sufficient number of data points to produce stable and
reliable results. This function will prevent training these models with
fewer than 60 homologous points to avoid overfitting. If you have a
small number of points, please use the more robust "lm" or "helmert"
methods.

## Examples

``` r
# Load the ground control points data
data(gcps)

# Train a General Additive Model (GAM)
gam_model <- train_pai_model(gcps, pai_method = "gam")
#> Training 'gam' model...
summary(gam_model$model)
#> 
#> Family: Multivariate normal 
#> Link function: 
#> 
#> Formula:
#> dx ~ s(source_x, source_y)
#> <environment: 0x5587c0f411e0>
#> dy ~ s(source_x, source_y)
#> <environment: 0x5587c0f411e0>
#> 
#> Parametric coefficients:
#>               Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)    0.05257    0.01722   3.053  0.00227 ** 
#> (Intercept).1 -0.31945    0.02119 -15.074  < 2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Approximate significance of smooth terms:
#>                           edf Ref.df Chi.sq p-value    
#> s(source_x,source_y)   23.845  27.45  234.5  <2e-16 ***
#> s.1(source_x,source_y)  7.624  10.63  156.8  <2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Deviance explained = 39.9%
#> -REML = -305.73  Scale est. = 1         n = 300

# Train a linear model
lm_model <- train_pai_model(gcps, pai_method = "lm")
#> Training 'lm' model...
summary(lm_model$model$model_dx)
#> 
#> Call:
#> stats::lm(formula = formula, data = data)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -1.27422 -0.18591  0.00183  0.15729  1.18893 
#> 
#> Coefficients:
#>               Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) -6.644e+03  7.461e+02  -8.904   <2e-16 ***
#> source_x     5.886e-05  1.373e-04   0.429    0.668    
#> source_y     1.477e-03  1.663e-04   8.880   <2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 0.3619 on 297 degrees of freedom
#> Multiple R-squared:  0.2109, Adjusted R-squared:  0.2056 
#> F-statistic: 39.68 on 2 and 297 DF,  p-value: 5.333e-16
#> 
summary(lm_model$model$model_dy)
#> 
#> Call:
#> stats::lm(formula = formula, data = data)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -1.22190 -0.22767 -0.03835  0.20294  1.60099 
#> 
#> Coefficients:
#>               Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) -6.104e+03  7.848e+02  -7.778 1.22e-13 ***
#> source_x    -1.228e-03  1.444e-04  -8.505 9.02e-16 ***
#> source_y     1.434e-03  1.749e-04   8.196 7.53e-15 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 0.3806 on 297 degrees of freedom
#> Multiple R-squared:  0.3129, Adjusted R-squared:  0.3083 
#> F-statistic: 67.64 on 2 and 297 DF,  p-value: < 2.2e-16
#> 
```
