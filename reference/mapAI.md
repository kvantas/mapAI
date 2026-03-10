# mapAI: Positional Accuracy Improvement for Historical Maps

The `mapAI` package provides a cohesive, end-to-end toolkit in R for the
Positional Accuracy Improvement (PAI) and distortion analysis of vector
maps.

## Details

The package is designed for researchers and practitioners in geomatics
and GIS, It provides a complete, modular workflow that guides the user
from importing data to final analysis and visualization.

### The Core Workflow

The main workflow follows a logical sequence:

1.  **Data Handling:** Imports spatial data and control points using
    [`read_map()`](https://kvantas.github.io/mapAI/reference/read_map.md)
    and
    [`read_gcps()`](https://kvantas.github.io/mapAI/reference/read_gcps.md).
    The package also includes functions to generate synthetic data
    ([`create_demo_data()`](https://kvantas.github.io/mapAI/reference/create_demo_data.md))
    and save results
    ([`write_map()`](https://kvantas.github.io/mapAI/reference/write_map.md)).

2.  **Model Training & Validation:** Train a correction model using
    [`train_pai_model()`](https://kvantas.github.io/mapAI/reference/train_pai_model.md)
    with a choice of methods `helmert`,`tps`, `gam`, `lm`, `rf`,
    `svmRadial` and `svmLinear`. Robustly evaluate model performance
    using spatial cross-validation with
    [`assess_pai_model()`](https://kvantas.github.io/mapAI/reference/assess_pai_model.md).

3.  **Geometric Correction:** Apply the trained model to the full vector
    map to get a geometrically corrected version using
    [`apply_pai_model()`](https://kvantas.github.io/mapAI/reference/apply_pai_model.md).

4.  **Distortion Analysis & Visualization:** Go beyond correction to
    quantify and understand the distortion that learns a PAI model
    itself.

    - Use
      [`analyze_distortion()`](https://kvantas.github.io/mapAI/reference/analyze_distortion.md)
      to compute detailed metrics based on Tissot's indicatrix theory
      (e.g., areal scale, angular distortion).

    - Use the dedicated plotting functions
      ([`plot_displacement()`](https://kvantas.github.io/mapAI/reference/plot_displacement.md),
      [`plot_residuals()`](https://kvantas.github.io/mapAI/reference/plot_residuals.md),
      [`plot_distortion_surface()`](https://kvantas.github.io/mapAI/reference/plot_distortion_surface.md),
      [`plot_indicatrices()`](https://kvantas.github.io/mapAI/reference/plot_indicatrices.md))
      to create compelling, publication-quality visualizations of the
      error and distortion patterns.

## See also

To get started with a practical example, see the introductory vignette:
`vignette("getting-started", package = "mapAI")`

For a more in-depth look at comparing models and validation strategies,
see the advanced vignette:
`vignette("swiss-analysis", package = "mapAI")`

The project's source code and issue tracker can be found on GitHub:
<https://github.com/kvantas/mapAI>

## Author

**Maintainer**: Konstantinos Vantas <kon.vantas@gmail.com>
([ORCID](https://orcid.org/0000-0001-6387-8791))
