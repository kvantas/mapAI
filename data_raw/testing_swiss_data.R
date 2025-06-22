library(magrittr)
data(swiss_cps)
gam_model <- train_pai_model(swiss_cps, method = "gam")

# --- 2. Create a regular grid of POINTS for analysis ---
analysis_points <- sf::st_make_grid(swiss_cps, n = c(40, 40)) %>%
  sf::st_centroid() %>%
  sf::st_sf()


distortion_on_grid <- analyze_distortion(gam_model, analysis_points)

# --- 3. Plot a metric using a standard sequential scale ---
plot_distortion_surface(
  distortion_on_grid,
  metric = "max_shear",
  palette = "magma"
)

# --- 4. Plot a metric using a diverging scale ---
# 'log2_area_scale' is ideal for this, as it's centered at 0.
plot_distortion_surface(
  distortion_on_grid,
  metric = "log2_area_scale",
  diverging = TRUE
)


plot_distortion_surface(
  distortion_on_grid,
  metric = "area_scale",
)

plot_displacement(swiss_cps)

plot_indicatrices(distortion_on_grid, scale_factor = 500)
