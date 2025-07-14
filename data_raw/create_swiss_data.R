library(mapAI)
library(sf)

swiss_p <- read_gcps("./data_raw/mclaeysb.csv", crs = 21781)

# initial displacement
plot_displacement(swiss_p)

# fit a helmert model
helmert_model <- train_pai_model(swiss_p, pai_method = "helmert")

# align orignal data using the helmert model
swiss_align <- apply_pai_model(helmert_model, swiss_p)


head(swiss_p)
head(swiss_align)

# compute again dx and dy for the aligned maps
align_coords <- sf::st_coordinates(swiss_align)

swiss_align$source_x <- align_coords[,1]
swiss_align$source_y <- align_coords[,2]
swiss_align$dx <- swiss_align$target_x - swiss_align$source_x
swiss_align$dy <- swiss_align$target_y - swiss_align$source_y


plot_displacement(swiss_align)

swiss_cps <- swiss_align

usethis::use_data(swiss_cps, overwrite = TRUE)


##########

pai_model <- train_pai_model(swiss_cps, pai_method = "lm")


# Analyze the distortion on a regular grid of points
analysis_grid <- sf::st_make_grid(swiss_cps, n = c(20, 20)) %>%
  sf::st_centroid() %>%
  sf::st_sf()

distortion_results <- analyze_distortion(pai_model, analysis_grid)

# Plot the learned 'log2_area_scale'. This is a symmetric metric centered
# at 0, making it ideal for a diverging palette. Red areas were expanded,
# blue areas were contracted.
plot_distortion_surface(
  distortion_results,
  metric = "log2_area_scale",
  diverging = TRUE
)

plot_correction_surface(pai_model, swiss_cps)
