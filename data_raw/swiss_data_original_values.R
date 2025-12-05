library(mapAI)
library(sf)

#crs = 21781
data_file <- read.csv("./data_raw/mclaeysb.csv")
swiss_p <- read_gcp(source_x = data_file$source_x,
                    source_y = data_file$source_y,
                    target_x = data_file$target_x,
                    target_y = data_file$target_y)

# Create a regular line grid to serve as our reference map
swiss_cps <- sf::st_as_sf(swiss_p, coords = c("source_x", "source_y"), crs = 21781)

# create the source map grid
source_grid <- sf::st_make_grid(swiss_cps, n = c(12, 8), square = TRUE) |>
  st_cast("MULTILINESTRING") |>
  st_sf()

# plot both points and grid
plot(sf::st_geometry(source_grid), col = 'lightgrey', lwd = 2)
plot(sf::st_geometry(swiss_cps), add = TRUE, pch = 16)

# fit a tps model
pai_model <- train_pai_model(swiss_p, "gam_biv")

# distortion analysis
distortions <- analyze_distortion(pai_model, swiss_p)

plot(distortions, "area_scale")
plot(distortions, "max_shear")

surface(pai_model, n_grid = 100)
residuals(pai_model)
