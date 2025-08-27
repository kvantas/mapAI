library(mapAI)
library(sf)

#crs = 21781
data_file <- read.csv("./data_raw/mclaeysb.csv")
swiss_p <- read_gcp(source_x = data_file$source_x,
                    source_y = data_file$source_y,
                    target_x = data_file$target_x,
                    target_y = data_file$target_y)
rm(data_file)

# initial displacement
plot(swiss_p)

# fit a helmert model
helmert_model <- train_pai_model(gcp_data = swiss_p, method = "helmert")


# align orignal data using the helmert model
swiss_align <- predict(helmert_model, swiss_p)


# replace original coords with transformed ones
swiss_p$source_x <- swiss_align$target_x
swiss_p$source_y <- swiss_align$target_y

swiss_p$dx <- swiss_p$target_x - swiss_p$source_x
swiss_p$dy <- swiss_p$target_y - swiss_p$source_y
plot(swiss_p)
rm(swiss_align)

# create a grid of the source map

# Create a regular line grid to serve as our reference map
swiss_cps <- sf::st_as_sf(swiss_p, coords = c("source_x", "source_y"), crs = 21781)

source_grid <- sf::st_make_grid(swiss_cps, n = c(12, 8), square = TRUE) |>
  st_cast("MULTILINESTRING") |>
  st_sf()
plot(source_grid)

# add to swiss data source_grid the

basel_frickthal <- list(gcp = swiss_p, grid = source_grid)

usethis::use_data(basel_frickthal, overwrite = TRUE)


#########

pai_model <- train_pai_model(basel_frickthal$gcp, "tps")
surface(pai_model)

distortion_results <- analyze_distortion(pai_model)

plot(distortion_results, metric = "area_scale")

# plot tissots indicatrices
indicatrices(distortion_results, scale_factor = 800)
