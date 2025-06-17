library(mapAI)
library(sf) # Useful for plotting

demo_files <- create_demo_data(
  type = "complex",
  noise_sd = 0.5,
  seed = 42
)

print(demo_files)


gcp_data <- read_gcps(gcp_path = demo_files$gcp_path, crs = 3857)
map_to_correct <- read_map(shp_path = demo_files$shp_path, crs = 3857)

print(head(gcp_data))
plot(st_geometry(map_to_correct), main = "Distorted 'Historical' Map")

# Assess Random Forest with both random and spatial CV
assessment_rf_random <- assess_pai_model(gcp_data, method = "rf", validation_type = "random", k_folds = 5)
assessment_rf_spatial <- assess_pai_model(gcp_data, method = "rf", validation_type = "spatial", k_folds = 5)

# Assess gam with both random and spatial CV
assessment_gam_random <- assess_pai_model(gcp_data, method = "gam", validation_type = "random", k_folds = 5)
assessment_gam_spatial <- assess_pai_model(gcp_data, method = "gam", validation_type = "spatial", k_folds = 5)

# Assess Linear Model with both random and spatial CV
assessment_lm_random <- assess_pai_model(gcp_data, method = "lm", validation_type = "random", k_folds = 5)
assessment_lm_spatial <- assess_pai_model(gcp_data, method = "lm", validation_type = "spatial", k_folds = 5)


all_assessments <- rbind(
  assessment_rf_random,
  assessment_rf_spatial,
  assessment_gam_random,
  assessment_gam_spatial,
  assessment_lm_random,
  assessment_lm_spatial
)

print(all_assessments)

final_model <- train_pai_model(gcp_data = gcp_data, method = "gam", seed = 123)
corrected_map <- apply_pai_model(pai_model = final_model, map = map_to_correct)

plot_correction_surface(pai_model = final_model, gcp_data = gcp_data) +
  patchwork::plot_annotation(title = "Learned Correction Field (dx and dy)")

plot(st_geometry(map_to_correct), col = "grey70", lty = 2,
     main = "Map Correction Comparison", border = 'grey40')
plot(st_geometry(corrected_map), col = "red", border = "red", lwd = 1.5, add = TRUE)
legend(
  "topright",
  legend = c("Original (Distorted)", "Corrected"),
  col = c("grey40", "red"),
  lty = c(2, 1),
  lwd = c(1, 1.5),
  bg = "white"
)

