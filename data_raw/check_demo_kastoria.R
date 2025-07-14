library(mapAI)
library(sf) # Useful for plotting

print(head(mapAI::gcps))
plot(st_geometry(mapAI::parcels), main = "Distorted 'Historical' Map")

# Assess Random Forest with both random and spatial CV
assessment_rf_random <- assess_pai_model(gcps, pai_method = "rf", validation_type = "random", k_folds = 5)
assessment_rf_spatial <- assess_pai_model(gcps, pai_method = "rf", validation_type = "spatial", k_folds = 5)

# Assess gam with both random and spatial CV
assessment_gam_random <- assess_pai_model(gcps, pai_method = "gam", validation_type = "random", k_folds = 5)
assessment_gam_spatial <- assess_pai_model(gcps, pai_method = "gam", validation_type = "spatial", k_folds = 5)

# Assess Linear Model with both random and spatial CV
assessment_lm_random <- assess_pai_model(gcps, pai_method = "lm", validation_type = "random", k_folds = 5)
assessment_lm_spatial <- assess_pai_model(gcps, pai_method = "lm", validation_type = "spatial", k_folds = 5)

all_assessments <- rbind(
  assessment_rf_random,
  assessment_rf_spatial,
  assessment_gam_random,
  assessment_gam_spatial,
  assessment_lm_random,
  assessment_lm_spatial
)

print(all_assessments)

final_model <- train_pai_model(gcp_data = gcps, pai_method = "gam", seed = 123)
corrected_map <- apply_pai_model(pai_model = final_model, map = parcels)

plot_correction_surface(pai_model = final_model, gcp_data = gcps) +
  patchwork::plot_annotation(title = "Learned Correction Field (dx and dy)")

plot(st_geometry(parcels), col = "grey70", lty = 2,
     main = "Map Correction Comparison", border = 'grey40')
plot(st_geometry(corrected_map), col = NA, border = "red", lwd = 1.5, add = TRUE)
legend(
  "topright",
  legend = c("Original (Distorted)", "Corrected"),
  col = c("grey40", "red"),
  lty = c(2, 1),
  lwd = c(1, 1.5),
  bg = "white"
)
