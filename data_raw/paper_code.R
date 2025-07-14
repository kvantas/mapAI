# ---
# R Script to Generate All Results and Figures for the mapAI Paper
# ---

# ... (Step 1: Setup remains the same) ...
message("Loading libraries...")
library(mapAI)
library(sf)
library(dplyr)
library(ggplot2)
library(patchwork)
library(knitr)
dir.create("./data_raw/figures", showWarnings = FALSE, recursive = TRUE)


# --- 2. Initial Data Loading and Error Assessment (for Figure 2) ---
message("Step 2: Assessing initial error...")
data(swiss_cps)
initial_rmse <- sqrt(mean(swiss_cps$dx^2 + swiss_cps$dy^2))
cat("Initial Baseline 2D RMSE:", initial_rmse, "meters\n")
figure2_plot <- plot_displacement(swiss_cps, title = "Residual Displacement Vectors of the swiss_cps Dataset") +
  theme(plot.title = element_text(size = 10))
ggsave("./data_raw/figures/figure2_displacement.png", plot = figure2_plot, width = 6, height = 6, dpi = 300)
message("Saved Figure 2: Displacement Plot")


# --- NEW: 3. Model Validation and Selection (for Table 2) ---
message("\nStep 3: Performing Spatial Cross-Validation...")

# Perform 5-fold spatial cross-validation for all models
# Note: This can take a moment, especially for rf and gam.
cv_results_helmert <- assess_pai_model(swiss_cps, pai_method = "helmert", validation_type = "spatial", k_folds = 5)
cv_results_lm <- assess_pai_model(swiss_cps, pai_method = "lm", validation_type = "random", k_folds = 10)
cv_results_rf <- assess_pai_model(swiss_cps, pai_method = "rf", validation_type = "random", k_folds = 10)
cv_results_gam <- assess_pai_model(swiss_cps, pai_method = "gam", validation_type = "random", k_folds = 10)

# Combine results into a single data frame
validation_df <- rbind(
  cv_results_helmert,
  cv_results_lm,
  cv_results_rf,
  cv_results_gam
)

# Format the table for the paper (Table 2)
validation_df_formatted <- validation_df %>%
  select(
    Model_Method = Method,
    Mean_2D_RMSE_m = Mean_RMSE_2D,
    Std_Deviation_of_RMSE_m = SD_RMSE_2D
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1)))

# Print the validation table
print(kable(validation_df_formatted, caption = "Table 2: 10-Fold Cross-Validation performance."))
message("Generated Table 2: Cross-Validation Results")


# --- 4. Final Model Training and Correction (for text results) ---
message("\nStep 4: Training final model and calculating final RMSE...")

# Train the chosen model (GAM) on all data
final_gam_model <- train_pai_model(swiss_cps, pai_method = "gam")

# Calculate the final RMSE on the full dataset (in-sample error, for reporting)
final_corrected_rmse <- calculate_corrected_rmse(final_gam_model, swiss_cps)
error_reduction <- (1 - (final_corrected_rmse / initial_rmse)) * 100

cat("Final Corrected RMSE (GAM model):", final_corrected_rmse, "meters\n")
cat("Total Error Reduction:", error_reduction, "%\n")


# --- 5. Visualizing the Correction (for Figure 3) ---
message("\nStep 5: Visualizing the correction on a reference grid...")

# Train a helmert model just for the visual comparison
source_grid <- sf::st_make_grid(swiss_cps, n = c(15, 15)) %>% sf::st_cast("MULTILINESTRING") %>% sf::st_sf()
helmert_corrected_grid <- source_grid
gam_corrected_grid <- apply_pai_model(final_gam_model, source_grid)

# ... (rest of plot generation for Figure 3 is the same) ...
plot_a <- ggplot() +
  geom_sf(data = helmert_corrected_grid, color = "grey50") +
  labs(title = "(a) Old map grid") +
  theme_minimal()
plot_b <- ggplot() +
  geom_sf(data = gam_corrected_grid, color = "#e41a1c") +
  labs(title = "(b) GAM Model Correction") +
  theme_minimal()
figure3_plot <- plot_a + plot_b
figure3_plot
ggsave("./data_raw/figures/figure3_grid_comparison.png", plot = figure3_plot, width = 8, height = 4, dpi = 300)
message("Saved Figure 3: Grid Correction Comparison")


# --- 6. Quantitative Distortion Analysis (for Figure 4) ---
message("\nStep 6: Performing quantitative distortion analysis...")
# ... (rest of script for Figures 4 and 5 is the same, using final_gam_model) ...
analysis_points <- sf::st_make_grid(swiss_cps, n = c(50, 50)) %>%
  sf::st_centroid() %>%
  sf::st_sf()

distortion_results <- analyze_distortion(final_gam_model, analysis_points)
plot_area <- plot_distortion_surface(distortion_results, "log2_area_scale", diverging = TRUE) +
  labs(title = "(a) Logarithmic Areal Distortion")
plot_shear <- plot_distortion_surface(distortion_results, "max_shear") +
  labs(title = "(b) Maximum Angular Distortion (°)")
figure4_plot <- plot_area + plot_shear

ggsave("./data_raw/figures/figure4_distortion_surfaces.png", plot = figure4_plot, width = 9, height = 4, dpi = 300)
message("Saved Figure 4: Distortion Surface Plots")

# --- 7. Visualization of Tissot's Indicatrices (for Figure 5) ---
message("\nStep 7: Visualizing Tissot's Indicatrices...")
distortion_at_gcps <- analyze_distortion(final_gam_model, swiss_cps)
figure5_plot <- plot_indicatrices(distortion_at_gcps, final_gam_model, scale_factor = 800) +
  labs(title = "Tissot's Indicatrices at GCP Locations")
ggsave("man/figures/figure5_indicatrices.png", plot = figure5_plot, width = 6, height = 6, dpi = 300)
message("Saved Figure 5: Tissot's Indicatrices Plot")

message("\nAnalysis script complete.")
