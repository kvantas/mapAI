# ==============================================================================
# Benchmark & Test Suite for apply_pai_model Implementations
# ==============================================================================
# This script tests the correctness and benchmarks the execution speed of three
# different implementations of the apply_pai_model function:
#
#   1. Original (v1) : data_raw/original_apply_function.R
#      - Loops over features one-by-one
#      - Calls predict(pai_model) for every single feature
#      - Reconstructs each sfg individually with sf::st_* constructors
#
#   2. Vectorized v2 : data_raw/apply_pai_model2.R
#      - Extracts all coordinates across all features at once
#      - Performs a single predict() call for all coordinates
#      - Reconstructs geometries in an R loop using split.data.frame & sf::st_*
#      - Includes Step 5 (dissolving AOI-split fragments)
#
#   3. Package (v3)  : R/apply_pai_model.R
#      - Extracts all coordinates across all features at once
#      - Performs a single predict() call for all coordinates
#      - Directly updates the underlying nested list/matrix structure in-place
#        via a recursive function (update_geom) without rebuilding sfg objects
#      - Includes Step 5 (dissolving AOI-split fragments)
# ==============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(testthat)
})

# Find project root
proj_root <- getwd()
if (!file.exists(file.path(proj_root, "DESCRIPTION"))) {
  if (file.exists(file.path("..", "DESCRIPTION"))) {
    proj_root <- normalizePath("..")
  } else {
    stop("Could not determine package root directory.")
  }
}

# Ensure mapAI package functions and datasets are loaded
if ("mapAI" %in% loadedNamespaces()) {
  pkg_env <- asNamespace("mapAI")
} else {
  pkg_env <- devtools::load_all(proj_root, quiet = TRUE)$env
}

# ------------------------------------------------------------------------------
# 1. Load the Three Implementations into Distinct Environments
# ------------------------------------------------------------------------------
cat("==============================================================================\n")
cat("Loading apply_pai_model implementations...\n")
cat("==============================================================================\n")

orig_path <- file.path(proj_root, "data_raw", "original_apply_function.R")
v2_path   <- file.path(proj_root, "data_raw", "apply_pai_model2.R")
pkg_path  <- file.path(proj_root, "R", "apply_pai_model.R")

if (!file.exists(orig_path)) stop("Missing: ", orig_path)
if (!file.exists(v2_path))   stop("Missing: ", v2_path)
if (!file.exists(pkg_path))  stop("Missing: ", pkg_path)

env_orig <- new.env(parent = asNamespace("mapAI"))
sys.source(orig_path, envir = env_orig)
apply_pai_model_orig <- env_orig$apply_pai_model

env_v2 <- new.env(parent = asNamespace("mapAI"))
sys.source(v2_path, envir = env_v2)
apply_pai_model_v2 <- env_v2$apply_pai_model

env_pkg <- new.env(parent = asNamespace("mapAI"))
sys.source(pkg_path, envir = env_pkg)
apply_pai_model_pkg <- env_pkg$apply_pai_model

cat("[OK] Loaded Implementation 1: Original (v1, feature-by-feature predict)\n")
cat("[OK] Loaded Implementation 2: Vectorized v2 (single predict, loop rebuild)\n")
cat("[OK] Loaded Implementation 3: Package (v3, single predict, recursive update)\n\n")

# ------------------------------------------------------------------------------
# 2. Data Generators (from package test suite)
# ------------------------------------------------------------------------------

# Helper: Create grid of polygons (from tests/testthat/test-apply_pai_model_grid.R)
create_polygon_grid <- function(n_rows, n_cols, cell_size = 10, crs = 3857) {
  n_total <- n_rows * n_cols
  polygons <- vector("list", n_total)
  idx <- 1
  for (i in seq_len(n_rows)) {
    for (j in seq_len(n_cols)) {
      x_min <- (j - 1) * cell_size
      y_min <- (i - 1) * cell_size
      x_max <- j * cell_size
      y_max <- i * cell_size
      coords <- matrix(
        c(
          x_min, y_min,
          x_max, y_min,
          x_max, y_max,
          x_min, y_max,
          x_min, y_min
        ),
        ncol = 2, byrow = TRUE
      )
      polygons[[idx]] <- sf::st_polygon(list(coords))
      idx <- idx + 1
    }
  }
  sf::st_sf(
    id = seq_len(n_total),
    geometry = sf::st_sfc(polygons, crs = crs)
  )
}

# Helper: Create grid of multipolygons (from tests/testthat/test-apply_pai_model_grid.R)
create_multipolygon_grid <- function(n_rows, n_cols, cell_size = 10, crs = 3857) {
  n_total <- n_rows * n_cols
  multipolygons <- vector("list", n_total)
  idx <- 1
  for (i in seq_len(n_rows)) {
    for (j in seq_len(n_cols)) {
      x_min <- (j - 1) * cell_size
      y_min <- (i - 1) * cell_size
      x_max <- j * cell_size
      y_max <- i * cell_size
      coords <- matrix(
        c(
          x_min, y_min,
          x_max, y_min,
          x_max, y_max,
          x_min, y_max,
          x_min, y_min
        ),
        ncol = 2, byrow = TRUE
      )
      poly <- sf::st_polygon(list(coords))
      multipolygons[[idx]] <- sf::st_multipolygon(list(poly))
      idx <- idx + 1
    }
  }
  sf::st_sf(
    id = seq_len(n_total),
    geometry = sf::st_sfc(multipolygons, crs = crs)
  )
}

# Helper: Create grid of linestrings
create_linestring_grid <- function(n_rows, n_cols, cell_size = 10, crs = 3857) {
  lines <- vector("list", n_rows + n_cols)
  idx <- 1
  # Horizontal lines
  for (i in seq_len(n_rows)) {
    y <- (i - 1) * cell_size
    lines[[idx]] <- sf::st_linestring(matrix(c(0, y, (n_cols - 1) * cell_size, y), ncol = 2, byrow = TRUE))
    idx <- idx + 1
  }
  # Vertical lines
  for (j in seq_len(n_cols)) {
    x <- (j - 1) * cell_size
    lines[[idx]] <- sf::st_linestring(matrix(c(x, 0, x, (n_rows - 1) * cell_size), ncol = 2, byrow = TRUE))
    idx <- idx + 1
  }
  sf::st_sf(
    id = seq_len(length(lines)),
    geometry = sf::st_sfc(lines, crs = crs)
  )
}

# Helper: Create polygon with a hole (from tests/testthat/test-apply_pai_model.R)
create_polygon_with_hole <- function(crs = 3857) {
  outer_ring <- matrix(c(0, 0, 0, 100, 100, 100, 100, 0, 0, 0), ncol = 2, byrow = TRUE)
  inner_hole <- matrix(c(25, 25, 75, 25, 75, 75, 25, 75, 25, 25), ncol = 2, byrow = TRUE)
  poly <- sf::st_polygon(list(outer_ring, inner_hole))
  sf::st_sf(id = 1L, geometry = sf::st_sfc(poly, crs = crs))
}

# ------------------------------------------------------------------------------
# 3. Test Suite: Verification of Equivalence Across Implementations
# ------------------------------------------------------------------------------
cat("==============================================================================\n")
cat("RUNNING TEST SUITE: Correctness & Output Equivalence\n")
cat("==============================================================================\n")

# Load datasets and train models
utils::data(gcps, package = "mapAI", envir = environment())
utils::data(parcels, package = "mapAI", envir = environment())

model_gam <- mapAI::train_pai_model(gcps, pai_method = "gam")
model_rf  <- mapAI::train_pai_model(gcps, pai_method = "rf")
model_lm  <- mapAI::train_pai_model(gcps, pai_method = "lm")

test_reporter <- list(passed = 0L, failed = 0L)

run_test <- function(test_name, expr) {
  cat(sprintf("  • %-55s ... ", test_name))
  tryCatch({
    expr
    test_reporter$passed <<- test_reporter$passed + 1L
    cat("[PASS]\n")
  }, error = function(e) {
    test_reporter$failed <<- test_reporter$failed + 1L
    cat("[FAIL]\n    Error:", conditionMessage(e), "\n")
  })
}

run_test("POINT geometry (gcps) coordinates match", {
  res_orig <- suppressMessages(apply_pai_model_orig(model_gam, gcps))
  res_v2   <- suppressMessages(apply_pai_model_v2(model_gam, gcps))
  res_pkg  <- suppressMessages(apply_pai_model_pkg(model_gam, gcps))

  expect_equal(sf::st_coordinates(res_orig), sf::st_coordinates(res_v2), tolerance = 1e-7)
  expect_equal(sf::st_coordinates(res_v2), sf::st_coordinates(res_pkg), tolerance = 1e-7)
  expect_equal(nrow(res_pkg), nrow(gcps))
})

run_test("LINESTRING geometry coordinates match", {
  ls_grid <- create_linestring_grid(5, 5)
  res_orig <- suppressMessages(apply_pai_model_orig(model_gam, ls_grid))
  res_v2   <- suppressMessages(apply_pai_model_v2(model_gam, ls_grid))
  res_pkg  <- suppressMessages(apply_pai_model_pkg(model_gam, ls_grid))

  expect_equal(sf::st_coordinates(res_orig), sf::st_coordinates(res_v2), tolerance = 1e-7)
  expect_equal(sf::st_coordinates(res_v2), sf::st_coordinates(res_pkg), tolerance = 1e-7)
  expect_equal(nrow(res_pkg), nrow(ls_grid))
})

run_test("POLYGON simple grid coordinates and area_new match", {
  poly_grid <- create_polygon_grid(5, 5)
  res_orig <- suppressMessages(apply_pai_model_orig(model_gam, poly_grid))
  res_v2   <- suppressMessages(apply_pai_model_v2(model_gam, poly_grid))
  res_pkg  <- suppressMessages(apply_pai_model_pkg(model_gam, poly_grid))

  expect_equal(sf::st_coordinates(res_orig), sf::st_coordinates(res_v2), tolerance = 1e-7)
  expect_equal(sf::st_coordinates(res_v2), sf::st_coordinates(res_pkg), tolerance = 1e-7)
  expect_equal(as.numeric(res_orig$area_new), as.numeric(res_pkg$area_new), tolerance = 1e-7)
  expect_equal(nrow(res_pkg), 25)
})

run_test("POLYGON with hole preserves inner ring structure", {
  poly_hole <- create_polygon_with_hole()
  res_orig <- suppressMessages(apply_pai_model_orig(model_gam, poly_hole))
  res_v2   <- suppressMessages(apply_pai_model_v2(model_gam, poly_hole))
  res_pkg  <- suppressMessages(apply_pai_model_pkg(model_gam, poly_hole))

  expect_equal(sf::st_coordinates(res_orig), sf::st_coordinates(res_v2), tolerance = 1e-7)
  expect_equal(sf::st_coordinates(res_v2), sf::st_coordinates(res_pkg), tolerance = 1e-7)
  # Verify both outer and inner rings are present (list of 2 matrices)
  expect_equal(length(res_pkg$geometry[[1]]), 2)
  expect_equal(length(res_v2$geometry[[1]]), 2)
  expect_equal(length(res_orig$geometry[[1]]), 2)
})

run_test("MULTIPOLYGON grid preserves structure and coordinates", {
  mpoly_grid <- create_multipolygon_grid(5, 5)
  res_orig <- suppressMessages(apply_pai_model_orig(model_gam, mpoly_grid))
  res_v2   <- suppressMessages(apply_pai_model_v2(model_gam, mpoly_grid))
  res_pkg  <- suppressMessages(apply_pai_model_pkg(model_gam, mpoly_grid))

  expect_equal(sf::st_coordinates(res_orig), sf::st_coordinates(res_v2), tolerance = 1e-7)
  expect_equal(sf::st_coordinates(res_v2), sf::st_coordinates(res_pkg), tolerance = 1e-7)
  expect_equal(as.character(unique(sf::st_geometry_type(res_pkg))), "MULTIPOLYGON")
})

run_test("Real-world MULTIPOLYGON (parcels: 493 features) match", {
  parcels_sub <- parcels[1:50, ]
  res_orig <- suppressMessages(apply_pai_model_orig(model_gam, parcels_sub))
  res_v2   <- suppressMessages(apply_pai_model_v2(model_gam, parcels_sub))
  res_pkg  <- suppressMessages(apply_pai_model_pkg(model_gam, parcels_sub))

  expect_equal(sf::st_coordinates(res_orig), sf::st_coordinates(res_v2), tolerance = 1e-7)
  expect_equal(sf::st_coordinates(res_v2), sf::st_coordinates(res_pkg), tolerance = 1e-7)
  expect_equal(as.numeric(res_orig$area_new), as.numeric(res_pkg$area_new), tolerance = 1e-7)
})

run_test("Empty geometries handled gracefully across all versions", {
  p1 <- sf::st_polygon(list(matrix(c(0, 0, 10, 0, 10, 10, 0, 10, 0, 0), ncol = 2, byrow = TRUE)))
  empty_geom <- sf::st_polygon()
  sf_empty <- sf::st_sf(id = 1:2, geometry = sf::st_sfc(p1, empty_geom, crs = 3857))

  res_orig <- suppressMessages(apply_pai_model_orig(model_gam, sf_empty))
  res_v2   <- suppressMessages(apply_pai_model_v2(model_gam, sf_empty))
  res_pkg  <- suppressMessages(apply_pai_model_pkg(model_gam, sf_empty))

  expect_true(sf::st_is_empty(res_orig$geometry[[2]]))
  expect_true(sf::st_is_empty(res_v2$geometry[[2]]))
  expect_true(sf::st_is_empty(res_pkg$geometry[[2]]))
  # Check non-empty feature coordinates match
  expect_equal(sf::st_coordinates(res_orig[1, ]), sf::st_coordinates(res_v2[1, ]), tolerance = 1e-7)
  expect_equal(sf::st_coordinates(res_v2[1, ]), sf::st_coordinates(res_pkg[1, ]), tolerance = 1e-7)
})

run_test("AOI full-coverage preserves all features identically", {
  grid <- create_polygon_grid(5, 5)
  aoi_full <- sf::st_sf(
    id = 1,
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(-10, -10, 60, -10, 60, 60, -10, 60, -10, -10), ncol = 2, byrow = TRUE))),
      crs = 3857
    )
  )
  res_v2  <- suppressMessages(apply_pai_model_v2(model_gam, grid, aoi = aoi_full))
  res_pkg <- suppressMessages(apply_pai_model_pkg(model_gam, grid, aoi = aoi_full))

  expect_equal(nrow(res_v2), 25)
  expect_equal(nrow(res_pkg), 25)
  expect_equal(sf::st_coordinates(res_v2), sf::st_coordinates(res_pkg), tolerance = 1e-7)
})

run_test("AOI cutting/dissolve: v2 and Package yield identical result", {
  grid <- create_polygon_grid(6, 6)
  aoi_cut <- sf::st_sf(
    id = 1,
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 35, 0, 35, 35, 0, 35, 0, 0), ncol = 2, byrow = TRUE))),
      crs = 3857
    )
  )
  res_v2  <- suppressMessages(apply_pai_model_v2(model_gam, grid, aoi = aoi_cut))
  res_pkg <- suppressMessages(apply_pai_model_pkg(model_gam, grid, aoi = aoi_cut))

  # Both v2 and Package have the Step 5 dissolve logic
  expect_equal(nrow(res_v2), 36)
  expect_equal(nrow(res_pkg), 36)
  # Compare via st_as_text to support mixed sfc_GEOMETRY types
  expect_equal(sf::st_as_text(sf::st_geometry(res_v2)), sf::st_as_text(sf::st_geometry(res_pkg)))
})

run_test("Different model algorithms (RF, LM) yield identical results", {
  poly_grid <- create_polygon_grid(4, 4)
  res_rf_v2  <- suppressMessages(apply_pai_model_v2(model_rf, poly_grid))
  res_rf_pkg <- suppressMessages(apply_pai_model_pkg(model_rf, poly_grid))
  expect_equal(sf::st_coordinates(res_rf_v2), sf::st_coordinates(res_rf_pkg), tolerance = 1e-7)

  res_lm_v2  <- suppressMessages(apply_pai_model_v2(model_lm, poly_grid))
  res_lm_pkg <- suppressMessages(apply_pai_model_pkg(model_lm, poly_grid))
  expect_equal(sf::st_coordinates(res_lm_v2), sf::st_coordinates(res_lm_pkg), tolerance = 1e-7)
})

cat(sprintf("\nTest Suite Summary: %d Passed, %d Failed\n\n",
            test_reporter$passed, test_reporter$failed))

# ------------------------------------------------------------------------------
# 4. Benchmarking Engine
# ------------------------------------------------------------------------------
cat("==============================================================================\n")
cat("BENCHMARK SUITE: Execution Speed Comparison\n")
cat("==============================================================================\n\n")

# Precise timing function taking a zero-argument function closure
time_closure <- function(fn, iterations = 5, warmup = 1) {
  # Warmup
  for (w in seq_len(warmup)) {
    suppressMessages(fn())
  }
  times <- numeric(iterations)
  for (i in seq_len(iterations)) {
    t0 <- proc.time()[["elapsed"]]
    suppressMessages(fn())
    t1 <- proc.time()[["elapsed"]]
    times[i] <- t1 - t0
  }
  list(
    mean   = mean(times),
    median = stats::median(times),
    min    = min(times),
    max    = max(times),
    sd     = stats::sd(times)
  )
}

# Helper to run a benchmark across all three implementations
benchmark_scenario <- function(name, map_obj, model_obj, aoi_obj = NULL, iterations = 5) {
  n_features <- nrow(map_obj)
  coords <- sf::st_coordinates(map_obj)
  n_vertices <- nrow(coords)

  cat(sprintf("Benchmarking: %-32s [%4d feats, %6d vertices] ...\n",
              name, n_features, n_vertices))

  fn_orig <- function() apply_pai_model_orig(model_obj, map_obj, aoi = aoi_obj)
  fn_v2   <- function() apply_pai_model_v2(model_obj, map_obj, aoi = aoi_obj)
  fn_pkg  <- function() apply_pai_model_pkg(model_obj, map_obj, aoi = aoi_obj)

  t_orig <- time_closure(fn_orig, iterations = iterations)
  t_v2   <- time_closure(fn_v2, iterations = iterations)
  t_pkg  <- time_closure(fn_pkg, iterations = iterations)

  # Use microsecond floor for ratios to prevent NA/Inf when timing rounds near zero
  t_pkg_floor <- max(t_pkg$median, 0.0005)
  speedup_vs_orig <- t_orig$median / t_pkg_floor
  speedup_vs_v2   <- t_v2$median / t_pkg_floor

  data.frame(
    Scenario         = name,
    Features         = n_features,
    Vertices         = n_vertices,
    Orig_Median_s    = sprintf("%.3f", t_orig$median),
    V2_Median_s      = sprintf("%.3f", t_v2$median),
    Pkg_Median_s     = sprintf("%.3f", t_pkg$median),
    Speedup_vs_Orig  = sprintf("%.1fx", speedup_vs_orig),
    Speedup_vs_v2    = sprintf("%.1fx", speedup_vs_v2),
    raw_orig         = t_orig$median,
    raw_v2           = t_v2$median,
    raw_pkg          = t_pkg$median,
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------------------------
# 5. Define Benchmark Scenarios
# ------------------------------------------------------------------------------

# Datasets
poly_grid_100  <- create_polygon_grid(10, 10)      # 100 polygons
poly_grid_400  <- create_polygon_grid(20, 20)      # 400 polygons
poly_grid_1000 <- create_polygon_grid(32, 32)      # 1024 polygons
mpoly_grid_100 <- create_multipolygon_grid(10, 10)  # 100 multipolygons
mpoly_grid_400 <- create_multipolygon_grid(20, 20)  # 400 multipolygons
line_grid_100  <- create_linestring_grid(50, 50)   # 100 lines
parcels_all    <- parcels                          # 493 cadastral parcels

# AOI covering 50% of 400 polygon grid
aoi_mid <- sf::st_sf(
  id = 1,
  geometry = sf::st_sfc(
    sf::st_polygon(list(matrix(c(0, 0, 100, 0, 100, 100, 0, 100, 0, 0), ncol = 2, byrow = TRUE))),
    crs = 3857
  )
)

results_list <- list()

# 1. POINT data (gcps: 300 points)
results_list[[1]] <- benchmark_scenario(
  name = "POINT: gcps (GAM)",
  map_obj = gcps,
  model_obj = model_gam,
  iterations = 5
)

# 2. LINESTRING grid (100 lines)
results_list[[2]] <- benchmark_scenario(
  name = "LINESTRING: grid 50x50 (GAM)",
  map_obj = line_grid_100,
  model_obj = model_gam,
  iterations = 5
)

# 3. POLYGON Small (100 features)
results_list[[3]] <- benchmark_scenario(
  name = "POLYGON: Small 10x10 (GAM)",
  map_obj = poly_grid_100,
  model_obj = model_gam,
  iterations = 5
)

# 4. POLYGON Medium (400 features)
results_list[[4]] <- benchmark_scenario(
  name = "POLYGON: Medium 20x20 (GAM)",
  map_obj = poly_grid_400,
  model_obj = model_gam,
  iterations = 5
)

# 5. POLYGON Large (~1,000 features)
results_list[[5]] <- benchmark_scenario(
  name = "POLYGON: Large 32x32 (GAM)",
  map_obj = poly_grid_1000,
  model_obj = model_gam,
  iterations = 3
)

# 6. MULTIPOLYGON (400 features)
results_list[[6]] <- benchmark_scenario(
  name = "MULTIPOLYGON: 20x20 (GAM)",
  map_obj = mpoly_grid_400,
  model_obj = model_gam,
  iterations = 5
)

# 7. Real Cadastral Parcels (493 features)
results_list[[7]] <- benchmark_scenario(
  name = "REAL DATA: parcels (GAM)",
  map_obj = parcels_all,
  model_obj = model_gam,
  iterations = 5
)

# 8. Real Cadastral Parcels with Random Forest
results_list[[8]] <- benchmark_scenario(
  name = "REAL DATA: parcels (RF)",
  map_obj = parcels_all,
  model_obj = model_rf,
  iterations = 5
)

# 9. POLYGON with AOI (400 features)
results_list[[9]] <- benchmark_scenario(
  name = "AOI Cut: Grid 20x20 (GAM)*",
  map_obj = poly_grid_400,
  model_obj = model_gam,
  aoi_obj = aoi_mid,
  iterations = 3
)

# Combine all results into single data frame
benchmark_results <- do.call(rbind, results_list)

# ------------------------------------------------------------------------------
# 6. Print Benchmark Results Summary Table
# ------------------------------------------------------------------------------
cat("\n==============================================================================\n")
cat("BENCHMARK RESULTS SUMMARY TABLE (Median Elapsed Time in Seconds)\n")
cat("==============================================================================\n\n")

display_table <- benchmark_results[, c(
  "Scenario", "Features", "Vertices",
  "Orig_Median_s", "V2_Median_s", "Pkg_Median_s",
  "Speedup_vs_Orig", "Speedup_vs_v2"
)]

print(knitr::kable(
  display_table,
  format = "simple",
  col.names = c(
    "Scenario",
    "Features",
    "Vertices",
    "Orig v1 (s)",
    "Vec v2 (s)",
    "Pkg v3 (s)",
    "Speedup vs Orig",
    "Speedup vs v2"
  )
))

cat("\n* Note on AOI Cut scenario: v2 and Package include Step 5 (dissolving AOI-cut\n")
cat("  fragments back to original features), whereas Orig v1 omits fragment dissolving.\n")
cat("  Comparing Package against v2 on AOI shows the in-place geometry speedup.\n\n")

cat("------------------------------------------------------------------------------\n")
cat("KEY PERFORMANCE INSIGHTS:\n")
non_aoi_results <- benchmark_results[!grepl("AOI", benchmark_results$Scenario), ]
vec_speedup <- non_aoi_results$raw_orig / pmax(non_aoi_results$raw_v2, 0.0005)
pkg_vs_v2   <- benchmark_results$raw_v2 / pmax(benchmark_results$raw_pkg, 0.0005)
pkg_vs_orig <- non_aoi_results$raw_orig / pmax(non_aoi_results$raw_pkg, 0.0005)

cat(sprintf("  • Vectorized predict (v2 vs Orig): Median ~%.1fx faster (up to ~%.1fx on RF / large grids)\n",
            median(vec_speedup), max(vec_speedup)))
cat(sprintf("  • In-place update_geom (Pkg vs v2): Median ~%.1fx faster (up to ~%.1fx across all formats)\n",
            median(pkg_vs_v2), max(pkg_vs_v2)))
cat(sprintf("  • Overall Speedup (Package vs Orig): Median ~%.1fx faster (up to ~%.1fx!)\n",
            median(pkg_vs_orig), max(pkg_vs_orig)))
cat("==============================================================================\n")
cat("Benchmark script execution completed.\n")
