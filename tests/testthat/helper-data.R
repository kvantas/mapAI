# This helper script creates temporary data for tests.
# It will be sourced automatically by testthat.

# Use a temporary directory for all test outputs
TEST_TEMP_DIR <- tempdir()

# Function to create a simple square polygon for area tests
create_test_polygon <- function(path) {
  poly_coords <- matrix(c(0,0, 0,10, 10,10, 10,0, 0,0), ncol = 2, byrow = TRUE)
  poly <- sf::st_polygon(list(poly_coords))
  poly_sf <- sf::st_sf(id = 1, geometry = sf::st_sfc(poly, crs = 3857))
  sf::st_write(poly_sf, path, delete_layer = TRUE, quiet = TRUE)
  return(path)
}

# Helper function to create dummy gcp_data
create_dummy_gcp_data <- function(n = 1000) { # Increased n further for more robust GAM fitting
  set.seed(123) # for reproducibility of dummy data
  data.frame(
    source_x = runif(n, 0, 1000), # Increased range for source_x and source_y
    source_y = runif(n, 0, 1000),
    dx = rnorm(n, 0, 5), # Increased variance for dx and dy
    dy = rnorm(n, 0, 5)
  ) %>%
    sf::st_as_sf(coords = c("source_x", "source_y"), crs = 4326, remove = FALSE)
}

# Generate the main demo data once for all tests
DEMO_FILES <- create_demo_data(output_dir = TEST_TEMP_DIR)
POLYGON_FILE <- create_test_polygon(file.path(TEST_TEMP_DIR, "test_polygon.shp"))
