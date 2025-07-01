# This helper script creates temporary data for tests.
# It will be sourced automatically by testthat.

library(testthat)
library(sf)
library(dplyr)
library(utils)
library(mockery)

# Helper function to create a dummy shapefile for testing
create_dummy_shp <- function(file_path, geometry_type = "POLYGON", has_crs = TRUE, has_area_old = FALSE) {
  if (geometry_type == "POLYGON") {
    # Create a simple polygon
    poly <- st_polygon(list(cbind(c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0))))
    sfc <- st_sfc(poly)
  } else if (geometry_type == "POINT") {
    # Create a simple point
    point <- st_point(c(0, 0))
    sfc <- st_sfc(point)
  } else {
    stop("Unsupported geometry type for dummy shapefile.")
  }

  if (has_crs) {
    st_crs(sfc) <- 4326 # WGS 84
  }

  df <- data.frame(id = 1)
  if (has_area_old) {
    df$area_old <- set_units(100, "m^2")
  }

  sf_obj <- st_sf(df, geometry = sfc)

  # Ensure the directory exists
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)

  st_write(sf_obj, file_path, delete_layer = TRUE, quiet = TRUE)
  return(file_path)
}

# Helper function to create a dummy GCP CSV file
create_dummy_gcp_csv <- function(path, data = NULL) {
  if (is.null(data)) {
    data <- data.frame(
      source_x = c(10, 20, 30),
      source_y = c(100, 110, 120),
      target_x = c(12, 23, 35),
      target_y = c(101, 112, 124)
    )
  }
  utils::write.csv(data, path, row.names = FALSE)
}

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
create_dummy_gcp_data <- function(n = 500) {
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


# This helper function creates mock model objects. It assigns a special
# test-only class, 'mock_pai_model', allowing us to define a custom S3 method
# for predict() that intercepts the call during testing.
create_mock_pai_model <- function(method) {

  # This is the simple function we want to execute when predict() is called in a test.
  predictor_func <- switch(method,
                           "lm" = function(newdata) {
                             data.frame(
                               dx = (newdata$source_x * 1.2) - newdata$source_x,
                               dy = (newdata$source_y * 0.8) - newdata$source_y
                             )
                           },
                           "helmert" = function(newdata) {
                             theta <- pi / 6
                             scale <- 1.1
                             x_new <- newdata$source_x * cos(theta) - newdata$source_y * sin(theta)
                             y_new <- newdata$source_x * sin(theta) + newdata$source_y * cos(theta)
                             data.frame(
                               dx = (x_new * scale) - newdata$source_x,
                               dy = (y_new * scale) - newdata$source_y
                             )
                           },
                           "gam" = function(newdata) {
                             data.frame(
                               dx = sin(newdata$source_x / 1e5) * 10,
                               dy = cos(newdata$source_y / 1e5) * 10
                             )
                           },
                           "rf" = function(newdata) {
                             data.frame(dx = 0, dy = 0)
                           }
  )

  # Build the mock object, assigning our new class first. This is key for S3.
  structure(
    list(
      # The 'method' element is still needed by the function being tested.
      method = method,
      # Store the simple prediction logic inside the object.
      predictor = predictor_func
    ),
    # The class vector tells R's S3 dispatch to look for predict.mock_pai_model first.
    class = c("mock_pai_model", "pai_model")
  )
}

# This is the core of the solution. We define a `predict` method for our test
# class. When `predict()` is called on a 'mock_pai_model' object during a test,
# this version runs instead of the real `predict.pai_model`, giving us full,
# predictable control.
predict.mock_pai_model <- function(object, newdata, ...) {
  # Execute the simple prediction function we stored inside the object.
  object$predictor(newdata)
}
