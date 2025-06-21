test_that("read_gcps successfully reads valid GCP file and returns sf object", {
  # Create a temporary CSV file
  temp_gcp_path <- tempfile(fileext = ".csv")
  create_dummy_gcp_csv(temp_gcp_path)

  # Define a valid CRS
  valid_crs <- 3857 # WGS 84 / Pseudo-Mercator

  # Call the function
  gcp_sf <- read_gcps(gcp_path = temp_gcp_path, crs = valid_crs)

  # Assertions
  expect_s3_class(gcp_sf, "sf")
  expect_true("dx" %in% names(gcp_sf))
  expect_true("dy" %in% names(gcp_sf))
  expect_equal(gcp_sf$dx, c(2, 3, 5))
  expect_equal(gcp_sf$dy, c(1, 2, 4))
  expect_equal(sf::st_crs(gcp_sf)$epsg, valid_crs)

  # Clean up
  unlink(temp_gcp_path)
})

test_that("read_gcps throws error if GCP file not found", {
  non_existent_path <- "non_existent_file.csv"
  expect_error(read_gcps(gcp_path = non_existent_path, crs = 3857),
               "GCP file not found at the specified path:")
})

test_that("read_gcps throws error if CRS is missing", {
  temp_gcp_path <- tempfile(fileext = ".csv")
  create_dummy_gcp_csv(temp_gcp_path)

  expect_error(read_gcps(gcp_path = temp_gcp_path),
               "A coordinate reference system \\(CRS\\) must be provided for the GCPs.")

  unlink(temp_gcp_path)
})

test_that("read_gcps throws error if required columns are missing", {
  temp_missing_cols_path <- tempfile(fileext = ".csv")
  data_missing_cols <- data.frame(
    x = c(1, 2),
    y = c(3, 4),
    target_x = c(5, 6),
    target_y = c(7, 8)
  )
  utils::write.csv(data_missing_cols, temp_missing_cols_path, row.names = FALSE)

  expect_error(read_gcps(gcp_path = temp_missing_cols_path, crs = 3857),
               "GCP file must contain columns: source_x, source_y, target_x, target_y")

  unlink(temp_missing_cols_path)
})
