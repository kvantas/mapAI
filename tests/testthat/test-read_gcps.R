test_that("read_gcps() works and handles errors", {
  withr::with_tempdir({
    demo_files <- create_demo_data(output_dir = ".")
    gcp_data <- read_gcps(gcp_path = demo_files$gcp_path)
    expect_s3_class(gcp_data, "sf")
    expect_true(all(c("source_x", "dx", "dy", "geometry") %in% names(gcp_data)))
  })
})



test_that("read_gcps successfully reads valid GCP file and returns sf object", {
  temp_dir <- withr::local_tempdir()
  # Create a temporary CSV file
  temp_gcp_path <- file.path(temp_dir, "gcp.csv")
  create_dummy_gcp_csv(temp_gcp_path)

  # Call the function
  gcp_sf <- read_gcps(gcp_path = temp_gcp_path)

  # Assertions
  expect_s3_class(gcp_sf, "sf")
  expect_true("dx" %in% names(gcp_sf))
  expect_true("dy" %in% names(gcp_sf))
  expect_equal(gcp_sf$dx, c(2, 3, 5))
  expect_equal(gcp_sf$dy, c(1, 2, 4))
})

test_that("read_gcps throws error if GCP file not found", {
  non_existent_path <- "non_existent_file.csv"
  expect_error(read_gcps(gcp_path = non_existent_path),
               "GCP file not found at the specified path:")
})

test_that("read_gcps throws error if required columns are missing", {
  temp_dir <- withr::local_tempdir()
  temp_missing_cols_path <- file.path(temp_dir, "missing_cols.csv")
  data_missing_cols <- data.frame(
    x = c(1, 2),
    y = c(3, 4),
    target_x = c(5, 6),
    target_y = c(7, 8)
  )
  utils::write.csv(data_missing_cols, temp_missing_cols_path, row.names = FALSE)

  expect_error(read_gcps(gcp_path = temp_missing_cols_path),
               "GCP file must contain columns: source_x, source_y, target_x, target_y")
})
