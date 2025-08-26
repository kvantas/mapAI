test_that("Function executes with default parameters and creates files", {

  demo_data <- create_demo_data()


  # Check that the returned object is a list with the correct names and objects
  expect_type(demo_data, "list")
  expect_named(demo_data, c("gcp", "map"))

  expect_s3_class(demo_data$gcp, c("gcp", "data.frame"))
  expect_s3_class(demo_data$map, c("sf", "data.frame"))

  expect_named(demo_data$gcp,
               c("source_x", "source_y", "target_x", "target_y", "dx", "dy"))
  expect_named(demo_data$map, c("id", "geom"))


})

test_that("All distortion types run without error", {
  # Test each type in a loop to keep code DRY
  for (type in c("helmert", "nonlinear", "complex")) {
    expect_type(create_demo_data(type = type), "list")
  }
})

test_that("Custom parameters are correctly applied", {

  # Test custom grid limits
  custom_limits <- c(1000, 1100, 2000, 2100)
  demo_data <- create_demo_data(grid_limits = custom_limits, seed = 1)

  # The "target" coordinates should be within the new custom limits
  expect_gte(min(demo_data$gcp$target_x), custom_limits[1])
  expect_lte(max(demo_data$gcp$target_x), custom_limits[2])
  expect_gte(min(demo_data$gcp$target_y), custom_limits[3])
  expect_lte(max(demo_data$gcp$target_y), custom_limits[4])
})

test_that("Seed ensures reproducibility", {

  # Generate two datasets with the same seed in different directories
  demo1 <- create_demo_data(seed = 12)
  demo2 <- create_demo_data(seed = 12)

  # The content of the GCP files should be identical
  expect_identical(demo1, demo2)

  # Generate a third dataset with a different seed
  demo3 <- create_demo_data(seed = 13)

  # The content should NOT be identical to the first one
  expect_false(identical(demo1, demo3))
})

test_that("Function stops with invalid `type` parameter", {
  # Expect the function to throw a specific error for a wrong type
  expect_error(
    create_demo_data(type = "invalid_type")
  )
})
