# Tests for apply_pai_model() with grid-based MULTIPOLYGON data
# These tests verify that feature count is preserved during transformation

# Helper function to create a grid of polygons
create_polygon_grid <- function(n_rows, n_cols, cell_size = 10, crs = 3857) {
    polygons <- vector("list", n_rows * n_cols)
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
        id = seq_len(n_rows * n_cols),
        geometry = sf::st_sfc(polygons, crs = crs)
    )
}

# Helper function to create a grid of multipolygons (each feature is a MULTIPOLYGON)
create_multipolygon_grid <- function(n_rows, n_cols, cell_size = 10, crs = 3857) {
    multipolygons <- vector("list", n_rows * n_cols)
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
        id = seq_len(n_rows * n_cols),
        geometry = sf::st_sfc(multipolygons, crs = crs)
    )
}


# Load test model
data(gcps)
test_model <- train_pai_model(gcps, pai_method = "gam")


# --- Test: Feature count preserved for small polygon grid ---
test_that("apply_pai_model preserves feature count for polygon grid (100 features)", {
    grid_sf <- create_polygon_grid(10, 10)
    expect_equal(nrow(grid_sf), 100)

    corrected_sf <- apply_pai_model(test_model, grid_sf)

    expect_s3_class(corrected_sf, "sf")
    expect_equal(nrow(corrected_sf), 100)
    expect_equal(
        as.character(unique(sf::st_geometry_type(corrected_sf))),
        "POLYGON"
    )
})


# --- Test: Feature count preserved for multipolygon grid ---
test_that("apply_pai_model preserves feature count for multipolygon grid (100 features)", {
    grid_sf <- create_multipolygon_grid(10, 10)
    expect_equal(nrow(grid_sf), 100)

    corrected_sf <- apply_pai_model(test_model, grid_sf)

    expect_s3_class(corrected_sf, "sf")
    expect_equal(nrow(corrected_sf), 100)
    expect_equal(
        as.character(unique(sf::st_geometry_type(corrected_sf))),
        "MULTIPOLYGON"
    )
})


# --- Test: Feature count preserved for larger grid (2500 features) ---
test_that("apply_pai_model preserves feature count for large grid (2500 features)", {
    grid_sf <- create_polygon_grid(50, 50)
    expect_equal(nrow(grid_sf), 2500)

    corrected_sf <- apply_pai_model(test_model, grid_sf)

    expect_s3_class(corrected_sf, "sf")
    expect_equal(nrow(corrected_sf), 2500)
})


# --- Test: Feature count preserved for large multipolygon grid (2500 features) ---
test_that("apply_pai_model preserves feature count for large multipolygon grid (2500 features)", {
    grid_sf <- create_multipolygon_grid(50, 50)
    expect_equal(nrow(grid_sf), 2500)

    corrected_sf <- apply_pai_model(test_model, grid_sf)

    expect_s3_class(corrected_sf, "sf")
    expect_equal(nrow(corrected_sf), 2500)
})


# --- Test: Geometry type is preserved ---
test_that("apply_pai_model preserves geometry type for multipolygons", {
    grid_sf <- create_multipolygon_grid(5, 5)

    corrected_sf <- apply_pai_model(test_model, grid_sf)

    original_types <- as.character(sf::st_geometry_type(grid_sf))
    corrected_types <- as.character(sf::st_geometry_type(corrected_sf))

    expect_equal(original_types, corrected_types)
})


# --- Test: IDs are preserved in order ---
test_that("apply_pai_model preserves feature IDs in order", {
    grid_sf <- create_polygon_grid(10, 10)

    corrected_sf <- apply_pai_model(test_model, grid_sf)

    expect_equal(corrected_sf$id, grid_sf$id)
})


# --- Test: Mixed geometry types (POLYGON + MULTIPOLYGON) ---
test_that("apply_pai_model preserves feature count for mixed geometry sf", {
    # Create a mix of POLYGON and MULTIPOLYGON
    poly1 <- sf::st_polygon(list(matrix(c(0, 0, 10, 0, 10, 10, 0, 10, 0, 0), ncol = 2, byrow = TRUE)))
    poly2 <- sf::st_polygon(list(matrix(c(20, 0, 30, 0, 30, 10, 20, 10, 20, 0), ncol = 2, byrow = TRUE)))
    mpoly <- sf::st_multipolygon(list(
        list(matrix(c(40, 0, 50, 0, 50, 10, 40, 10, 40, 0), ncol = 2, byrow = TRUE))
    ))

    mixed_sf <- sf::st_sf(
        id = 1:3,
        geometry = sf::st_sfc(poly1, poly2, mpoly, crs = 3857)
    )

    expect_equal(nrow(mixed_sf), 3)

    corrected_sf <- apply_pai_model(test_model, mixed_sf)

    expect_s3_class(corrected_sf, "sf")
    expect_equal(nrow(corrected_sf), 3)
})


# --- Test: Feature count with AOI that covers all features ---
test_that("apply_pai_model preserves feature count when AOI covers all features", {
    grid_sf <- create_polygon_grid(10, 10)
    # AOI that covers the entire grid (0-100 in both directions)
    aoi <- sf::st_sf(
        id = 1,
        geometry = sf::st_sfc(
            sf::st_polygon(list(matrix(c(-10, -10, 110, -10, 110, 110, -10, 110, -10, -10), ncol = 2, byrow = TRUE))),
            crs = 3857
        )
    )

    corrected_sf <- apply_pai_model(test_model, grid_sf, aoi = aoi)

    expect_s3_class(corrected_sf, "sf")
    expect_equal(nrow(corrected_sf), 100)
})


# --- Test: Feature count preserved when AOI splits and dissolves features ---
test_that("apply_pai_model preserves feature count when AOI cuts through features (dissolve)", {
    grid_sf <- create_polygon_grid(10, 10) # 100 features, each 10x10 from (0,0) to (100,100)

    # AOI that cuts through the middle of the grid
    # This will intersect some polygons partially, but dissolve should merge them back
    aoi <- sf::st_sf(
        id = 1,
        geometry = sf::st_sfc(
            sf::st_polygon(list(matrix(c(0, 0, 55, 0, 55, 55, 0, 55, 0, 0), ncol = 2, byrow = TRUE))),
            crs = 3857
        )
    )

    original_count <- nrow(grid_sf)
    corrected_sf <- apply_pai_model(test_model, grid_sf, aoi = aoi)

    # Print diagnostic info
    message(sprintf("Original features: %d, After AOI correction: %d", original_count, nrow(corrected_sf)))

    # Feature count should be preserved after dissolve
    expect_s3_class(corrected_sf, "sf")
    expect_equal(nrow(corrected_sf), 100) # Changed from just checking validity to verifying count

    # The function should return valid geometries
    expect_true(all(sf::st_is_valid(corrected_sf)))
})


# --- Test: No AOI vs AOI covering entire area should give same feature count ---
test_that("apply_pai_model gives same feature count with full-coverage AOI as without AOI", {
    grid_sf <- create_polygon_grid(10, 10)

    # No AOI
    corrected_no_aoi <- apply_pai_model(test_model, grid_sf)

    # AOI covering everything
    aoi <- sf::st_sf(
        id = 1,
        geometry = sf::st_sfc(
            sf::st_polygon(list(matrix(c(-100, -100, 200, -100, 200, 200, -100, 200, -100, -100), ncol = 2, byrow = TRUE))),
            crs = 3857
        )
    )
    corrected_with_aoi <- apply_pai_model(test_model, grid_sf, aoi = aoi)

    expect_equal(nrow(corrected_no_aoi), nrow(corrected_with_aoi))
    expect_equal(nrow(corrected_no_aoi), 100)
})
