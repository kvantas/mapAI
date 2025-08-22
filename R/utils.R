#' Validate Input for Helmert  and create_gcps function
input_validation <- function(source_x, source_y, target_x, target_y) {
  if (length(source_x) != length(source_y) ||
      length(source_x) != length(target_x) ||
      length(source_x) != length(target_y)) {
    stop("All input vectors must have the same length.", call. = FALSE)
  }
  if (length(source_x) < 2) {
    stop("At least two points are required to compute a transformation.",
         call. = FALSE)
  }
  if (any(!is.finite(c(source_x, source_y, target_x, target_y)))) {
    stop("All input coordinates must be finite numbers.", call. = FALSE)
  }
  if (length(unique(source_x)) < 2 || length(unique(source_y)) < 2) {
    stop("Source points must not be co-located.", call. = FALSE)
  }
  if (length(unique(target_x)) < 2 || length(unique(target_y)) < 2) {
    stop("Target points must not be co-located.", call. = FALSE)
  }
}
