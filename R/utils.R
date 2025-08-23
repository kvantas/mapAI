#' Validate Input for Helmert  and create_gcps function
#' @keywords internal
#' @noRd
input_validation <- function(source_x, source_y,
                             target_x, target_y) {

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

#' Validate newdata for predict.pai_train function
#' @keywords internal
#' @noRd
new_data_validation <- function(newdata) {

  if (!all(c("source_x", "source_y") %in% names(newdata))) {
    stop("`newdata` must contain 'source_x' and 'source_y' columns.",
         call. = FALSE)
  }

  if (any(!is.finite(c(newdata$source_x, newdata$source_y)))) {
    stop("All coordinates in `newdata` must be finite numbers.",
         call. = FALSE)
  }

}
