#' @title Check for Topological Inversion in Triangulated PAI Models
#' @description Evaluates the signed Jacobian determinant and area ratio of all
#'   triangles in a triangulated PAI model or GCP dataset to detect topological
#'   fold-overs (triangle flipping) and dimensional collapses.
#'
#' @details
#' In piecewise affine and triangulated rubbersheeting, each source triangle
#' \eqn{T_i = (\mathbf{s}_1, \mathbf{s}_2, \mathbf{s}_3)} is mapped to a target
#' triangle \eqn{T'_i = (\mathbf{t}_1, \mathbf{t}_2, \mathbf{t}_3)}. The local
#' transformation Jacobian determinant is the ratio of signed areas:
#' \deqn{\det(\mathbf{J}_i) = \frac{\operatorname{Area}(T'_i)}{\operatorname{Area}(T_i)}}
#'
#' If \eqn{\det(\mathbf{J}_i) \le 0}, the triangle has undergone a topological
#' inversion (flipped orientation from counterclockwise to clockwise or
#' collapsed into a line), producing self-intersecting geometries and overlapping
#' coordinates in transformed vector or raster maps.
#'
#' @param object A trained `pai_model` object (such as fitted with `"tin_linear"`,
#'   `"hybrid_helmert_tin"`, or `"hybrid_affine_tin"`), or a `gcp` object.
#' @param plot Logical; if `TRUE`, returns a `ggplot` visualization displaying
#'   the triangulation mesh with inverted triangles highlighted in red. Defaults
#'   to `FALSE`.
#'
#' @return A data frame of class `c("tin_inversion", "data.frame")` detailing each
#'   triangle's vertex indices, signed areas, Jacobian determinant, and inversion
#'   status. If `plot = TRUE`, a `ggplot` object is returned instead.
#'
#' @import ggplot2
#' @importFrom rlang .data
#' @export
#' @examples
#' demo_data <- create_demo_data()
#' model <- train_pai_model(demo_data$gcp, method = "tin_linear")
#' inv_check <- check_tin_inversion(model)
#' head(inv_check)
check_tin_inversion <- function(object, plot = FALSE) {
  gcp_dat <- NULL
  tri_mesh <- NULL

  if (inherits(object, "pai_model")) {
    gcp_dat <- object$gcp
    if (inherits(object$model, "tin_linear_fit") ||
        inherits(object$model, "hybrid_helmert_tin_fit") ||
        inherits(object$model, "hybrid_affine_tin_fit")) {
      tri_mesh <- object$model$mesh
    }
  } else if (inherits(object, "gcp") || (is.data.frame(object) && all(c("source_x", "source_y", "target_x", "target_y") %in% names(object)))) {
    gcp_dat <- object
  } else {
    stop("`object` must be a trained `pai_model` or a `gcp` data frame.", call. = FALSE)
  }

  if (is.null(tri_mesh)) {
    tri_mesh <- interp::tri.mesh(x = gcp_dat$source_x, y = gcp_dat$source_y)
  }

  tris <- interp::triangles(tri_mesh)
  v1 <- tris[, 1]
  v2 <- tris[, 2]
  v3 <- tris[, 3]

  src_area <- 0.5 * ((gcp_dat$source_x[v2] - gcp_dat$source_x[v1]) * (gcp_dat$source_y[v3] - gcp_dat$source_y[v1]) -
                     (gcp_dat$source_x[v3] - gcp_dat$source_x[v1]) * (gcp_dat$source_y[v2] - gcp_dat$source_y[v1]))

  tgt_area <- 0.5 * ((gcp_dat$target_x[v2] - gcp_dat$target_x[v1]) * (gcp_dat$target_y[v3] - gcp_dat$target_y[v1]) -
                     (gcp_dat$target_x[v3] - gcp_dat$target_x[v1]) * (gcp_dat$target_y[v2] - gcp_dat$target_y[v1]))

  det_J <- tgt_area / src_area
  is_inv <- (det_J <= 0)

  res_df <- data.frame(
    triangle_id = seq_len(nrow(tris)),
    v1 = v1,
    v2 = v2,
    v3 = v3,
    source_area = src_area,
    target_area = tgt_area,
    det_J = det_J,
    is_inverted = is_inv
  )

  attr(res_df, "n_triangles") <- nrow(tris)
  attr(res_df, "n_inverted") <- sum(is_inv, na.rm = TRUE)
  attr(res_df, "inversion_rate") <- mean(is_inv, na.rm = TRUE)
  class(res_df) <- c("tin_inversion", "data.frame")

  if (isTRUE(plot)) {
    poly_list <- lapply(seq_len(nrow(tris)), function(i) {
      data.frame(
        triangle_id = i,
        x = c(gcp_dat$source_x[v1[i]], gcp_dat$source_x[v2[i]], gcp_dat$source_x[v3[i]]),
        y = c(gcp_dat$source_y[v1[i]], gcp_dat$source_y[v2[i]], gcp_dat$source_y[v3[i]]),
        is_inverted = is_inv[i]
      )
    })
    poly_df <- do.call(rbind, poly_list)

    n_inv_count <- sum(is_inv, na.rm = TRUE)
    subtitle_text <- if (n_inv_count == 0) {
      "All triangular facets preserve orientation (det(J) > 0)"
    } else {
      sprintf("WARNING: %d inverted facet(s) detected (det(J) <= 0)", n_inv_count)
    }

    plt <- ggplot2::ggplot(poly_df, ggplot2::aes(x = .data$x, y = .data$y, group = .data$triangle_id)) +
      ggplot2::geom_polygon(ggplot2::aes(fill = .data$is_inverted), color = "grey40", linewidth = 0.3) +
      ggplot2::scale_fill_manual(
        name = "Status",
        values = c("FALSE" = "#e0f3f8", "TRUE" = "#d73027"),
        labels = c("FALSE" = "Valid (det(J) > 0)", "TRUE" = "Inverted (det(J) <= 0)")
      ) +
      ggplot2::geom_point(data = gcp_dat, ggplot2::aes(x = .data$source_x, y = .data$source_y),
                          inherit.aes = FALSE, size = 1.2, color = "black") +
      ggplot2::labs(
        title = "Delaunay Triangulation Inversion Diagnostic",
        subtitle = subtitle_text,
        x = "Source X",
        y = "Source Y"
      ) +
      ggplot2::theme_minimal()

    return(plt)
  }

  return(res_df)
}

#' @export
print.tin_inversion <- function(x, ...) {
  n_tri <- attr(x, "n_triangles")
  n_inv <- attr(x, "n_inverted")
  rate <- attr(x, "inversion_rate")
  cat("TIN Topological Inversion Assessment\n")
  cat("  Total Triangular Facets: ", n_tri, "\n")
  cat("  Inverted Facets (det(J) <= 0): ", n_inv, sprintf("(%.1f%%)\n", rate * 100))
  if (n_inv > 0) {
    cat("  Inverted Triangle IDs: ", paste(x$triangle_id[x$is_inverted], collapse = ", "), "\n")
  }
  cat("\nFirst few triangles:\n")
  print.data.frame(head(as.data.frame(x), 6))
  invisible(x)
}
