#
# This script defines various spatial transformation models and compiles them
# into a single list for use in the package. the first part is about univariate
# models (i.e. lm, rf, tps) and the second about bivariate models (i.e. gam_biv,
# helmert)

# ---- part 1: univariate models ----
{
# Define Model 1: Linear Model (lm)
lm_model <- list(
  label = "Linear Model",
  library = NULL,
  modelType = "univariate",
  fit = function(x, y, ...) {
    dat <- cbind(y, x)
    names(dat)[1] <- "outcome"
    stats::lm(outcome ~ ., data = dat, ...)
  },
  predict = function(modelFit, newdata, ...) {
    stats::predict(modelFit, newdata = newdata, ...)
  }
)


# Define Model 3: TPS
tps_model <- list(
  label = "TPS",
  library = "fields",
  modelType = "univariate",
  fit = function(x, y, ...) {
    fields::Tps(x = as.matrix(x), Y = y, ...)
  },
  predict = function(modelFit, newdata, ...) {
    predict(modelFit, x = as.matrix(newdata), ...)
  }
)

}
# ---- part 2: bivariate models ----
{
# Define Model 4: Bivariate GAM (gam_biv)
gam_biv_model <- list(
  label = "Bivariate GAM",
  library = "mgcv",
  modelType = "bivariate",
  fit = function(dat, k = NULL, ...) {
    n_unique <- nrow(unique(dat[, c("source_x", "source_y")]))
    if (is.null(k)) {
      k_adaptive <- max(3, min(29, floor(n_unique * 0.6)))
    } else {
      k_adaptive <- min(k, n_unique - 1)
    }
    formula_list <- list(
      stats::as.formula(sprintf("dx ~ s(source_x, source_y, k = %d)", k_adaptive)),
      stats::as.formula(sprintf("dy ~ s(source_x, source_y, k = %d)", k_adaptive))
    )
    mgcv::gam(formula_list, data = dat, family = mgcv::mvn(d = 2), ...)
  },
  predict = function(modelFit, newdata, ...) {
    # bivariate prediction
    stats::predict(modelFit, newdata = newdata, ...)
  }
)

# Define Model 5: Helmert model
helmert_model <- list(
  label = "Helmert Model",
  library = NULL,
  modelType = "bivariate",
  fit = function(dat, ...) {
    helmert(dat$source_x, dat$source_y,
            dat$target_x, dat$target_y, ...)

  },
  predict = function(modelFit, newdata, ...) {

    preds  <- stats::predict(modelFit, newdata = newdata, ...)
    pred_dx <- preds[, 1] - newdata$source_x
    pred_dy <- preds[, 2] - newdata$source_y

    return(data.frame(dx = pred_dx, dy = pred_dy, row.names = row.names(newdata)))
  }
)
}
# ---- save models as internal data ----

# Combine all model definitions into a single named list
pai_model_list <- list(
  gam_biv = gam_biv_model,
  helmert = helmert_model,
  lm = lm_model,
  tps = tps_model
)

# Save the list to the internal package data file
usethis::use_data(pai_model_list, internal = TRUE, overwrite = TRUE)

