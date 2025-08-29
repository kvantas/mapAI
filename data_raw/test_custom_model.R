library(LSCfit)
library(mapAI)

data(basel_frickthal)
plot(basel_frickthal$gcp, exaggeration_factor = 2)

# create a custom univariate model
custom_model <- list(
  label = "GAM Model using REML",
  library = "mgcv",
  modelType = "univariate",
  fit = function(x, y, ...) {
    dat <- cbind(y, x)
    names(dat)[1] <- "outcome"

    mgcv::gam(outcome ~ s(source_x) + s(source_y),
            method = "REML",
            data = dat,
            ...)
  },
  predict = function(modelFit, newdata, ...) {
    stats::predict(modelFit, newdata = newdata, ...)
  }
)

# create a custom bivariate model based on LSC
lsc_model <- list(
  label = "Least squares collocation",
  library = "LSCfit",
  modelType = "bivariate",
  fit = function(dat, ...) {

    # create data frames for training
    primary_coords <- data.frame(x = dat$source_x,
                                 y = dat$source_y)
    secondary_coords <- data.frame(X = dat$target_x,
                                   Y = dat$target_y)

    # estimate covariance parameters
    cov_params <- LSCfit::est_cov_wls(primary_coords, secondary_coords, nu = 0.5, cutoff_factor = .5)

    LSCfit::lsc_train(primary_coords, secondary_coords, cov_params$params)

  },
  predict = function(modelFit, newdata, ...) {
    # bivariate prediction
    newdata <- data.frame(x = newdata$source_x,
                          y = newdata$source_y)
    # return dx and dy
    preds <- LSCfit:::predict.lsc(modelFit, newdata = newdata)
    pred_dx <- preds[, 1] - newdata$x
    pred_dy <- preds[, 2] - newdata$y

    return(data.frame(dx = pred_dx,
                      dy = pred_dy,
                      row.names = row.names(newdata)))

  }
)

# fit the  models
gam_model <- train_pai_model(gcp_data = basel_frickthal$gcp, method = "gam_biv")
custom_fitted <- train_pai_model(gcp_data = basel_frickthal$gcp, method = custom_model)
lsc_fitted <- train_pai_model(gcp_data = basel_frickthal$gcp, method = lsc_model)
tps_fitted <- train_pai_model(gcp_data = basel_frickthal$gcp, method = "tps")


plot(lsc_fitted$model)

surface(gam_model)
surface(custom_fitted)
surface(lsc_fitted)
surface(tps_fitted)

residuals(gam_model)
residuals(lsc_fitted)

lsc_dda <- analyze_distortion(lsc_fitted)
summary(lsc_dda)
plot(lsc_dda, metric = "area_scale")

gam_dda <- analyze_distortion(gam_model)
summary(gam_dda)
plot(gam_dda, metric = "area_scale")

map_gam <- transform_map(gam_model, basel_frickthal$grid)
plot(map_gam)

map_lsc <- transform_map(lsc_fitted, basel_frickthal$grid)
plot(map_lsc)


# compare results
cv_gam <- cv_pai_model(basel_frickthal$gcp, "gam_biv", seed = 1, k = 10)
cv_custom <- cv_pai_model(basel_frickthal$gcp, custom_model, seed = 1, k = 10)
cv_lsc <- cv_pai_model(basel_frickthal$gcp, lsc_model, seed = 1, k = 10)
cv_tps <- cv_pai_model(basel_frickthal$gcp, "tps", seed = 1, k = 10)

CV_results <- rbind(cv_gam$summary, cv_custom$summary, cv_lsc$summary, cv_tps$summary)
print(CV_results)
