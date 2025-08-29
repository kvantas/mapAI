library(mapAI)

demo_data <- create_demo_data()

# fit a bivariate GAM model
gam_model <- train_pai_model(gcp_data = demo_data$gcp, method = "gam_biv")

# create a custom model
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

# fit the custom model
custom_fitted <- train_pai_model(gcp_data = demo_data$gcp, method = custom_model)

# compare results
cv_gam <- cv_pai_model(demo_data$gcp, "gam_biv", seed = 1, k = 5)
cv_custom <- cv_pai_model(demo_data$gcp, custom_model, seed = 1, k = 5)
print(cv_gam)
print(cv_custom)
