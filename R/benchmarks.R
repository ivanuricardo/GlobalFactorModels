library(HDeconometrics)
library(bigtime)
library(HDGCvar)
library(vars)

set.seed(20230322)

####### VAR estimation
normal_var_data <- readRDS("data/VARdata.rds")
var_train <- normal_var_data[1:113,]
var_test <- normal_var_data[114:161,]

# Determine upper bound 
lags_upbound_BIC(var_train)  # 1

first_lasso_var <- sparseVAR(scale(var_train), p = 1, VARpen = "L1",
                             selection = "cv")
first_forecast_var <- directforecast(first_lasso_var)
var_forecast_residuals <- t(matrix((first_forecast_var - var_test[1,])^2))

# Rolling Window VAR

num_forecasts <- 49

# Ignore the error at the end
for (sim in 2:num_forecasts) {
  sim_lasso_var <- sparseVAR(scale(normal_var_data[(sim):(112+sim),]), p = 1,
                             VARpen = "L1",
                             selection = "cv")
  sim_forecast_var <- directforecast(sim_lasso_var)
  var_forecast_residuals <- rbind(var_forecast_residuals,
                                  (sim_forecast_var - var_test[sim,])^2)
}

# Plot the residuals
par(mfrow = c(2, 3), mar = c(4, 4, 1, 1), oma = c(0, 0, 2, 0))

var_res_plot1 <- plot(var_forecast_residuals[,1], type = 'l', main = "NL GDP",
                      xlab = "Time", ylab = "Residual")
var_res_plot2 <- plot(var_forecast_residuals[,2], type = 'l', main = "NL Inflation",
                      xlab = "Time", ylab = "Residual")
var_res_plot3 <- plot(var_forecast_residuals[,3], type = 'l', main = "NL EP Index",
                      xlab = "Time", ylab = "Residual")
var_res_plot4 <- plot(var_forecast_residuals[,11], type = 'l', main = "Oil Prices",
                      xlab = "Time", ylab = "Residual")
var_res_plot5 <- plot(var_forecast_residuals[,12], type = 'l', main = "Materials Prices",
                      xlab = "Time", ylab = "Residual")
var_res_plot6 <- plot(var_forecast_residuals[,13], type = 'l', main = "Metal Prices",
                      xlab = "Time", ylab = "Residual")

# Root Mean of each series
var_resMeans <- colMeans(var_forecast_residuals) %>% sqrt()
saveRDS(var_resMeans, "var_resMeans.rds")

####### A normal FAVAR
favar_data <- readRDS("data/FAVARdata.rds")

# Lag length?
lags_upbound_BIC(favar_data) # 2

# Estimation
favar_train <- favar_data[1:113,]
favar_test <- favar_data[114:161,]
first_favar <- VAR(favar_train, type = "none", p = 2)
first_prediction_favar <- predict(first_favar, n.ahead = 1)
first_forecast_favar <- sapply(first_prediction_favar$fcst, `[`, 1)
favar_forecast_residuals <- t(matrix((first_forecast_favar - favar_test[1,])^2))

# Rolling Window FAVAR

# ignore the last error...
for (sim in 2:num_forecasts) {
  sim_favar <- VAR(favar_data[(sim):(112+sim),], p = 2, type = 'none')
  sim_prediction_favar <- predict(sim_favar, n.ahead = 1)
  sim_forecast_favar <- sapply(sim_prediction_favar$fcst, `[`, 1)
  favar_forecast_residuals <- rbind(favar_forecast_residuals,
                                    (sim_forecast_favar - favar_test[sim,])^2)
}

par(mfrow = c(2, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))

favar_res_plot1 <- plot(favar_forecast_residuals[,1], type = 'l', main = "NL GDP",
                        xlab = "Time", ylab = "Residual")
favar_res_plot2 <- plot(favar_forecast_residuals[,2], type = 'l', main = "NL Inflation",
                        xlab = "Time", ylab = "Residual")
favar_res_plot3 <- plot(favar_forecast_residuals[,3], type = 'l', main = "NL EP Index",
                        xlab = "Time", ylab = "Residual")
favar_res_plot4 <- plot(favar_forecast_residuals[,11], type = 'l', main = "Oil Prices",
                        xlab = "Time", ylab = "Residual")
favar_res_plot5 <- plot(favar_forecast_residuals[,12], type = 'l', main = "Materials Prices",
                        xlab = "Time", ylab = "Residual")
favar_res_plot6 <- plot(favar_forecast_residuals[,13], type = 'l', main = "Metal Prices",
                        xlab = "Time", ylab = "Residual")

# Root Mean of each series
favar_resMeans <- colMeans(favar_forecast_residuals) %>% sqrt()
saveRDS(favar_resMeans, "favar_resMeans.rds")
