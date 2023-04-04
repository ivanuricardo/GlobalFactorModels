library(rTensor)
library(tensorFun)
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

first_lasso_var <- sparseVAR(scale(normal_var_data[1:(113),]), p = 1, VARpen = "L1",
                             selection = "cv")
first_forecast_var <- directforecast(first_lasso_var)
var_forecast_residuals <- t(matrix((first_forecast_var - var_test[1,])^2))

# Rolling Window VAR

num_forecasts <- 49

for (sim in 2:num_forecasts) {
  sim_lasso_var <- sparseVAR(scale(normal_var_data[(1+sim):(112+sim),]), p = 1,
                             VARpen = "L1",
                             selection = "cv")
  sim_forecast_var <- directforecast(sim_lasso_var)
  var_forecast_residuals <- rbind(var_forecast_residuals,
                                  (sim_forecast_var - var_test[sim,])^2)
  print(sim)
}

# Plot the residuals
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))

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

num_forecasts <- 49

for (sim in 2:num_forecasts) {
  sim_favar <- VAR(favar_data[(1+sim):(112+sim),], p = 2, type = 'none')
  sim_prediction_favar <- predict(sim_favar, n.ahead = 1)
  sim_forecast_favar <- sapply(sim_prediction_favar$fcst, `[`, 1)
  favar_forecast_residuals <- rbind(favar_forecast_residuals,
                                    (sim_forecast_favar - favar_test[sim,])^2)
  print(sim)
}

par(mfrow = c(3, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))

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
favar_res_plot_title <- title("Squared Residual Plots for FAVAR")

# GFAVAR
gfavar_data <- readRDS("data/GFAVARdata.rds")
lags_upbound_BIC(gfavar_data)

gfavar_train <- gfavar_data[1:113,]
gfavar_test <- gfavar_data[114:161,]
first_gfavar <- VAR(gfavar_train, type = "none", p = 3)
first_prediction_gfavar <- predict(first_gfavar, n.ahead = 1)
first_forecast_gfavar <- sapply(first_prediction_gfavar$fcst, `[`, 1)
gfavar_forecast_residuals <- data.frame(t(matrix((first_forecast_gfavar -
                                                    gfavar_test[1,])^2)))

# Rolling Window GFAVAR

num_forecasts <- 49

for (sim in 2:num_forecasts) {
  sim_gfavar <- VAR(full_data_matrix[(1+sim):(112+sim),], p = 3, type = 'none')
  sim_prediction_gfavar <- predict(sim_gfavar, n.ahead = 1)
  sim_forecast_gfavar <- sapply(sim_prediction_gfavar$fcst, `[`, 1)
  gfavar_res <- data.frame((sim_forecast_gfavar - gfavar_test[sim,])^2)
  colnames(gfavar_res) <- colnames(gfavar_forecast_residuals)
  gfavar_forecast_residuals <- rbind(gfavar_forecast_residuals, gfavar_res)
  print(sim)
}

par(mfrow = c(3, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))

gfavar_res_plot1 <- plot(as.numeric(gfavar_forecast_residuals[,1]), type = 'l',
                         main = "NL GDP", xlab = "Time", ylab = "Residual")
gfavar_res_plot2 <- plot(as.numeric(gfavar_forecast_residuals[,2]), type = 'l',
                         main = "NL Inflation", xlab = "Time", ylab = "Residual")
gfavar_res_plot3 <- plot(as.numeric(gfavar_forecast_residuals[,3]), type = 'l',
                         main = "NL EP Index", xlab = "Time", ylab = "Residual")
gfavar_res_plot4 <- plot(as.numeric(gfavar_forecast_residuals[,11]), type = 'l',
                         main = "Oil Prices", xlab = "Time", ylab = "Residual")
gfavar_res_plot5 <- plot(as.numeric(gfavar_forecast_residuals[,12]), type = 'l',
                         main = "Materials Prices", xlab = "Time", ylab = "Residual")
gfavar_res_plot6 <- plot(as.numeric(gfavar_forecast_residuals[,13]), type = 'l',
                         main = "Metal Prices", xlab = "Time", ylab = "Residual")






