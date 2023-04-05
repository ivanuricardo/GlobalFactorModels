library(rTensor)
library(tensorFun)
library(HDeconometrics)
library(bigtime)
library(HDGCvar)
library(vars)
library(tensorTS)

set.seed(20230322)

####### Matrix DFM of Wang
matrixdfm_data <- readRDS("data/MATfactordata.rds")

lags_upbound_BIC(matrixdfm_data)  # 2

matrixdfm_train <- matrixdfm_data[1:113,]
matrixdfm_test <- matrixdfm_data[114:161,]
first_matdfm <- VAR(matrixdfm_train, type = "none", p = 2)
first_prediction_matdfm <- predict(first_matdfm, n.ahead = 1)
first_forecast_matdfm <- sapply(first_prediction_matdfm$fcst, `[`, 1)
matdfm_forecast_residuals <- data.frame(t(matrix((first_forecast_matdfm -
                                                    matrixdfm_test[1,])^2)))

num_forecasts <- 49

for (sim in 2:num_forecasts) {
  sim_matdfm <- VAR(matrixdfm_data[(sim):(112+sim),], p = 2, type = 'none')
  sim_prediction_matdfm <- predict(sim_matdfm, n.ahead = 1)
  sim_forecast_matdfm <- sapply(sim_prediction_matdfm$fcst, `[`, 1)
  matdfm_res <- data.frame((sim_forecast_matdfm - matrixdfm_test[sim,])^2)
  colnames(matdfm_res) <- colnames(matdfm_forecast_residuals)
  matdfm_forecast_residuals <- rbind(matdfm_forecast_residuals, matdfm_res)
}
matdfm_forecast_residuals <- matdfm_forecast_residuals[-nrow(matdfm_forecast_residuals),]

par(mfrow = c(2, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))

matdfm_res_plot1 <- plot(as.numeric(matdfm_forecast_residuals[,1]), type = 'l',
                         main = "NL GDP", xlab = "Time", ylab = "Residual")
matdfm_res_plot2 <- plot(as.numeric(matdfm_forecast_residuals[,2]), type = 'l',
                         main = "NL Inflation", xlab = "Time", ylab = "Residual")
matdfm_res_plot3 <- plot(as.numeric(matdfm_forecast_residuals[,3]), type = 'l',
                         main = "NL EP Index", xlab = "Time", ylab = "Residual")
matdfm_res_plot4 <- plot(as.numeric(matdfm_forecast_residuals[,11]), type = 'l',
                         main = "Oil Prices", xlab = "Time", ylab = "Residual")
matdfm_res_plot5 <- plot(as.numeric(matdfm_forecast_residuals[,12]), type = 'l',
                         main = "Materials Prices", xlab = "Time", ylab = "Residual")
matdfm_res_plot6 <- plot(as.numeric(matdfm_forecast_residuals[,13]), type = 'l',
                         main = "Metal Prices", xlab = "Time", ylab = "Residual")

# Boxplots

saveRDS(matdfm_forecast_residuals, "matdfm_forecast_residuals.rds")

####### Sparse Matrix DFM

first_sparse_matdfm <- sparseVAR(scale(matrixdfm_train), p = 2, VARpen = "L1",
                                 selection = "cv")
first_forecast_smatdfm <- directforecast(first_sparse_matdfm)
smatdfm_forecast_residuals <- data.frame(t(matrix((first_forecast_smatdfm -
                                                     matrixdfm_test[1,])^2)))

for (sim in 2:num_forecasts) {
  sim_lasso_smatdfm <- sparseVAR(scale(matrixdfm_data[(sim):(112+sim),]), p = 2,
                                 VARpen = "L1",
                                 selection = "cv")
  sim_forecast_smatdfm <- directforecast(sim_lasso_smatdfm)
  smatdfm_res <- data.frame((sim_forecast_smatdfm - matrixdfm_test[sim,])^2)
  colnames(smatdfm_res) <- colnames(smatdfm_forecast_residuals)
  smatdfm_forecast_residuals <- rbind(smatdfm_forecast_residuals,
                                      smatdfm_res)
}
smatdfm_forecast_residuals <- smatdfm_forecast_residuals[-nrow(smatdfm_forecast_residuals),]

par(mfrow = c(2, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))

smatdfm_res_plot1 <- plot(as.numeric(smatdfm_forecast_residuals[,1]), type = 'l',
                          main = "NL GDP", xlab = "Time", ylab = "Residual")
smatdfm_res_plot2 <- plot(as.numeric(smatdfm_forecast_residuals[,2]), type = 'l',
                          main = "NL Inflation", xlab = "Time", ylab = "Residual")
smatdfm_res_plot3 <- plot(as.numeric(smatdfm_forecast_residuals[,3]), type = 'l',
                          main = "NL EP Index", xlab = "Time", ylab = "Residual")
smatdfm_res_plot4 <- plot(as.numeric(smatdfm_forecast_residuals[,11]), type = 'l',
                          main = "Oil Prices", xlab = "Time", ylab = "Residual")
smatdfm_res_plot5 <- plot(as.numeric(smatdfm_forecast_residuals[,12]), type = 'l',
                          main = "Materials Prices", xlab = "Time", ylab = "Residual")
smatdfm_res_plot6 <- plot(as.numeric(smatdfm_forecast_residuals[,13]), type = 'l',
                          main = "Metal Prices", xlab = "Time", ylab = "Residual")

# Root Mean of each series
saveRDS(smatdfm_forecast_residuals, "smatdfm_forecast_residuals.rds")
