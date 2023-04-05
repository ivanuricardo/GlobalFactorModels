library(rTensor)
library(tensorFun)
library(HDeconometrics)
library(bigtime)
library(HDGCvar)
library(vars)
library(tensorTS)

set.seed(20230322)

# GFAVAR
gfavar_data <- readRDS("data/GFAVARdata.rds")
lags_upbound_BIC(gfavar_data)  # 3

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
  sim_gfavar <- VAR(gfavar_data[(sim):(112+sim),], p = 3, type = 'none')
  sim_prediction_gfavar <- predict(sim_gfavar, n.ahead = 1)
  sim_forecast_gfavar <- sapply(sim_prediction_gfavar$fcst, `[`, 1)
  gfavar_res <- data.frame((sim_forecast_gfavar - gfavar_test[sim,])^2)
  colnames(gfavar_res) <- colnames(gfavar_forecast_residuals)
  gfavar_forecast_residuals <- rbind(gfavar_forecast_residuals, gfavar_res)
}
gfavar_forecast_residuals <- gfavar_forecast_residuals[-nrow(gfavar_forecast_residuals),]

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

# Boxplots
num_gfavar_res <- apply(gfavar_forecast_residuals, 2, as.numeric)
gfavar_resMeans <- colMeans(num_gfavar_res) %>% sqrt()
gfavar_boxplot <- boxplot(gfavar_resMeans)
gfavar_boxplot_stats <- gfavar_boxplot[["stats"]]

# Sparse GFAVAR
# I use the same data as the gfavar from above, but apply it to the lasso VAR 
# implemented in bigtime.

first_sparse_gfavar <- sparseVAR(scale(gfavar_train), p = 3, VARpen = "L1",
                                 selection = "cv")
first_forecast_sgfavar <- directforecast(first_sparse_gfavar)
sgfavar_forecast_residuals <- data.frame(t(matrix((first_forecast_sgfavar -
                                                     gfavar_test[1,])^2)))

for (sim in 2:num_forecasts) {
  sim_lasso_sgfavar <- sparseVAR(scale(gfavar_data[(sim):(112+sim),]), p = 3,
                                 VARpen = "L1",
                                 selection = "cv")
  sim_forecast_sgfavar <- directforecast(sim_lasso_sgfavar)
  sgfavar_res <- data.frame((sim_forecast_sgfavar - gfavar_test[sim,])^2)
  colnames(sgfavar_res) <- colnames(sgfavar_forecast_residuals)
  sgfavar_forecast_residuals <- rbind(sgfavar_forecast_residuals,
                                      sgfavar_res)
}
sgfavar_forecast_residuals <- sgfavar_forecast_residuals[-nrow(sgfavar_forecast_residuals),]

par(mfrow = c(3, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))

sgfavar_res_plot1 <- plot(as.numeric(sgfavar_forecast_residuals[,1]), type = 'l',
                          main = "NL GDP", xlab = "Time", ylab = "Residual")
sgfavar_res_plot2 <- plot(as.numeric(sgfavar_forecast_residuals[,2]), type = 'l',
                          main = "NL Inflation", xlab = "Time", ylab = "Residual")
sgfavar_res_plot3 <- plot(as.numeric(sgfavar_forecast_residuals[,3]), type = 'l',
                          main = "NL EP Index", xlab = "Time", ylab = "Residual")
sgfavar_res_plot4 <- plot(as.numeric(sgfavar_forecast_residuals[,11]), type = 'l',
                          main = "Oil Prices", xlab = "Time", ylab = "Residual")
sgfavar_res_plot5 <- plot(as.numeric(sgfavar_forecast_residuals[,12]), type = 'l',
                          main = "Materials Prices", xlab = "Time", ylab = "Residual")
sgfavar_res_plot6 <- plot(as.numeric(sgfavar_forecast_residuals[,13]), type = 'l',
                          main = "Metal Prices", xlab = "Time", ylab = "Residual")

# Boxplots
num_sgfavar_res <- apply(sgfavar_forecast_residuals, 2, as.numeric)
sgfavar_resMeans <- colMeans(num_sgfavar_res) %>% sqrt()
sgfavar_boxplot <- boxplot(sgfavar_resMeans)
sgfavar_boxplot_stats <- sgfavar_boxplot[["stats"]]
