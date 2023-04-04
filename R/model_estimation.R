library(rTensor)
library(tensor)
library(tensorFun)
library(plotly)
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


# All possible combinations for the scree plot
country_set <- 1:31
econ_set <- 1:5

###### 3D Plot for the factors
vec_hosvd2 <- Vectorize(hosvd2)
var_forecast_residuals <- outer(country_set, econ_set, vec_hosvd2)

plot_ly(x = country_set, y = econ_set, z = var_forecast_residuals, 
        type = "surface", mode = "markers",
        text = sprintf("Value: %0.2f", var_forecast_residuals)) %>%
  layout(scene = list(xaxis = list(title = "Country Set"),
                      yaxis = list(title = "Econ Set"),
                      zaxis = list(title = "F-Norm")),
         title = "Scree Plot")

# Plot the scree plots holding all elements of econ factors constant
par(mfrow = c(2, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
plot_econ1 <- plot(var_forecast_residuals[,1], type = 'l',
                   main = "Scree Plot for First Econ Factor",
                   xlab = "Country factors", ylab = "Fit (fnorm)")
plot_econ2 <- plot(var_forecast_residuals[,2], type = 'l',
                   main = "Scree Plot for Second Econ Factor",
                   xlab = "Country factors", ylab = "Fit (fnorm)")
plot_econ3 <- plot(var_forecast_residuals[,3], type = 'l',
                   main = "Scree Plot for Third Econ Factor",
                   xlab = "Country factors", ylab = "Fit (fnorm)")
plot_econ4 <- plot(var_forecast_residuals[,4], type = 'l',
                   main = "Scree Plot for Fourth Econ Factor",
                   xlab = "Country factors", ylab = "Fit (fnorm)")
plot_econ5 <- plot(var_forecast_residuals[,5], type = 'l',
                   main = "Scree Plot for Fifth Econ Factor",
                   xlab = "Country factors", ylab = "Fit (fnorm)")

# Then we choose 5 country factors and 3 economic factors and fnorm is 1.90. Perform HOSVD
country_factors <- 5

econ_factors <- 3

hosvd_world <- tnsr_hosvd(final_stat_tensor,
                          num_ranks = c(161, country_factors, econ_factors))

# Create representative factor matrix
world_g <- hosvd_world$G
world_U <- hosvd_world$U[[1]]
world_factors <- tensor(world_g, world_U, alongA = 1, alongB = 2)
world_factors <- tensorFun::unfold(world_factors, 3)

num_factors <- country_factors*econ_factors

# Create data containing the factors and the Netherlands variables
full_data_matrix <- cbind(stat_nl, world_factors)
factor_names <- paste("F", 1:num_factors, sep = "")
colnames(full_data_matrix) <- c("NL_y", "NL_Dp", "NL_eq", "NL_ep", "NL_r", "NL_ys",
                                "NL_dps","NL_eqs", "NL_rs", "NL_lrs", "NL_poil",
                                "NL_pmat", "NL_pmetal","NL_lr", factor_names)
full_data_matrix <- as.data.frame(full_data_matrix)

X_final <- model.matrix(NL_y ~ ., data = full_data_matrix)[,-1]
NL_y <- full_data_matrix$NL_y

# GFAVAR

lags_upbound_BIC(full_data_matrix)

gfavar_train <- full_data_matrix[1:113,]
gfavar_test <- full_data_matrix[114:161,]
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






