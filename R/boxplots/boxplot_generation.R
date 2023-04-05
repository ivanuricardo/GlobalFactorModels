library(dplyr)
library(xtable)

var_forecast_residuals <- readRDS("R/boxplots/var_forecast_residuals.rds")
favar_forecast_residuals <- readRDS("R/boxplots/favar_forecast_residuals.rds")
gfavar_forecast_residuals <- readRDS("R/boxplots/gfavar_forecast_residuals.rds")
sgfavar_forecast_residuals <- readRDS("R/boxplots/sgfavar_forecast_residuals.rds")
matdfm_forecast_residuals <- readRDS("R/boxplots/matdfm_forecast_residuals.rds")
smatdfm_forecast_residuals <- readRDS("R/boxplots/smatdfm_forecast_residuals.rds")

# VAR
var_resMeans <- colMeans(var_forecast_residuals) %>% sqrt()
var_boxplot <- boxplot(var_resMeans)
var_boxplot_stats <- var_boxplot[["stats"]]

# FAVAR
favar_resMeans <- colMeans(favar_forecast_residuals) %>% sqrt()
favar_boxplot <- boxplot(favar_resMeans)
favar_boxplot_stats <- favar_boxplot[["stats"]]

# GFAVAR
num_gfavar_res <- apply(gfavar_forecast_residuals, 2, as.numeric)
gfavar_resMeans <- colMeans(num_gfavar_res) %>% sqrt()
gfavar_boxplot <- boxplot(gfavar_resMeans)
gfavar_boxplot_stats <- gfavar_boxplot[["stats"]]

# Sparse GFAVAR
num_sgfavar_res <- apply(sgfavar_forecast_residuals, 2, as.numeric)
sgfavar_resMeans <- colMeans(num_sgfavar_res) %>% sqrt()
sgfavar_boxplot <- boxplot(sgfavar_resMeans)
sgfavar_boxplot_stats <- sgfavar_boxplot[["stats"]]

# MATDFM
num_matdfm_res <- apply(matdfm_forecast_residuals, 2, as.numeric)
matdfm_resMeans <- colMeans(num_matdfm_res) %>% sqrt()
matdfm_boxplot <- boxplot(matdfm_resMeans)
matdfm_boxplot_stats <- matdfm_boxplot[["stats"]]

# Sparse MATDFM
num_smatdfm_res <- apply(smatdfm_forecast_residuals, 2, as.numeric)
smatdfm_resMeans <- colMeans(num_smatdfm_res) %>% sqrt()
smatdfm_boxplot <- boxplot(smatdfm_resMeans)
smatdfm_boxplot_stats <- smatdfm_boxplot[["stats"]]

####### Plots in conjunction
par(mfrow = c(2, 3), mar = c(1, 4, 1, 1), oma = c(1, 0, 2, 0))

boxplot1 <- boxplot(var_resMeans, main = "Sparse-VAR", ylab = "Residual")
boxplot2 <- boxplot(gfavar_resMeans, main = "Global FAVAR")
boxplot3 <- boxplot(matdfm_resMeans, main = "MATDFM")
boxplot4 <- boxplot(favar_resMeans, main = "FAVAR", ylab = "Residual")
boxplot5 <- boxplot(sgfavar_resMeans, main = "Sparse Global FAVAR")
boxplot5 <- boxplot(smatdfm_resMeans, main = "Sparse MATDFM")

# Table with forecast errors for GDP, Inflation, EP index, Oil Prices, 
# Materials Prices, and Metal prices

indicator_cols <- c(1,2,3,11,12,13)

var_col <- var_resMeans[indicator_cols]
favar_col <- favar_resMeans[indicator_cols]
gfavar_col <- gfavar_resMeans[indicator_cols]
sgfavar_col <- sgfavar_resMeans[indicator_cols]
matdfm_col <- matdfm_resMeans[indicator_cols]
smatdfm_col <- smatdfm_resMeans[indicator_cols]

rmsfe_matrix <- rbind(var_col, favar_col, gfavar_col, sgfavar_col,
                      matdfm_col, smatdfm_col)
rmsfe_matrix <- rmsfe_matrix/rmsfe_matrix[1,1]
latex_matrix <- xtable(rmsfe_matrix, digits = 4)
print(latex_matrix, type = "latex")
