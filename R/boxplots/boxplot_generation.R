var_resMeans <- readRDS("R/boxplots/var_resMeans.rds")
favar_resMeans <- readRDS("R/boxplots/favar_resMeans.rds")
gfavar_resMeans <- readRDS("R/boxplots/gfavar_resMeans.rds")
sgfavar_resMeans <- readRDS("R/boxplots/sgfavar_resMeans.rds")
matdfm_resMeans <- readRDS("R/boxplots/matdfm_resMeans.rds")
smatdfm_resMeans <- readRDS("R/boxplots/smatdfm_resMeans.rds")

# VAR
var_boxplot <- boxplot(var_resMeans)
var_boxplot_stats <- var_boxplot[["stats"]]

# FAVAR
favar_boxplot <- boxplot(favar_resMeans)
favar_boxplot_stats <- favar_boxplot[["stats"]]

# GFAVAR
gfavar_boxplot <- boxplot(gfavar_resMeans)
gfavar_boxplot_stats <- gfavar_boxplot[["stats"]]

# Sparse GFAVAR
sgfavar_boxplot <- boxplot(sgfavar_resMeans)
sgfavar_boxplot_stats <- sgfavar_boxplot[["stats"]]

# MATDFM
matdfm_boxplot <- boxplot(matdfm_resMeans)
matdfm_boxplot_stats <- matdfm_boxplot[["stats"]]

# Sparse MATDFM
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




