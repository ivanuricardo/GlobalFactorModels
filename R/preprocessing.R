###### Preprocessing for Models
library(readxl)
library(tidyverse)
library(reshape2)
library(bootUR)
library(plotly)
library(tensorFun)
library(nowcasting)
library(tensor)
library(tensorTS)
set.seed(20230322)

###### Constructing Data
pathname <- "/Users/Ivan/Desktop/gvar_database/country_data.xls"
sheet_names <- excel_sheets(pathname)
data <- read_excel(pathname, sheet = 1)
gvar_data <- data %>% 
  mutate(country = sheet_names[1])

for (sheet in (2:(length(sheet_names)-1))) {
  appending_data <- read_excel(pathname, sheet = sheet)
  appending_data <- appending_data%>%mutate(country = sheet_names[sheet])
  new_data <- plyr::rbind.fill(gvar_data, appending_data)
  gvar_data <- new_data
}

# remove unnecessary columns and columns with NA. Additionally remove NL from ROW
sa_gvar1 <- subset(gvar_data, country != "SAUDI ARABIA")
sa_gvar2 <- subset(sa_gvar1, country != "NETHERLANDS")
sa_gvar2[, c("date", "country", "eps", "poil", "pmetal", "pmat")] <- list(NULL)
updated_gvar <- sa_gvar2[ , colSums(is.na(sa_gvar2))==0]

# Same for NL variables
nl_variables <- subset(gvar_data, country == "NETHERLANDS")
nl_variables[, c("date", "country", "eps")] <- list(NULL)

# Transform NL to stationarity and remove first two observations
stat_nl_var <- order_integration(nl_variables)
stat_nl <- stat_nl_var$diff_data[-1:-2,]

# create 3-dimensional tensor of ROW
levels_tensor_data <- updated_gvar %>% 
  as.matrix() %>% 
  array(dim = c(163, 31, 5)) 

# Flatten to convert all series to stationary
# This is a neat trick to convert the long matrix to a wide matrix with the 
# target variables
perm_tensor_data <- aperm(levels_tensor_data, c(1,3,2))
mat_data <- tensorFun::unfold(perm_tensor_data, 1)

# Perform Unit Root tests and difference the series appropriately
# I lose the first two observations
stat_tensor_data <- order_integration(mat_data)

###### The flattened data and the tensor form are in the following variables
final_stat_data <- stat_tensor_data$diff_data[-1:-2,]
final_stat_tensor <- aperm(array(final_stat_data, dim = c(161, 5, 31)), c(1,3,2))

###### Full Data Tensor
sa_gvar1[, c("date", "country", "eps", "poil", "pmetal", "pmat")] <- list(NULL)
full_data <- sa_gvar1[ , colSums(is.na(sa_gvar1))==0]

# create 3-dimensional tensor of full data
levels_full <- full_data %>% 
  as.matrix() %>% 
  array(dim = c(163, 32, 5))
perm_tensor_full <- aperm(levels_full, c(1,3,2))
mat_full <- tensorFun::unfold(perm_tensor_full, 1)

# Stationary variables
stat_full_data <- order_integration(mat_full)
stat_full <- stat_full_data$diff_data[-1:-2,]
full_stat_tensor <- aperm(array(stat_full, dim = c(161, 5, 32)), c(1,3,2))
saveRDS(full_stat_tensor, "fulldata.rds")

###### VAR preprocessing
normal_var_data <- as.matrix(cbind(stat_nl, final_stat_data))
saveRDS(normal_var_data, "VARdata.rds")

###### FAVAR preprocessing

# How many factors?
ICfactors(final_stat_data) # 6 factors

# Extract PCs from the data
svd_world_data <- svd(final_stat_data)
world_factors <- svd_world_data$u[,1:6] %*% diag(svd_world_data$d[1:6])

favar_data <- as.matrix(cbind(stat_nl, world_factors))
saveRDS(favar_data, "FAVARdata.rds")

###### GFAVAR preprocessing

# Need to modify hosvd2 function to only accept 2 inputs
hosvd2 <- function(country_set, econ_set) {
  X <- final_stat_tensor
  num_modes <- length(dim(X))
  U_list <- vector("list", num_modes)
  
  num_ranks <- c(163, country_set, econ_set)
  
  # Factor Matrices
  for (m in 1:num_modes) {
    temp_mat <- tensorFun::unfold(X, m)
    U_list[[m]] <- svd(temp_mat, nu = num_ranks[m])$u
  }
  
  # Core Tensor
  G <- X
  for (n in 1:num_modes) {
    G <- tensor(G, t(U_list[[n]]), alongA = 1, alongB = 2)
  }
  
  # Estimate
  X_hat <- G
  for (o in 1:num_modes) {
    X_hat <- tensor(X_hat, U_list[[o]], alongA = 1, alongB = 2)
  }
  return(sqrt(tensor_ip(X - X_hat)))
}

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
gfavar_data <- cbind(stat_nl, world_factors)
saveRDS(gfavar_data, "GFAVARdata.rds")

###### Matrix Factor Model Preprocessing

# Perform the matrix factor model on the same data as the HOSVD
matrix_factor_model <- tenFM.est(final_stat_tensor, r = c(5,3))

# Extract the factors and flatten them to world factors
matrix_factors <- matrix_factor_model$Ft
world_factors2 <- tensorFun::unfold(matrix_factors, 1)

# Append to the stationary Netherlands data
mat_factor_data <- cbind(stat_nl, world_factors2)
saveRDS(mat_factor_data, "MATfactordata.rds")

