###### Preprocessing for Models
library(readxl)
library(tidyverse)
library(reshape2)
library(bootUR)
library(tensorFun)
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
mat_data <- unfold(perm_tensor_data, 1)

# Perform Unit Root tests and difference the series appropriately
# I lose the first two observations
stat_tensor_data <- order_integration(mat_data)

###### The flattened data and the tensor form are in the following variables
final_stat_data <- stat_tensor_data$diff_data[-1:-2,]
final_stat_tensor <- aperm(array(final_stat_data, dim = c(161, 5, 31)), c(1,3,2))


###### VAR preprocessing
normal_var_data <- as.matrix(cbind(stat_nl, final_stat_data))
saveRDS(normal_var_data, "VARdata.rds")

###### FAVAR preprocessing







