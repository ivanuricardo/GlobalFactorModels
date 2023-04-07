library(ggplot2)
library(ggpubr)
library(desla)
library(HDGCvar)
library(igraph)
set.seed(20230322)

####### Matrix DFM of Wang
matrixdfm_data <- readRDS("data/MATfactordata.rds")
series_names <- colnames(matrixdfm_data)[1:14]
factor_names <- c(15, 16, 20, 21, 25, 26)

# Do the factors load onto the individual Dutch series?
r_squared <- matrix(NA, nrow = 6, ncol = 14)
counter <- 0
for (i in factor_names) {
  counter <- counter + 1
  for (j in 1:14) {
    fac_model <- lm(matrixdfm_data[,i] ~ matrixdfm_data[,j])
    r_squared[counter,j] <- summary(fac_model)$r.squared
  }
}

barplot(r_squared[5,], names.arg = series_names)

df1 <- data.frame(
  series_names = series_names,
  r_squared = r_squared[1,]
)
df2 <- data.frame(
  series_names = series_names,
  r_squared = r_squared[2,]
)
df3 <- data.frame(
  series_names = series_names,
  r_squared = r_squared[3,]
)
df4 <- data.frame(
  series_names = series_names,
  r_squared = r_squared[4,]
)
df5 <- data.frame(
  series_names = series_names,
  r_squared = r_squared[5,]
)
df6 <- data.frame(
  series_names = series_names,
  r_squared = r_squared[6,]
)

# Create the barplots using ggplot
countryecon11 <- ggplot(df1, aes(x = series_names, y = r_squared)) +
    geom_bar(stat = "identity", fill = "blue") +
    labs(x = "Series Names", y = "R-Squared") +
    ggtitle("1st Country, 1st Econ")

countryecon21 <- ggplot(df2, aes(x = series_names, y = r_squared)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Series Names", y = "R-Squared") +
  ggtitle("2nd Country, 1st Econ")

countryecon12 <- ggplot(df3, aes(x = series_names, y = r_squared)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Series Names", y = "R-Squared") +
  ggtitle("1st Country, 2nd Econ")

countryecon22 <- ggplot(df4, aes(x = series_names, y = r_squared)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Series Names", y = "R-Squared") +
  ggtitle("2nd Country, 2nd Econ")

countryecon13 <- ggplot(df5, aes(x = series_names, y = r_squared)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Series Names", y = "R-Squared") +
  ggtitle("1st Country, 3rd Econ")

countryecon23 <- ggplot(df6, aes(x = series_names, y = r_squared)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Series Names", y = "R-Squared") +
  ggtitle("2nd Country, 3rd Econ")

figure1 <- ggarrange(countryecon11, countryecon12, countryecon13,
                    countryecon21, countryecon22, countryecon23,
                    ncol = 3, nrow = 2)
figure


####### Local Projection Inference

# First, reorder the data such that fast moving variables are on the right
# and slow moving variables are on the left.
new_order <- c(15:29, 1, 6, 2, 7, 11, 12, 13, 14, 10, 4, 3, 8, 5, 9)
reordered_matdfm <- matrixdfm_data[,new_order]


### IR to GDP

shock_idx <- 28
response_idx <- 16

shock_var <- reordered_matdfm[,shock_idx]
response_var <- reordered_matdfm[,response_idx]

updated_matdfm <- reordered_matdfm[, -c(shock_idx, response_idx)]
slow_vars <- updated_matdfm[,1:13]
fast_vars <- updated_matdfm[,14:27]

# Local Projections
LP_ry <- HDLP(x = shock_var, y = response_var, r = slow_vars, q = fast_vars,
              hmax = 20, lags = 10)
graph_yr <- plot.hdlp(LP_ry, response = "GDP", impulse = "IR")

### IR to Factor 11

shock_idx <- 28
response_idx <- 11

shock_var <- reordered_matdfm[,shock_idx]
response_var <- reordered_matdfm[,response_idx]

updated_matdfm <- reordered_matdfm[, -c(shock_idx, response_idx)]
slow_vars <- updated_matdfm[,1:13]
fast_vars <- updated_matdfm[,14:27]

# Local Projections
LP_r11 <- HDLP(x = shock_var, y = response_var, r = slow_vars, q = fast_vars,
              hmax = 20, lags = 10)
graph_yf11 <- plot.hdlp(LP_r11, response = "FAC11", impulse = "IR")

### IR to Dps

shock_idx <- 28
response_idx <- 19

shock_var <- reordered_matdfm[,shock_idx]
response_var <- reordered_matdfm[,response_idx]

updated_matdfm <- reordered_matdfm[, -c(shock_idx, response_idx)]
slow_vars <- updated_matdfm[,1:13]
fast_vars <- updated_matdfm[,14:27]

# Local Projections
LP_dps <- HDLP(x = shock_var, y = response_var, r = slow_vars, q = fast_vars,
              hmax = 20, lags = 10)
graph_ydps <- plot.hdlp(LP_dps, response = "Dps", impulse = "IR")

### IR to PMAT

shock_idx <- 28
response_idx <- 21

shock_var <- reordered_matdfm[,shock_idx]
response_var <- reordered_matdfm[,response_idx]

updated_matdfm <- reordered_matdfm[, -c(shock_idx, response_idx)]
slow_vars <- updated_matdfm[,1:13]
fast_vars <- updated_matdfm[,14:27]

# Local Projections
LP_rpmat <- HDLP(x = shock_var, y = response_var, r = slow_vars, q = fast_vars,
              hmax = 20, lags = 10)
graph_ypmat <- plot.hdlp(LP_rpmat, response = "PMAT", impulse = "IR")

### IR to PMETAL

shock_idx <- 28
response_idx <- 22

shock_var <- reordered_matdfm[,shock_idx]
response_var <- reordered_matdfm[,response_idx]

updated_matdfm <- reordered_matdfm[, -c(shock_idx, response_idx)]
slow_vars <- updated_matdfm[,1:13]
fast_vars <- updated_matdfm[,14:27]

# Local Projections
LP_rpmetal <- HDLP(x = shock_var, y = response_var, r = slow_vars, q = fast_vars,
              hmax = 20, lags = 10)
graph_ypmetal <- plot.hdlp(LP_rpmetal, response = "PMET", impulse = "IR")

### EX to GDP

shock_idx <- 25
response_idx <- 16

shock_var <- reordered_matdfm[,shock_idx]
response_var <- reordered_matdfm[,response_idx]

updated_matdfm <- reordered_matdfm[, -c(shock_idx, response_idx)]
slow_vars <- updated_matdfm[,1:13]
fast_vars <- updated_matdfm[,14:27]

# Local Projections
LP_exy <- HDLP(x = shock_var, y = response_var, r = slow_vars, q = fast_vars,
              hmax = 20, lags = 10)
graph_exy <- plot.hdlp(LP_exy, response = "GDP", impulse = "EXR")

figure2 <- ggarrange(graph_yr, graph_yf11, graph_ydps,
                     graph_ypmat, graph_ypmetal, graph_exy,
                     ncol = 3, nrow = 2)
figure2

####### Granger Causality
normal_var_data <- readRDS("data/VARdata.rds")

# Changing the bound does not do anything..?
hdgc_netherlands <- HDGC_VAR_multiple_I0(normal_var_data, p = 1,
                                         GCpairs = list(list("GCto"="y","GCfrom"="r"),
                                                        list("GCto"="Dp","GCfrom"="r" )))

hdgc_all <- HDGC_VAR_all_I0(normal_var_data, p = 1)

hdgc_all_factors <- HDGC_VAR_all_I0(matrixdfm_data, p = 2)

Plot_GC_all(hdgc_all_factors, Stat_type="FS_cor",alpha=0.01,directed=T,
            layout=layout.circle, main="Network",edge.arrow.size=.2,vertex.size=5,
            vertex.color=c("lightblue"), vertex.frame.color="blue",vertex.label.size=2,
            vertex.label.color="black",vertex.label.cex=0.6, vertex.label.dist=1,
            edge.curved=0,cluster=list(T,5,"black",0.8,1,0)) 

Plot_GC_all(hdgc_all, Stat_type="FS_cor",alpha=0.01,multip_corr=list(F),directed=T,
            layout=layout.circle, main="Network",edge.arrow.size=.2,vertex.size=5,
            vertex.color=c("lightblue"), vertex.frame.color="blue",vertex.label.size=2,
            vertex.label.color="black",vertex.label.cex=0.6, vertex.label.dist=1,
            edge.curved=0,cluster=list(T,5,"black",0.8,1,0)) 
