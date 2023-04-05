library(ggplot2)
library(gridExtra)
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
    ggtitle("First Country Factor, First Econ Factor")

countryecon21 <- ggplot(df2, aes(x = series_names, y = r_squared)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Series Names", y = "R-Squared") +
  ggtitle("Second Country Factor, First Econ Factor")

countryecon12 <- ggplot(df3, aes(x = series_names, y = r_squared)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Series Names", y = "R-Squared") +
  ggtitle("First Country Factor, Second Econ Factor")

countryecon22 <- ggplot(df4, aes(x = series_names, y = r_squared)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Series Names", y = "R-Squared") +
  ggtitle("Second Country Factor, Second Econ Factor")

countryecon13 <- ggplot(df5, aes(x = series_names, y = r_squared)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Series Names", y = "R-Squared") +
  ggtitle("First Country Factor, Third Econ Factor")

countryecon32 <- ggplot(df6, aes(x = series_names, y = r_squared)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Series Names", y = "R-Squared") +
  ggtitle("Second Country Factor, Third Econ Factor")


