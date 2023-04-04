# Global Factor Models
Tensor Methods applied to the Global VAR data set

I apply a variety of methods to the global VAR data set. This includes lasso, factor models, and tensor decomposition specific for multidimensional data sets. Additionally, valid methods for inference are implemented to analyze variables specific to the Netherlands.

'R' contains the preprocessing steps, estimation, and inference for the data. 'data' contains .rds files that are already preprocessed and ready for estimation. In this sense, you can run `model_estimation.R` on its own without having to deal with the preprocessing.

The idea behind this project is to provide a **completely reproducible project**. You can run every file individually, without having to change directory or hidden variables.
