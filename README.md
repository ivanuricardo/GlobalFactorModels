# Global Factor Models
Tensor Methods applied to the Global VAR data set

I apply a variety of methods to the global VAR data set. This includes lasso, factor models, and tensor decomposition specific for multidimensional data sets. Additionally, valid methods for inference are implemented to analyze variables specific to the Netherlands.

'R' contains the preprocessing steps, estimation, and inference for the data. 'data' contains .rds files that are already preprocessed and ready for estimation. In this sense, you can run `model_estimation.R` on its own without having to deal with the preprocessing.

In order to begin, make sure you download the GVAR data from [here](https://www.mohaddes.org/gvar). Then the pathname in `preprocessing.R` can be changed accordingly.
