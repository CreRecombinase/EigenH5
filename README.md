
# EigenH5

<!-- badges: start -->
<!-- badges: end -->

The goal of EigenH5 is to make working with HDF5 files from R a little easier by:
1) Providing a set of helpful functions for reading and writing R objects to HDF5 files
2) automatically transposing matrices from HDF5's row-major order to R-friendly column-major order
3) Compressing data with the flexible and powerful [zstd](https://github.com/facebook/zstd) compression library 

## Installation

You can (maybe one day) install the released version of EigenH5 from [CRAN](https://CRAN.R-project.org) with:

``` r
#(not yet) install.packages("EigenH5")
```

## Example 


``` r
## basic example code
library(EigenH5)
temp_h5 <- fs::file_temp(ext="h5") #come up with a file path for our hdf5 file
delim2h5(readr::readr_example("mtcars.csv"),temp_h5,delim="/") #convert csv to HDF5 in chunks
mtcars_df <- read_df_h5(temp_h5,subcols=c("cyl","mpg"),subset=c(1,3,5,11)) read a subset of the rows and columns of the dataframe




```

