source("pre_analysis.R")
source("pre_processing.R")
source("regression.R")
source("random_sample.R")

# File reading ------------------------
data <- read.csv("data/train.csv",
                 header = TRUE,
                 sep = ",")

data.info <- read.csv("feature_approach.csv", 
                      header = TRUE, 
                      sep = ",")

# Data Segmentation --------------------

house.price <- data[ ,ncol(data)]

all.features <- data[ ,2:(ncol(data)-1)]

rm (data)

# Data Ploting --------------------------

FeaturePloting(all.features, house.price)

# Feature Processing --------------------

data.regression <- RegressionMatrix(all.features,
                                    house.price, 
                                    data.info)

# Least Squares Estimation ---------------

regression.result <- RegressionFunction(data.regression)
