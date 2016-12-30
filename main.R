source("pre_analysis.R")
source("pre_processing.R")
source("regression_tools.R")
source("multicolinearity.R")
source("step_wise.R")


# File reading ------------------------
data <- read.csv("data/train.csv",
                 header = TRUE,
                 sep = ",")

data.info <- read.csv("feature_approach.csv", 
                      header = TRUE, 
                      sep = ",")

# Data Segmentation --------------------

all.features <- data[ ,2:(ncol(data)-1)]

house.price <- log(data[ ,ncol(data)])

# Data Ploting --------------------------

#FeaturePloting(all.features, house.price)

# Cleaning Global environment -----------

#rm (data)
#rm (train.data)

# Feature Processing --------------------

data.regression <- RegressionMatrix(all.features,
                                    house.price, 
                                    data.info)

# Linear Dependece ----------------------

#data.regression <- LinearDependence(data.regression, FALSE)

# Multicolinearity ----------------------
#vifs <- StepWiseMulticolinearity(data.regression)

# Least Squares Estimation ---------------

#regression.result <- RegressionEstimationType(data.regression)

best.formula <- CrossValidationStepWise(data.regression, 2, 0.7)


