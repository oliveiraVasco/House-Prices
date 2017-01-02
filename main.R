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

data.test <- read.csv("data/test.csv",
                      header = TRUE,
                      sep = ",")

# Data Segmentation --------------------

all.features <- data[ ,2:(ncol(data)-1)]

house.price <- log(data[ ,ncol(data)])

data.test <- data.test[ ,-1]

# Data Ploting --------------------------

#FeaturePloting(all.features, house.price)

# Cleaning Global environment -----------

#rm (data)
#rm (train.data)

# Feature Processing --------------------


data.regression <- InputConstruction(rbind(all.features, data.test),
                                     data.info)

test.regression <- data.regression[(nrow(all.features) + 1):nrow(data.regression), ]

data.regression <- data.regression[1:nrow(all.features), ]

data.regression <- AppendYToRegressionMatrix(data.regression, 
                                             house.price)

# Linear Dependece ----------------------

#data.regression <- LinearDependence(data.regression, FALSE)

# Multicolinearity ----------------------
#vifs <- StepWiseMulticolinearity(data.regression)

# Least Squares Estimation ---------------

#regression.result <- RegressionEstimationType(data.regression)

# Step Wise ------------------------------

model.information <- CrossValidationStepWiseForward(data.regression, 2, 0.7, 2, 5, NA)


